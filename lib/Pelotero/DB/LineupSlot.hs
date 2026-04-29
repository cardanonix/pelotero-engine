{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pelotero.DB.LineupSlot
  ( LineupSlotRow(..)
  , getSlotsForTeamT
  , addSlotT
  , removeSlotT
  , clearTeamLineupT
  , replaceTeamLineupT
  , getSlotsForTeam
  , addSlot
  , removeSlot
  , clearTeamLineup
  , replaceTeamLineup
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           GHC.Generics               (Generic)

import qualified Hasql.Transaction          as Tx

import           Rel8                       ( Column
                                            , Name
                                            , Rel8able
                                            , Result
                                            , TableSchema(..)
                                            , (==.)
                                            , (&&.)
                                            )
import qualified Rel8                       as R

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Rel8Instances ()
import Pelotero.Domain.Id    (DbLeagueTeamId(..), DbPlayerId(..))

-- ============================================================================
-- lineup_slot
--
-- Same shape as roster_slot: surrogate id and created_at exist on disk but
-- aren't part of the public API. The (league_team_id, player_id) unique
-- constraint is the logical key.
-- ============================================================================

data LineupSlotE f = LineupSlotE
  { _lsId           :: Column f Int64
  , _lsLeagueTeamId :: Column f DbLeagueTeamId
  , _lsSlot         :: Column f Text
  , _lsPlayerId     :: Column f DbPlayerId
  , _lsCreatedAt    :: Column f UTCTime
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (LineupSlotE f)
deriving stock instance f ~ Result => Eq   (LineupSlotE f)

lineupSlotSchema :: TableSchema (LineupSlotE Name)
lineupSlotSchema = TableSchema
  { name    = "lineup_slot"
  , columns = LineupSlotE
      { _lsId           = "id"
      , _lsLeagueTeamId = "league_team_id"
      , _lsSlot         = "slot"
      , _lsPlayerId     = "player_id"
      , _lsCreatedAt    = "created_at"
      }
  }

-- ============================================================================
-- Public row type (API compatibility with old hasql module)
-- ============================================================================

data LineupSlotRow = LineupSlotRow
  { lsLeagueTeamId :: !DbLeagueTeamId
  , lsSlot         :: !Text
  , lsPlayerId     :: !DbPlayerId
  }
  deriving stock (Show, Eq)

fromResult :: LineupSlotE Result -> LineupSlotRow
fromResult LineupSlotE{..} = LineupSlotRow
  { lsLeagueTeamId = _lsLeagueTeamId
  , lsSlot         = _lsSlot
  , lsPlayerId     = _lsPlayerId
  }

-- ============================================================================
-- Transaction-flavored CRUD
--
-- addSlotT does ON CONFLICT (league_team_id, player_id) DO UPDATE SET slot,
-- preserving the old hasql module's "moving a player to a new lineup slot
-- doesn't fail" semantics.
-- ============================================================================

addSlotT :: LineupSlotRow -> Tx.Transaction ()
addSlotT row = Tx.statement () $ R.run_ $ R.insert R.Insert
  { R.into       = lineupSlotSchema
  , R.rows       = R.values
      [ LineupSlotE
          { _lsId           = R.unsafeDefault
          , _lsLeagueTeamId = R.lit (lsLeagueTeamId row)
          , _lsSlot         = R.lit (lsSlot row)
          , _lsPlayerId     = R.lit (lsPlayerId row)
          , _lsCreatedAt    = R.unsafeDefault
          }
      ]
  , R.onConflict = R.DoUpdate R.Upsert
      { R.index       = \r -> (_lsLeagueTeamId r, _lsPlayerId r)
      , R.predicate   = Nothing
      , R.set         = \new old -> LineupSlotE
          { _lsId           = _lsId old
          , _lsLeagueTeamId = _lsLeagueTeamId old
          , _lsSlot         = _lsSlot new
          , _lsPlayerId     = _lsPlayerId old
          , _lsCreatedAt    = _lsCreatedAt old
          }
      , R.updateWhere = \_ _ -> R.lit True
      }
  , R.returning  = R.NoReturning
  }

getSlotsForTeamT :: DbLeagueTeamId -> Tx.Transaction [LineupSlotRow]
getSlotsForTeamT tid = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy ((_lsSlot >$< R.asc) <> (_lsPlayerId >$< R.asc)) $ do
      r <- R.each lineupSlotSchema
      R.where_ (_lsLeagueTeamId r ==. R.lit tid)
      pure r
  pure (map fromResult rows)

removeSlotT :: DbLeagueTeamId -> DbPlayerId -> Tx.Transaction ()
removeSlotT tid pid = Tx.statement () $ R.run_ $ R.delete R.Delete
  { R.from        = lineupSlotSchema
  , R.using       = pure ()
  , R.deleteWhere = \_ r ->
      _lsLeagueTeamId r ==. R.lit tid &&. _lsPlayerId r ==. R.lit pid
  , R.returning   = R.NoReturning
  }

clearTeamLineupT :: DbLeagueTeamId -> Tx.Transaction ()
clearTeamLineupT tid = Tx.statement () $ R.run_ $ R.delete R.Delete
  { R.from        = lineupSlotSchema
  , R.using       = pure ()
  , R.deleteWhere = \_ r -> _lsLeagueTeamId r ==. R.lit tid
  , R.returning   = R.NoReturning
  }

replaceTeamLineupT :: DbLeagueTeamId -> [LineupSlotRow] -> Tx.Transaction ()
replaceTeamLineupT tid rows = do
  clearTeamLineupT tid
  mapM_ addSlotT rows

-- ============================================================================
-- Pool-flavored CRUD
-- ============================================================================

getSlotsForTeam :: Pool -> DbLeagueTeamId -> IO (Either DBError [LineupSlotRow])
getSlotsForTeam pool tid = runTransaction pool (getSlotsForTeamT tid)

addSlot :: Pool -> LineupSlotRow -> IO (Either DBError ())
addSlot pool row = runTransaction pool (addSlotT row)

removeSlot :: Pool -> DbLeagueTeamId -> DbPlayerId -> IO (Either DBError ())
removeSlot pool tid pid = runTransaction pool (removeSlotT tid pid)

clearTeamLineup :: Pool -> DbLeagueTeamId -> IO (Either DBError ())
clearTeamLineup pool tid = runTransaction pool (clearTeamLineupT tid)

replaceTeamLineup :: Pool -> DbLeagueTeamId -> [LineupSlotRow] -> IO (Either DBError ())
replaceTeamLineup pool tid rows = runTransaction pool (replaceTeamLineupT tid rows)