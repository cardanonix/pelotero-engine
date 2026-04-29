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

module Pelotero.DB.RosterSlot
  ( RosterSlotRow(..)
  , getSlotsForTeamT
  , addSlotT
  , removeSlotT
  , removePlayerFromTeamT
  , clearTeamRosterT
  , replaceTeamRosterT
  , countBySlotT
  , getSlotsForTeam
  , addSlot
  , removeSlot
  , removePlayerFromTeam
  , clearTeamRoster
  , replaceTeamRoster
  , countBySlot
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
-- roster_slot
--
-- We model the surrogate id and created_at columns even though the public
-- API doesn't expose them; rel8's TableSchema needs to know about every
-- column the table actually has so we can SELECT *. The id column is filled
-- in by BIGSERIAL on insert, the created_at column by NOW().
-- ============================================================================

data RosterSlotE f = RosterSlotE
  { _rsId           :: Column f Int64
  , _rsLeagueTeamId :: Column f DbLeagueTeamId
  , _rsSlot         :: Column f Text
  , _rsPlayerId     :: Column f DbPlayerId
  , _rsCreatedAt    :: Column f UTCTime
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (RosterSlotE f)
deriving stock instance f ~ Result => Eq   (RosterSlotE f)

rosterSlotSchema :: TableSchema (RosterSlotE Name)
rosterSlotSchema = TableSchema
  { name    = "roster_slot"
  , columns = RosterSlotE
      { _rsId           = "id"
      , _rsLeagueTeamId = "league_team_id"
      , _rsSlot         = "slot"
      , _rsPlayerId     = "player_id"
      , _rsCreatedAt    = "created_at"
      }
  }

-- ============================================================================
-- Public row type (API compatibility with old hasql module)
-- ============================================================================

data RosterSlotRow = RosterSlotRow
  { rsLeagueTeamId :: !DbLeagueTeamId
  , rsSlot         :: !Text
  , rsPlayerId     :: !DbPlayerId
  }
  deriving stock (Show, Eq)

fromResult :: RosterSlotE Result -> RosterSlotRow
fromResult RosterSlotE{..} = RosterSlotRow
  { rsLeagueTeamId = _rsLeagueTeamId
  , rsSlot         = _rsSlot
  , rsPlayerId     = _rsPlayerId
  }

-- ============================================================================
-- Transaction-flavored CRUD
--
-- Note: the old hasql module's insertStmt did
--   ON CONFLICT (league_team_id, player_id) DO UPDATE SET slot = EXCLUDED.slot
-- so that adding a player who's already on the roster moves them to the new
-- slot rather than failing. rel8 1.7's Upsert encodes this directly.
-- ============================================================================

addSlotT :: RosterSlotRow -> Tx.Transaction ()
addSlotT row = Tx.statement () $ R.run_ $ R.insert R.Insert
  { R.into       = rosterSlotSchema
  , R.rows       = R.values
      [ RosterSlotE
          { _rsId           = R.unsafeDefault
          , _rsLeagueTeamId = R.lit (rsLeagueTeamId row)
          , _rsSlot         = R.lit (rsSlot row)
          , _rsPlayerId     = R.lit (rsPlayerId row)
          , _rsCreatedAt    = R.unsafeDefault
          }
      ]
  , R.onConflict = R.DoUpdate R.Upsert
      { R.index       = \r -> (_rsLeagueTeamId r, _rsPlayerId r)
      , R.predicate   = Nothing
      , R.set         = \new old -> RosterSlotE
          { _rsId           = _rsId old
          , _rsLeagueTeamId = _rsLeagueTeamId old
          , _rsSlot         = _rsSlot new
          , _rsPlayerId     = _rsPlayerId old
          , _rsCreatedAt    = _rsCreatedAt old
          }
      , R.updateWhere = \_ _ -> R.lit True
      }
  , R.returning  = R.NoReturning
  }

getSlotsForTeamT :: DbLeagueTeamId -> Tx.Transaction [RosterSlotRow]
getSlotsForTeamT tid = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy ((_rsSlot >$< R.asc) <> (_rsPlayerId >$< R.asc)) $ do
      r <- R.each rosterSlotSchema
      R.where_ (_rsLeagueTeamId r ==. R.lit tid)
      pure r
  pure (map fromResult rows)

removeSlotT :: DbLeagueTeamId -> DbPlayerId -> Tx.Transaction ()
removeSlotT tid pid = Tx.statement () $ R.run_ $ R.delete R.Delete
  { R.from        = rosterSlotSchema
  , R.using       = pure ()
  , R.deleteWhere = \_ r ->
      _rsLeagueTeamId r ==. R.lit tid &&. _rsPlayerId r ==. R.lit pid
  , R.returning   = R.NoReturning
  }

removePlayerFromTeamT :: DbLeagueTeamId -> DbPlayerId -> Tx.Transaction ()
removePlayerFromTeamT = removeSlotT

clearTeamRosterT :: DbLeagueTeamId -> Tx.Transaction ()
clearTeamRosterT tid = Tx.statement () $ R.run_ $ R.delete R.Delete
  { R.from        = rosterSlotSchema
  , R.using       = pure ()
  , R.deleteWhere = \_ r -> _rsLeagueTeamId r ==. R.lit tid
  , R.returning   = R.NoReturning
  }

replaceTeamRosterT :: DbLeagueTeamId -> [RosterSlotRow] -> Tx.Transaction ()
replaceTeamRosterT tid rows = do
  clearTeamRosterT tid
  mapM_ addSlotT rows

countBySlotT :: DbLeagueTeamId -> Text -> Tx.Transaction Int64
countBySlotT tid slot = do
  ns <- Tx.statement () $ R.run $ R.select $ R.aggregate1 R.countStar $ do
    r <- R.each rosterSlotSchema
    R.where_ (_rsLeagueTeamId r ==. R.lit tid &&. _rsSlot r ==. R.lit slot)
    pure r
  pure $ case ns of
    (n : _) -> n
    []      -> 0

-- ============================================================================
-- Pool-flavored CRUD
-- ============================================================================

getSlotsForTeam :: Pool -> DbLeagueTeamId -> IO (Either DBError [RosterSlotRow])
getSlotsForTeam pool tid = runTransaction pool (getSlotsForTeamT tid)

addSlot :: Pool -> RosterSlotRow -> IO (Either DBError ())
addSlot pool row = runTransaction pool (addSlotT row)

removeSlot :: Pool -> DbLeagueTeamId -> DbPlayerId -> IO (Either DBError ())
removeSlot pool tid pid = runTransaction pool (removeSlotT tid pid)

removePlayerFromTeam :: Pool -> DbLeagueTeamId -> DbPlayerId -> IO (Either DBError ())
removePlayerFromTeam = removeSlot

clearTeamRoster :: Pool -> DbLeagueTeamId -> IO (Either DBError ())
clearTeamRoster pool tid = runTransaction pool (clearTeamRosterT tid)

replaceTeamRoster :: Pool -> DbLeagueTeamId -> [RosterSlotRow] -> IO (Either DBError ())
replaceTeamRoster pool tid rows = runTransaction pool (replaceTeamRosterT tid rows)

countBySlot :: Pool -> DbLeagueTeamId -> Text -> IO (Either DBError Int64)
countBySlot pool tid slot = runTransaction pool (countBySlotT tid slot)