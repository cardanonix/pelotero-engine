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

module Pelotero.DB.PlayerRanking
  ( PlayerRankingRow(..)
  , getRankingsForTeamT
  , replaceRankingsT
  , clearRankingsT
  , getRankingCountT
  , getRankingsForTeam
  , replaceRankings
  , clearRankings
  , getRankingCount
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Int                   (Int32, Int64)
import           Data.Time                  (UTCTime)
import           GHC.Generics               (Generic)

import qualified Hasql.Transaction          as Tx

import           Rel8                       ( Column
                                            , Name
                                            , Rel8able
                                            , Result
                                            , TableSchema(..)
                                            , (==.)
                                            )
import qualified Rel8                       as R

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Rel8Instances ()
import Pelotero.Domain.Id    (DbLeagueTeamId(..), DbPlayerId(..))

-- ============================================================================
-- player_ranking
--
-- No surrogate id; (league_team_id, player_id) is the natural primary key.
-- updated_at is server-managed.
-- ============================================================================

data PlayerRankingE f = PlayerRankingE
  { _prLeagueTeamId :: Column f DbLeagueTeamId
  , _prPlayerId     :: Column f DbPlayerId
  , _prRankSlot     :: Column f Int32
  , _prUpdatedAt    :: Column f UTCTime
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (PlayerRankingE f)
deriving stock instance f ~ Result => Eq   (PlayerRankingE f)

playerRankingSchema :: TableSchema (PlayerRankingE Name)
playerRankingSchema = TableSchema
  { name    = "player_ranking"
  , columns = PlayerRankingE
      { _prLeagueTeamId = "league_team_id"
      , _prPlayerId     = "player_id"
      , _prRankSlot     = "rank_slot"
      , _prUpdatedAt    = "updated_at"
      }
  }

-- ============================================================================
-- Public row type (API compatibility with old hasql module)
-- ============================================================================

data PlayerRankingRow = PlayerRankingRow
  { prLeagueTeamId :: !DbLeagueTeamId
  , prPlayerId     :: !DbPlayerId
  , prRankSlot     :: !Int32
  }
  deriving stock (Show, Eq)

fromResult :: PlayerRankingE Result -> PlayerRankingRow
fromResult PlayerRankingE{..} = PlayerRankingRow
  { prLeagueTeamId = _prLeagueTeamId
  , prPlayerId     = _prPlayerId
  , prRankSlot     = _prRankSlot
  }

-- ============================================================================
-- Transaction-flavored CRUD
-- ============================================================================

insertOneT :: PlayerRankingRow -> Tx.Transaction ()
insertOneT row = Tx.statement () $ R.run_ $ R.insert R.Insert
  { R.into       = playerRankingSchema
  , R.rows       = R.values
      [ PlayerRankingE
          { _prLeagueTeamId = R.lit (prLeagueTeamId row)
          , _prPlayerId     = R.lit (prPlayerId row)
          , _prRankSlot     = R.lit (prRankSlot row)
          , _prUpdatedAt    = R.unsafeDefault
          }
      ]
  , R.onConflict = R.Abort
  , R.returning  = R.NoReturning
  }

getRankingsForTeamT :: DbLeagueTeamId -> Tx.Transaction [PlayerRankingRow]
getRankingsForTeamT tid = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy (_prRankSlot >$< R.asc) $ do
      r <- R.each playerRankingSchema
      R.where_ (_prLeagueTeamId r ==. R.lit tid)
      pure r
  pure (map fromResult rows)

clearRankingsT :: DbLeagueTeamId -> Tx.Transaction ()
clearRankingsT tid = Tx.statement () $ R.run_ $ R.delete R.Delete
  { R.from        = playerRankingSchema
  , R.using       = pure ()
  , R.deleteWhere = \_ r -> _prLeagueTeamId r ==. R.lit tid
  , R.returning   = R.NoReturning
  }

replaceRankingsT :: DbLeagueTeamId -> [PlayerRankingRow] -> Tx.Transaction ()
replaceRankingsT tid rows = do
  clearRankingsT tid
  mapM_ insertOneT rows

getRankingCountT :: DbLeagueTeamId -> Tx.Transaction Int64
getRankingCountT tid = do
  ns <- Tx.statement () $ R.run $ R.select $ R.aggregate1 R.countStar $ do
    r <- R.each playerRankingSchema
    R.where_ (_prLeagueTeamId r ==. R.lit tid)
    pure r
  pure $ case ns of
    (n : _) -> n
    []      -> 0

-- ============================================================================
-- Pool-flavored CRUD
-- ============================================================================

getRankingsForTeam :: Pool -> DbLeagueTeamId -> IO (Either DBError [PlayerRankingRow])
getRankingsForTeam pool tid = runTransaction pool (getRankingsForTeamT tid)

replaceRankings :: Pool -> DbLeagueTeamId -> [PlayerRankingRow] -> IO (Either DBError ())
replaceRankings pool tid rows = runTransaction pool (replaceRankingsT tid rows)

clearRankings :: Pool -> DbLeagueTeamId -> IO (Either DBError ())
clearRankings pool tid = runTransaction pool (clearRankingsT tid)

getRankingCount :: Pool -> DbLeagueTeamId -> IO (Either DBError Int64)
getRankingCount pool tid = runTransaction pool (getRankingCountT tid)