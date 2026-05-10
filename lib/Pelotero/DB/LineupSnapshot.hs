{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pelotero.DB.LineupSnapshot
  ( LineupSnapshotRow(..)

  , writeSnapshotsT
  , getSnapshotForTeamGameT
  , snapshotExistsForTeamGameT
  , getSnapshotsForGameT
  , getSnapshotsForDateRangeT

  , writeSnapshots
  , getSnapshotForTeamGame
  , snapshotExistsForTeamGame
  , getSnapshotsForGame
  , getSnapshotsForDateRange
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import           Data.Time                  (Day, UTCTime)
import           GHC.Generics               (Generic)

import qualified Hasql.Transaction          as Tx

import           Rel8                       ( Column
                                            , Name
                                            , Rel8able
                                            , Result
                                            , TableSchema(..)
                                            , (==.)
                                            , (>=.)
                                            , (<=.)
                                            )
import qualified Rel8                       as R

import qualified Pelotero.DB.Game            as Game
import           Pelotero.DB.Pool            (DBError, Pool, runTransaction)
import           Pelotero.DB.Rel8Instances   ()
import           Pelotero.Domain.Id          (DbGameId, DbLeagueTeamId, DbPlayerId)

data LineupSnapshot f = LineupSnapshot
  { _lsnapId            :: Column f Int64
  , _lsnapLeagueTeamId  :: Column f DbLeagueTeamId
  , _lsnapGameId        :: Column f DbGameId
  , _lsnapSlot          :: Column f Text
  , _lsnapPlayerId      :: Column f DbPlayerId
  , _lsnapSnapshottedAt :: Column f UTCTime
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (LineupSnapshot f)
deriving stock instance f ~ Result => Eq   (LineupSnapshot f)

lineupSnapshotSchema :: TableSchema (LineupSnapshot Name)
lineupSnapshotSchema = TableSchema
  { name    = "lineup_snapshot"
  , columns = LineupSnapshot
      { _lsnapId            = "id"
      , _lsnapLeagueTeamId  = "league_team_id"
      , _lsnapGameId        = "game_id"
      , _lsnapSlot          = "slot"
      , _lsnapPlayerId      = "player_id"
      , _lsnapSnapshottedAt = "snapshotted_at"
      }
  }

data LineupSnapshotRow = LineupSnapshotRow
  { lsnapLeagueTeamId :: !DbLeagueTeamId
  , lsnapGameId       :: !DbGameId
  , lsnapSlot         :: !Text
  , lsnapPlayerId     :: !DbPlayerId
  }
  deriving stock (Show, Eq)

fromResult :: LineupSnapshot Result -> LineupSnapshotRow
fromResult LineupSnapshot{..} = LineupSnapshotRow
  { lsnapLeagueTeamId = _lsnapLeagueTeamId
  , lsnapGameId       = _lsnapGameId
  , lsnapSlot         = _lsnapSlot
  , lsnapPlayerId     = _lsnapPlayerId
  }

writeSnapshotsT :: [LineupSnapshotRow] -> Tx.Transaction ()
writeSnapshotsT []   = pure ()
writeSnapshotsT rows = Tx.statement () $ R.run_ $ R.insert R.Insert
  { R.into       = lineupSnapshotSchema
  , R.rows       = R.values (map rowToExpr rows)
  , R.onConflict = R.DoNothing
  , R.returning  = R.NoReturning
  }
  where
    rowToExpr r = LineupSnapshot
      { _lsnapId            = R.unsafeDefault
      , _lsnapLeagueTeamId  = R.lit (lsnapLeagueTeamId r)
      , _lsnapGameId        = R.lit (lsnapGameId r)
      , _lsnapSlot          = R.lit (lsnapSlot r)
      , _lsnapPlayerId      = R.lit (lsnapPlayerId r)
      , _lsnapSnapshottedAt = R.unsafeDefault
      }

getSnapshotForTeamGameT
  :: DbLeagueTeamId -> DbGameId -> Tx.Transaction [LineupSnapshotRow]
getSnapshotForTeamGameT ltid gid = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    s <- R.each lineupSnapshotSchema
    R.where_ (_lsnapLeagueTeamId s ==. R.lit ltid)
    R.where_ (_lsnapGameId       s ==. R.lit gid)
    pure s
  pure (map fromResult rows)

snapshotExistsForTeamGameT
  :: DbLeagueTeamId -> DbGameId -> Tx.Transaction Bool
snapshotExistsForTeamGameT ltid gid = do
  rows <- Tx.statement () $ R.run $ R.select $ R.limit 1 $ do
    s <- R.each lineupSnapshotSchema
    R.where_ (_lsnapLeagueTeamId s ==. R.lit ltid)
    R.where_ (_lsnapGameId       s ==. R.lit gid)
    pure s
  pure (not (null rows))

getSnapshotsForGameT :: DbGameId -> Tx.Transaction [LineupSnapshotRow]
getSnapshotsForGameT gid = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    s <- R.each lineupSnapshotSchema
    R.where_ (_lsnapGameId s ==. R.lit gid)
    pure s
  pure (map fromResult rows)

-- | Single SELECT: every snapshot row for games whose game_date falls in
-- [startDay, endDay]. Replaces the per-(team, game) round-trips that the
-- scoring engine used to do.
getSnapshotsForDateRangeT
  :: Day -> Day -> Tx.Transaction [LineupSnapshotRow]
getSnapshotsForDateRangeT startDay endDay = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy ((_lsnapLeagueTeamId >$< R.asc) <> (_lsnapGameId >$< R.asc)) $ do
      g <- R.each Game.gameSchema
      s <- R.each lineupSnapshotSchema
      R.where_ (Game._gameId       g ==. _lsnapGameId s)
      R.where_ (Game._gameGameDate g >=. R.lit startDay)
      R.where_ (Game._gameGameDate g <=. R.lit endDay)
      pure s
  pure (map fromResult rows)

writeSnapshots :: Pool -> [LineupSnapshotRow] -> IO (Either DBError ())
writeSnapshots pool rs = runTransaction pool (writeSnapshotsT rs)

getSnapshotForTeamGame
  :: Pool
  -> DbLeagueTeamId
  -> DbGameId
  -> IO (Either DBError [LineupSnapshotRow])
getSnapshotForTeamGame pool ltid gid =
  runTransaction pool (getSnapshotForTeamGameT ltid gid)

snapshotExistsForTeamGame
  :: Pool -> DbLeagueTeamId -> DbGameId -> IO (Either DBError Bool)
snapshotExistsForTeamGame pool ltid gid =
  runTransaction pool (snapshotExistsForTeamGameT ltid gid)

getSnapshotsForGame
  :: Pool -> DbGameId -> IO (Either DBError [LineupSnapshotRow])
getSnapshotsForGame pool gid = runTransaction pool (getSnapshotsForGameT gid)

getSnapshotsForDateRange
  :: Pool -> Day -> Day -> IO (Either DBError [LineupSnapshotRow])
getSnapshotsForDateRange pool s e =
  runTransaction pool (getSnapshotsForDateRangeT s e)