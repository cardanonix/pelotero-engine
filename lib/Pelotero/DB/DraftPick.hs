-- | Repository for the @draft_pick@ table. Append-only during a draft;
-- the UNIQUE constraints on (league, pick_number) and (league, player)
-- enforce that no player is drafted twice and no pick slot is used twice.
module Pelotero.DB.DraftPick
  ( DraftPickRow(..)
  , recordPickT
  , getPicksForLeagueT
  , getPickCountT
  , recordPick
  , getPicksForLeague
  , getPickCount
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int                   (Int32, Int64)
import Data.Time                  (UTCTime)
import qualified Data.Vector as V
import qualified Hasql.Decoders   as D
import qualified Hasql.Encoders   as E
import qualified Hasql.Statement  as Stmt
import qualified Hasql.Transaction as Tx

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Statement
import Pelotero.Domain.Id
  ( DbDraftPickId(..)
  , DbLeagueConfigId(..)
  , DbLeagueTeamId(..)
  , DbPlayerId(..)
  )

data DraftPickRow = DraftPickRow
  { dpId              :: !(Maybe DbDraftPickId)
  , dpLeagueConfigId  :: !DbLeagueConfigId
  , dpPickNumber      :: !Int32
  , dpLeagueTeamId    :: !DbLeagueTeamId
  , dpPlayerId        :: !DbPlayerId
  , dpPickedAt        :: !(Maybe UTCTime)
  }
  deriving stock (Show, Eq)

recordPickT :: DraftPickRow -> Tx.Transaction DbDraftPickId
recordPickT row = Tx.statement row insertStmt

getPicksForLeagueT :: DbLeagueConfigId -> Tx.Transaction [DraftPickRow]
getPicksForLeagueT lcid = V.toList <$> Tx.statement lcid selectForLeagueStmt

getPickCountT :: DbLeagueConfigId -> Tx.Transaction Int64
getPickCountT lcid = do
  mc <- Tx.statement lcid countStmt
  pure (maybe 0 id mc)

recordPick :: Pool -> DraftPickRow -> IO (Either DBError DbDraftPickId)
recordPick pool row = runTransaction pool (recordPickT row)

getPicksForLeague :: Pool -> DbLeagueConfigId -> IO (Either DBError [DraftPickRow])
getPicksForLeague pool lcid = runTransaction pool (getPicksForLeagueT lcid)

getPickCount :: Pool -> DbLeagueConfigId -> IO (Either DBError Int64)
getPickCount pool lcid = runTransaction pool (getPickCountT lcid)

insertEncoder :: E.Params DraftPickRow
insertEncoder =
     (dpLeagueConfigId >$< encDbLeagueConfigId)
  <> (dpPickNumber     >$< encInt32')
  <> (dpLeagueTeamId   >$< encDbLeagueTeamId)
  <> (dpPlayerId       >$< encDbPlayerId)
  where
    encInt32' :: E.Params Int32
    encInt32' = E.param (E.nonNullable E.int4)

rowDecoder :: D.Row DraftPickRow
rowDecoder = DraftPickRow
  <$> (Just <$> decDbDraftPickId)
  <*> decDbLeagueConfigId
  <*> D.column (D.nonNullable D.int4)
  <*> decDbLeagueTeamId
  <*> decDbPlayerId
  <*> (Just <$> decUTCTime)

insertStmt :: Stmt.Statement DraftPickRow DbDraftPickId
insertStmt = Stmt.Statement sql insertEncoder (D.singleRow decDbDraftPickId) True
  where
    sql = "INSERT INTO draft_pick \
          \  (league_config_id, pick_number, league_team_id, player_id) \
          \VALUES ($1, $2, $3, $4) \
          \RETURNING id"

selectForLeagueStmt :: Stmt.Statement DbLeagueConfigId (V.Vector DraftPickRow)
selectForLeagueStmt = Stmt.Statement sql encDbLeagueConfigId (D.rowVector rowDecoder) True
  where
    sql = "SELECT id, league_config_id, pick_number, league_team_id, \
          \       player_id, picked_at \
          \FROM draft_pick \
          \WHERE league_config_id = $1 \
          \ORDER BY pick_number"

countStmt :: Stmt.Statement DbLeagueConfigId (Maybe Int64)
countStmt = Stmt.Statement sql encDbLeagueConfigId (D.rowMaybe decInt64) True
  where
    sql = "SELECT COUNT(*) FROM draft_pick WHERE league_config_id = $1"