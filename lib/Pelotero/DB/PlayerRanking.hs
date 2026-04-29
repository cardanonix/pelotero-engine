-- | Repository for the @player_ranking@ table. Rankings are always
-- operated on as a complete list per team: read all, replace all, clear.
-- There is no single-row upsert because partial ranking edits are a
-- domain error (the ordering of the full list is the ranking).
module Pelotero.DB.PlayerRanking
  ( -- * Row type
    PlayerRankingRow(..)
    -- * Transaction-level API
  , getRankingsForTeamT
  , replaceRankingsT
  , clearRankingsT
  , getRankingCountT
    -- * Pool/IO API
  , getRankingsForTeam
  , replaceRankings
  , clearRankings
  , getRankingCount
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int                   (Int32, Int64)
import qualified Data.Vector as V
import qualified Hasql.Decoders   as D
import qualified Hasql.Encoders   as E
import qualified Hasql.Statement  as Stmt
import qualified Hasql.Transaction as Tx

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Statement
import Pelotero.Domain.Id    (DbLeagueTeamId(..), DbPlayerId(..))

--------------------------------------------------------------------------------
-- Row type

data PlayerRankingRow = PlayerRankingRow
  { prLeagueTeamId :: !DbLeagueTeamId
  , prPlayerId     :: !DbPlayerId
  , prRankSlot     :: !Int32
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Transaction-level API

getRankingsForTeamT :: DbLeagueTeamId -> Tx.Transaction [PlayerRankingRow]
getRankingsForTeamT tid = V.toList <$> Tx.statement tid selectForTeamStmt

-- | Replace all rankings for a team atomically. Callers pass the full
-- ranked list in order; 'prRankSlot' in each row must already be set.
replaceRankingsT :: DbLeagueTeamId -> [PlayerRankingRow] -> Tx.Transaction ()
replaceRankingsT tid rows = do
  clearRankingsT tid
  mapM_ insertOneT rows

clearRankingsT :: DbLeagueTeamId -> Tx.Transaction ()
clearRankingsT tid = Tx.statement tid deleteAllStmt

getRankingCountT :: DbLeagueTeamId -> Tx.Transaction Int64
getRankingCountT tid = do
  mc <- Tx.statement tid countStmt
  pure (maybe 0 id mc)

--------------------------------------------------------------------------------
-- Pool/IO API

getRankingsForTeam :: Pool -> DbLeagueTeamId -> IO (Either DBError [PlayerRankingRow])
getRankingsForTeam pool tid = runTransaction pool (getRankingsForTeamT tid)

replaceRankings :: Pool -> DbLeagueTeamId -> [PlayerRankingRow] -> IO (Either DBError ())
replaceRankings pool tid rows = runTransaction pool (replaceRankingsT tid rows)

clearRankings :: Pool -> DbLeagueTeamId -> IO (Either DBError ())
clearRankings pool tid = runTransaction pool (clearRankingsT tid)

getRankingCount :: Pool -> DbLeagueTeamId -> IO (Either DBError Int64)
getRankingCount pool tid = runTransaction pool (getRankingCountT tid)

--------------------------------------------------------------------------------
-- Internal

insertOneT :: PlayerRankingRow -> Tx.Transaction ()
insertOneT row = Tx.statement row insertStmt

--------------------------------------------------------------------------------
-- Encoder / Decoder

rowEncoder :: E.Params PlayerRankingRow
rowEncoder =
     (prLeagueTeamId >$< encDbLeagueTeamId)
  <> (prPlayerId     >$< encDbPlayerId)
  <> (prRankSlot     >$< encInt32')
  where
    encInt32' :: E.Params Int32
    encInt32' = E.param (E.nonNullable E.int4)

rowDecoder :: D.Row PlayerRankingRow
rowDecoder = PlayerRankingRow
  <$> decDbLeagueTeamId
  <*> decDbPlayerId
  <*> D.column (D.nonNullable D.int4)

--------------------------------------------------------------------------------
-- Statements

insertStmt :: Stmt.Statement PlayerRankingRow ()
insertStmt = Stmt.Statement sql rowEncoder D.noResult True
  where
    sql = "INSERT INTO player_ranking \
          \  (league_team_id, player_id, rank_slot) \
          \VALUES ($1, $2, $3)"

selectForTeamStmt :: Stmt.Statement DbLeagueTeamId (V.Vector PlayerRankingRow)
selectForTeamStmt = Stmt.Statement sql encDbLeagueTeamId (D.rowVector rowDecoder) True
  where
    sql = "SELECT league_team_id, player_id, rank_slot \
          \FROM player_ranking \
          \WHERE league_team_id = $1 \
          \ORDER BY rank_slot"

deleteAllStmt :: Stmt.Statement DbLeagueTeamId ()
deleteAllStmt = Stmt.Statement sql encDbLeagueTeamId D.noResult True
  where
    sql = "DELETE FROM player_ranking WHERE league_team_id = $1"

countStmt :: Stmt.Statement DbLeagueTeamId (Maybe Int64)
countStmt = Stmt.Statement sql encDbLeagueTeamId (D.rowMaybe decInt64) True
  where
    sql = "SELECT COUNT(*) FROM player_ranking WHERE league_team_id = $1"