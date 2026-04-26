-- | Repository for the @game@ table and its @game_external_id@ side table.
-- See "Pelotero.DB.Team" for the API conventions.
--
-- A game's away/home teams are required (NOT NULL FKs to @team@); upsert
-- callers must have already inserted the teams first. The sync layer is
-- responsible for ordering: teams come before games come before stats.
module Pelotero.DB.Game
  ( -- * Row type
    GameRow(..)
    -- * Transaction-level API
  , insertGameT
  , updateGameT
  , getByIdT
  , getByDateT
  , linkExternalIdT
  , lookupByExternalIdT
  , getExternalIdT
  , upsertByExternalIdT
    -- * Pool/IO API (wrappers)
  , insertGame
  , updateGame
  , getById
  , getByDate
  , linkExternalId
  , lookupByExternalId
  , getExternalId
  , upsertByExternalId
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text                  (Text)
import Data.Time                  (Day, UTCTime)
import qualified Data.Vector as V
import qualified Hasql.Decoders   as D
import qualified Hasql.Encoders   as E
import qualified Hasql.Statement  as Stmt
import qualified Hasql.Transaction as Tx

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.DB.Statement
import Pelotero.Domain.Id    (DbGameId(..), DbTeamId)

--------------------------------------------------------------------------------
-- Row type

data GameRow = GameRow
  { gameRowId                 :: !(Maybe DbGameId)
  , gameRowGameDate           :: !Day
  , gameRowAwayTeamId         :: !DbTeamId
  , gameRowHomeTeamId         :: !DbTeamId
  , gameRowLastSyncedProvider :: !(Maybe ProviderName)
  , gameRowLastSyncedAt       :: !(Maybe UTCTime)
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Transaction-level API

insertGameT :: GameRow -> Tx.Transaction DbGameId
insertGameT row = Tx.statement (toFieldsTuple row) insertStmt

updateGameT :: DbGameId -> GameRow -> Tx.Transaction ()
updateGameT gid row = Tx.statement (gid, toFieldsTuple row) updateStmt

getByIdT :: DbGameId -> Tx.Transaction (Maybe GameRow)
getByIdT gid = Tx.statement gid selectByIdStmt

-- | All games on a given calendar date, ordered by id (the natural insertion
-- order, which roughly tracks game-time). Used by the day-by-day sync flow.
getByDateT :: Day -> Tx.Transaction [GameRow]
getByDateT d = V.toList <$> Tx.statement d selectByDateStmt

linkExternalIdT
  :: DbGameId -> ProviderName -> Text -> Tx.Transaction ()
linkExternalIdT gid provider extId =
  Tx.statement (gid, provider, extId) linkExternalIdStmt

lookupByExternalIdT
  :: ProviderName -> Text -> Tx.Transaction (Maybe DbGameId)
lookupByExternalIdT provider extId =
  Tx.statement (provider, extId) lookupByExternalIdStmt

getExternalIdT
  :: DbGameId -> ProviderName -> Tx.Transaction (Maybe Text)
getExternalIdT gid provider =
  Tx.statement (gid, provider) getExternalIdStmt

upsertByExternalIdT
  :: ProviderName -> Text -> GameRow -> Tx.Transaction DbGameId
upsertByExternalIdT provider extId row = do
  found <- lookupByExternalIdT provider extId
  case found of
    Just gid -> do
      updateGameT gid row
      pure gid
    Nothing -> do
      gid <- insertGameT row
      linkExternalIdT gid provider extId
      pure gid

--------------------------------------------------------------------------------
-- Pool/IO API (wrappers)

insertGame :: Pool -> GameRow -> IO (Either DBError DbGameId)
insertGame pool row = runTransaction pool (insertGameT row)

updateGame :: Pool -> DbGameId -> GameRow -> IO (Either DBError ())
updateGame pool gid row = runTransaction pool (updateGameT gid row)

getById :: Pool -> DbGameId -> IO (Either DBError (Maybe GameRow))
getById pool gid = runTransaction pool (getByIdT gid)

getByDate :: Pool -> Day -> IO (Either DBError [GameRow])
getByDate pool d = runTransaction pool (getByDateT d)

linkExternalId :: Pool -> DbGameId -> ProviderName -> Text -> IO (Either DBError ())
linkExternalId pool gid provider extId =
  runTransaction pool (linkExternalIdT gid provider extId)

lookupByExternalId :: Pool -> ProviderName -> Text -> IO (Either DBError (Maybe DbGameId))
lookupByExternalId pool provider extId =
  runTransaction pool (lookupByExternalIdT provider extId)

getExternalId :: Pool -> DbGameId -> ProviderName -> IO (Either DBError (Maybe Text))
getExternalId pool gid provider =
  runTransaction pool (getExternalIdT gid provider)

upsertByExternalId :: Pool -> ProviderName -> Text -> GameRow -> IO (Either DBError DbGameId)
upsertByExternalId pool provider extId row =
  runTransaction pool (upsertByExternalIdT provider extId row)

--------------------------------------------------------------------------------
-- Field tuple and encoder

type GameFields =
  ( Day                    -- game_date
  , DbTeamId               -- away_team_id
  , DbTeamId               -- home_team_id
  , Maybe ProviderName     -- last_synced_provider
  , Maybe UTCTime          -- last_synced_at
  )

toFieldsTuple :: GameRow -> GameFields
toFieldsTuple GameRow{..} =
  ( gameRowGameDate
  , gameRowAwayTeamId
  , gameRowHomeTeamId
  , gameRowLastSyncedProvider
  , gameRowLastSyncedAt
  )

gameFieldsEncoder :: E.Params GameFields
gameFieldsEncoder =
     ((\(a,_,_,_,_) -> a) >$< encDay)
  <> ((\(_,b,_,_,_) -> b) >$< encDbTeamId)
  <> ((\(_,_,c,_,_) -> c) >$< encDbTeamId)
  <> ((\(_,_,_,d,_) -> d) >$< encProviderMaybe)
  <> ((\(_,_,_,_,e) -> e) >$< encUTCTimeMaybe)

--------------------------------------------------------------------------------
-- Row decoder

rowDecoder :: D.Row GameRow
rowDecoder = GameRow
  <$> (Just <$> decDbGameId)
  <*> decDay
  <*> decDbTeamId
  <*> decDbTeamId
  <*> decProviderMaybe
  <*> decUTCTimeMaybe

--------------------------------------------------------------------------------
-- Statements

insertStmt :: Stmt.Statement GameFields DbGameId
insertStmt = Stmt.Statement sql gameFieldsEncoder (D.singleRow decDbGameId) True
  where
    sql = "INSERT INTO game \
          \  (game_date, away_team_id, home_team_id, \
          \   last_synced_provider, last_synced_at) \
          \VALUES ($1, $2, $3, $4, $5) \
          \RETURNING id"

updateStmt :: Stmt.Statement (DbGameId, GameFields) ()
updateStmt = Stmt.Statement sql encoder D.noResult True
  where
    sql = "UPDATE game SET \
          \  game_date            = $2, \
          \  away_team_id         = $3, \
          \  home_team_id         = $4, \
          \  last_synced_provider = $5, \
          \  last_synced_at       = $6, \
          \  updated_at = NOW() \
          \WHERE id = $1"
    encoder = (fst >$< encDbGameId) <> (snd >$< gameFieldsEncoder)

selectByIdStmt :: Stmt.Statement DbGameId (Maybe GameRow)
selectByIdStmt = Stmt.Statement sql encDbGameId (D.rowMaybe rowDecoder) True
  where
    sql = "SELECT id, game_date, away_team_id, home_team_id, \
          \       last_synced_provider, last_synced_at \
          \FROM game WHERE id = $1"

selectByDateStmt :: Stmt.Statement Day (V.Vector GameRow)
selectByDateStmt = Stmt.Statement sql encDay (D.rowVector rowDecoder) True
  where
    sql = "SELECT id, game_date, away_team_id, home_team_id, \
          \       last_synced_provider, last_synced_at \
          \FROM game \
          \WHERE game_date = $1 \
          \ORDER BY id"

linkExternalIdStmt :: Stmt.Statement (DbGameId, ProviderName, Text) ()
linkExternalIdStmt = Stmt.Statement sql encoder D.noResult True
  where
    sql = "INSERT INTO game_external_id (game_id, provider, external_id) \
          \VALUES ($1, $2, $3) \
          \ON CONFLICT (provider, external_id) DO NOTHING"
    encoder =
         ((\(a,_,_) -> a) >$< encDbGameId)
      <> ((\(_,b,_) -> b) >$< encProvider)
      <> ((\(_,_,c) -> c) >$< encText)

lookupByExternalIdStmt :: Stmt.Statement (ProviderName, Text) (Maybe DbGameId)
lookupByExternalIdStmt = Stmt.Statement sql encoder (D.rowMaybe decDbGameId) True
  where
    sql = "SELECT game_id FROM game_external_id \
          \WHERE provider = $1 AND external_id = $2"
    encoder = (fst >$< encProvider) <> (snd >$< encText)

getExternalIdStmt :: Stmt.Statement (DbGameId, ProviderName) (Maybe Text)
getExternalIdStmt = Stmt.Statement sql encoder (D.rowMaybe decText) True
  where
    sql = "SELECT external_id FROM game_external_id \
          \WHERE game_id = $1 AND provider = $2"
    encoder = (fst >$< encDbGameId) <> (snd >$< encProvider)