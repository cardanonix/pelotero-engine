-- | Repository for the @team@ table and its @team_external_id@ side table.
--
-- The module exposes two parallel APIs:
--
--   * @*T@ functions return 'Tx.Transaction'. Use these for tests
--     (composable with rollback) and for multi-step atomic operations
--     (where you want a single transaction across several reads and
--     writes).
--   * Unsuffixed functions return @IO (Either DBError a)@. They wrap the
--     @T@ variants in 'runTransaction'. Use these from one-shot call sites
--     (a single sync step, a CLI tool, a one-off lookup).
--
-- The implementation lives in the @T@ variants; the IO wrappers are thin.
module Pelotero.DB.Team
  ( -- * Row type
    TeamRow(..)
    -- * Transaction-level API
  , insertTeamT
  , updateTeamT
  , getByIdT
  , getAllT
  , linkExternalIdT
  , lookupByExternalIdT
  , getExternalIdT
  , upsertByExternalIdT
    -- * Pool/IO API (wrappers)
  , insertTeam
  , updateTeam
  , getById
  , getAll
  , linkExternalId
  , lookupByExternalId
  , getExternalId
  , upsertByExternalId
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text                  (Text)
import Data.Time                  (UTCTime)
import qualified Data.Vector as V
import qualified Hasql.Decoders   as D
import qualified Hasql.Encoders   as E
import qualified Hasql.Statement  as Stmt
import qualified Hasql.Transaction as Tx

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.DB.Statement
import Pelotero.Domain.Id    (DbTeamId(..))

--------------------------------------------------------------------------------
-- Row type

data TeamRow = TeamRow
  { teamRowId                 :: !(Maybe DbTeamId)
  , teamRowName               :: !Text
  , teamRowAbbreviation       :: !Text
  , teamRowLocationName       :: !Text
  , teamRowLastSyncedProvider :: !(Maybe ProviderName)
  , teamRowLastSyncedAt       :: !(Maybe UTCTime)
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Transaction-level API

insertTeamT :: TeamRow -> Tx.Transaction DbTeamId
insertTeamT row = Tx.statement (toFieldsTuple row) insertStmt

updateTeamT :: DbTeamId -> TeamRow -> Tx.Transaction ()
updateTeamT tid row = Tx.statement (tid, toFieldsTuple row) updateStmt

getByIdT :: DbTeamId -> Tx.Transaction (Maybe TeamRow)
getByIdT tid = Tx.statement tid selectByIdStmt

getAllT :: Tx.Transaction [TeamRow]
getAllT = V.toList <$> Tx.statement () selectAllStmt

linkExternalIdT
  :: DbTeamId -> ProviderName -> Text -> Tx.Transaction ()
linkExternalIdT tid provider extId =
  Tx.statement (tid, provider, extId) linkExternalIdStmt

lookupByExternalIdT
  :: ProviderName -> Text -> Tx.Transaction (Maybe DbTeamId)
lookupByExternalIdT provider extId =
  Tx.statement (provider, extId) lookupByExternalIdStmt

getExternalIdT
  :: DbTeamId -> ProviderName -> Tx.Transaction (Maybe Text)
getExternalIdT tid provider =
  Tx.statement (tid, provider) getExternalIdStmt

-- | Atomic version of 'upsertByExternalId': lookup, then insert+link or
-- update, all in one transaction. No race window.
upsertByExternalIdT
  :: ProviderName
  -> Text
  -> TeamRow
  -> Tx.Transaction DbTeamId
upsertByExternalIdT provider extId row = do
  found <- lookupByExternalIdT provider extId
  case found of
    Just tid -> do
      updateTeamT tid row
      pure tid
    Nothing -> do
      tid <- insertTeamT row
      linkExternalIdT tid provider extId
      pure tid

--------------------------------------------------------------------------------
-- Pool/IO API (wrappers)

insertTeam :: Pool -> TeamRow -> IO (Either DBError DbTeamId)
insertTeam pool row = runTransaction pool (insertTeamT row)

updateTeam :: Pool -> DbTeamId -> TeamRow -> IO (Either DBError ())
updateTeam pool tid row = runTransaction pool (updateTeamT tid row)

getById :: Pool -> DbTeamId -> IO (Either DBError (Maybe TeamRow))
getById pool tid = runTransaction pool (getByIdT tid)

getAll :: Pool -> IO (Either DBError [TeamRow])
getAll pool = runTransaction pool getAllT

linkExternalId :: Pool -> DbTeamId -> ProviderName -> Text -> IO (Either DBError ())
linkExternalId pool tid provider extId =
  runTransaction pool (linkExternalIdT tid provider extId)

lookupByExternalId :: Pool -> ProviderName -> Text -> IO (Either DBError (Maybe DbTeamId))
lookupByExternalId pool provider extId =
  runTransaction pool (lookupByExternalIdT provider extId)

getExternalId :: Pool -> DbTeamId -> ProviderName -> IO (Either DBError (Maybe Text))
getExternalId pool tid provider =
  runTransaction pool (getExternalIdT tid provider)

upsertByExternalId
  :: Pool
  -> ProviderName
  -> Text
  -> TeamRow
  -> IO (Either DBError DbTeamId)
upsertByExternalId pool provider extId row =
  runTransaction pool (upsertByExternalIdT provider extId row)

--------------------------------------------------------------------------------
-- Field tuples and encoders

type TeamFields =
  ( Text                     -- name
  , Text                     -- abbreviation
  , Text                     -- location_name
  , Maybe ProviderName       -- last_synced_provider
  , Maybe UTCTime            -- last_synced_at
  )

toFieldsTuple :: TeamRow -> TeamFields
toFieldsTuple TeamRow{..} =
  ( teamRowName
  , teamRowAbbreviation
  , teamRowLocationName
  , teamRowLastSyncedProvider
  , teamRowLastSyncedAt
  )

teamFieldsEncoder :: E.Params TeamFields
teamFieldsEncoder =
     ((\(a,_,_,_,_) -> a) >$< encText)
  <> ((\(_,b,_,_,_) -> b) >$< encText)
  <> ((\(_,_,c,_,_) -> c) >$< encText)
  <> ((\(_,_,_,d,_) -> d) >$< encProviderMaybe)
  <> ((\(_,_,_,_,e) -> e) >$< encUTCTimeMaybe)

--------------------------------------------------------------------------------
-- Row decoder

rowDecoder :: D.Row TeamRow
rowDecoder = TeamRow
  <$> (Just <$> decDbTeamId)
  <*> decText
  <*> decText
  <*> decText
  <*> decProviderMaybe
  <*> decUTCTimeMaybe

--------------------------------------------------------------------------------
-- Statements

insertStmt :: Stmt.Statement TeamFields DbTeamId
insertStmt = Stmt.Statement sql teamFieldsEncoder (D.singleRow decDbTeamId) True
  where
    sql = "INSERT INTO team \
          \  (name, abbreviation, location_name, last_synced_provider, last_synced_at) \
          \VALUES ($1, $2, $3, $4, $5) \
          \RETURNING id"

updateStmt :: Stmt.Statement (DbTeamId, TeamFields) ()
updateStmt = Stmt.Statement sql encoder D.noResult True
  where
    sql = "UPDATE team SET \
          \  name = $2, \
          \  abbreviation = $3, \
          \  location_name = $4, \
          \  last_synced_provider = $5, \
          \  last_synced_at = $6, \
          \  updated_at = NOW() \
          \WHERE id = $1"
    encoder = (fst >$< encDbTeamId) <> (snd >$< teamFieldsEncoder)

selectByIdStmt :: Stmt.Statement DbTeamId (Maybe TeamRow)
selectByIdStmt = Stmt.Statement sql encDbTeamId (D.rowMaybe rowDecoder) True
  where
    sql = "SELECT id, name, abbreviation, location_name, \
          \       last_synced_provider, last_synced_at \
          \FROM team WHERE id = $1"

selectAllStmt :: Stmt.Statement () (V.Vector TeamRow)
selectAllStmt = Stmt.Statement sql E.noParams (D.rowVector rowDecoder) True
  where
    sql = "SELECT id, name, abbreviation, location_name, \
          \       last_synced_provider, last_synced_at \
          \FROM team \
          \ORDER BY name"

linkExternalIdStmt :: Stmt.Statement (DbTeamId, ProviderName, Text) ()
linkExternalIdStmt = Stmt.Statement sql encoder D.noResult True
  where
    sql = "INSERT INTO team_external_id (team_id, provider, external_id) \
          \VALUES ($1, $2, $3) \
          \ON CONFLICT (provider, external_id) DO NOTHING"
    encoder =
         ((\(a,_,_) -> a) >$< encDbTeamId)
      <> ((\(_,b,_) -> b) >$< encProvider)
      <> ((\(_,_,c) -> c) >$< encText)

lookupByExternalIdStmt :: Stmt.Statement (ProviderName, Text) (Maybe DbTeamId)
lookupByExternalIdStmt = Stmt.Statement sql encoder (D.rowMaybe decDbTeamId) True
  where
    sql = "SELECT team_id FROM team_external_id \
          \WHERE provider = $1 AND external_id = $2"
    encoder = (fst >$< encProvider) <> (snd >$< encText)

getExternalIdStmt :: Stmt.Statement (DbTeamId, ProviderName) (Maybe Text)
getExternalIdStmt = Stmt.Statement sql encoder (D.rowMaybe decText) True
  where
    sql = "SELECT external_id FROM team_external_id \
          \WHERE team_id = $1 AND provider = $2"
    encoder = (fst >$< encDbTeamId) <> (snd >$< encProvider)