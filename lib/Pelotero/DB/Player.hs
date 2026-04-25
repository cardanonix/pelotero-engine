-- | Repository for the @player@ table and its @player_external_id@ side
-- table. See "Pelotero.DB.Team" for the API conventions.
module Pelotero.DB.Player
  ( -- * Row type
    PlayerRow(..)
    -- * Transaction-level API
  , insertPlayerT
  , updatePlayerT
  , getByIdT
  , getAllT
  , getActiveT
  , linkExternalIdT
  , lookupByExternalIdT
  , getExternalIdT
  , upsertByExternalIdT
    -- * Pool/IO API (wrappers)
  , insertPlayer
  , updatePlayer
  , getById
  , getAll
  , getActive
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
import Pelotero.Domain.Id    (DbPlayerId(..), DbTeamId)

--------------------------------------------------------------------------------
-- Row type

data PlayerRow = PlayerRow
  { playerRowId                 :: !(Maybe DbPlayerId)
  , playerRowFirstName          :: !Text
  , playerRowLastName           :: !Text
  , playerRowNameSlug           :: !Text
  , playerRowPosition           :: !(Maybe Text)
  , playerRowBatSide            :: !(Maybe Char)
  , playerRowPitchHand          :: !(Maybe Char)
  , playerRowActive             :: !Bool
  , playerRowCurrentTeamId      :: !(Maybe DbTeamId)
  , playerRowLastSyncedProvider :: !(Maybe ProviderName)
  , playerRowLastSyncedAt       :: !(Maybe UTCTime)
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Transaction-level API

insertPlayerT :: PlayerRow -> Tx.Transaction DbPlayerId
insertPlayerT row = Tx.statement (toFieldsTuple row) insertStmt

updatePlayerT :: DbPlayerId -> PlayerRow -> Tx.Transaction ()
updatePlayerT pid row = Tx.statement (pid, toFieldsTuple row) updateStmt

getByIdT :: DbPlayerId -> Tx.Transaction (Maybe PlayerRow)
getByIdT pid = Tx.statement pid selectByIdStmt

getAllT :: Tx.Transaction [PlayerRow]
getAllT = V.toList <$> Tx.statement () selectAllStmt

getActiveT :: Tx.Transaction [PlayerRow]
getActiveT = V.toList <$> Tx.statement () selectActiveStmt

linkExternalIdT
  :: DbPlayerId -> ProviderName -> Text -> Tx.Transaction ()
linkExternalIdT pid provider extId =
  Tx.statement (pid, provider, extId) linkExternalIdStmt

lookupByExternalIdT
  :: ProviderName -> Text -> Tx.Transaction (Maybe DbPlayerId)
lookupByExternalIdT provider extId =
  Tx.statement (provider, extId) lookupByExternalIdStmt

getExternalIdT
  :: DbPlayerId -> ProviderName -> Tx.Transaction (Maybe Text)
getExternalIdT pid provider =
  Tx.statement (pid, provider) getExternalIdStmt

upsertByExternalIdT
  :: ProviderName -> Text -> PlayerRow -> Tx.Transaction DbPlayerId
upsertByExternalIdT provider extId row = do
  found <- lookupByExternalIdT provider extId
  case found of
    Just pid -> do
      updatePlayerT pid row
      pure pid
    Nothing -> do
      pid <- insertPlayerT row
      linkExternalIdT pid provider extId
      pure pid

--------------------------------------------------------------------------------
-- Pool/IO API (wrappers)

insertPlayer :: Pool -> PlayerRow -> IO (Either DBError DbPlayerId)
insertPlayer pool row = runTransaction pool (insertPlayerT row)

updatePlayer :: Pool -> DbPlayerId -> PlayerRow -> IO (Either DBError ())
updatePlayer pool pid row = runTransaction pool (updatePlayerT pid row)

getById :: Pool -> DbPlayerId -> IO (Either DBError (Maybe PlayerRow))
getById pool pid = runTransaction pool (getByIdT pid)

getAll :: Pool -> IO (Either DBError [PlayerRow])
getAll pool = runTransaction pool getAllT

getActive :: Pool -> IO (Either DBError [PlayerRow])
getActive pool = runTransaction pool getActiveT

linkExternalId :: Pool -> DbPlayerId -> ProviderName -> Text -> IO (Either DBError ())
linkExternalId pool pid provider extId =
  runTransaction pool (linkExternalIdT pid provider extId)

lookupByExternalId :: Pool -> ProviderName -> Text -> IO (Either DBError (Maybe DbPlayerId))
lookupByExternalId pool provider extId =
  runTransaction pool (lookupByExternalIdT provider extId)

getExternalId :: Pool -> DbPlayerId -> ProviderName -> IO (Either DBError (Maybe Text))
getExternalId pool pid provider =
  runTransaction pool (getExternalIdT pid provider)

upsertByExternalId
  :: Pool -> ProviderName -> Text -> PlayerRow -> IO (Either DBError DbPlayerId)
upsertByExternalId pool provider extId row =
  runTransaction pool (upsertByExternalIdT provider extId row)

--------------------------------------------------------------------------------
-- Field tuple and encoder

type PlayerFields =
  ( Text
  , Text
  , Text
  , Maybe Text
  , Maybe Char
  , Maybe Char
  , Bool
  , Maybe DbTeamId
  , Maybe ProviderName
  , Maybe UTCTime
  )

toFieldsTuple :: PlayerRow -> PlayerFields
toFieldsTuple PlayerRow{..} =
  ( playerRowFirstName
  , playerRowLastName
  , playerRowNameSlug
  , playerRowPosition
  , playerRowBatSide
  , playerRowPitchHand
  , playerRowActive
  , playerRowCurrentTeamId
  , playerRowLastSyncedProvider
  , playerRowLastSyncedAt
  )

playerFieldsEncoder :: E.Params PlayerFields
playerFieldsEncoder =
     ((\(a,_,_,_,_,_,_,_,_,_) -> a) >$< encText)
  <> ((\(_,b,_,_,_,_,_,_,_,_) -> b) >$< encText)
  <> ((\(_,_,c,_,_,_,_,_,_,_) -> c) >$< encText)
  <> ((\(_,_,_,d,_,_,_,_,_,_) -> d) >$< encTextMaybe)
  <> ((\(_,_,_,_,e,_,_,_,_,_) -> e) >$< encChar1Maybe)
  <> ((\(_,_,_,_,_,f,_,_,_,_) -> f) >$< encChar1Maybe)
  <> ((\(_,_,_,_,_,_,g,_,_,_) -> g) >$< encBool)
  <> ((\(_,_,_,_,_,_,_,h,_,_) -> h) >$< encDbTeamIdMaybe)
  <> ((\(_,_,_,_,_,_,_,_,i,_) -> i) >$< encProviderMaybe)
  <> ((\(_,_,_,_,_,_,_,_,_,j) -> j) >$< encUTCTimeMaybe)

--------------------------------------------------------------------------------
-- Row decoder

rowDecoder :: D.Row PlayerRow
rowDecoder = PlayerRow
  <$> (Just <$> decDbPlayerId)
  <*> decText
  <*> decText
  <*> decText
  <*> decTextMaybe
  <*> decChar1Maybe
  <*> decChar1Maybe
  <*> decBool
  <*> decDbTeamIdMaybe
  <*> decProviderMaybe
  <*> decUTCTimeMaybe

--------------------------------------------------------------------------------
-- Statements

insertStmt :: Stmt.Statement PlayerFields DbPlayerId
insertStmt = Stmt.Statement sql playerFieldsEncoder (D.singleRow decDbPlayerId) True
  where
    sql = "INSERT INTO player \
          \  (first_name, last_name, name_slug, position, \
          \   bat_side, pitch_hand, active, current_team_id, \
          \   last_synced_provider, last_synced_at) \
          \VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10) \
          \RETURNING id"

updateStmt :: Stmt.Statement (DbPlayerId, PlayerFields) ()
updateStmt = Stmt.Statement sql encoder D.noResult True
  where
    sql = "UPDATE player SET \
          \  first_name = $2, \
          \  last_name  = $3, \
          \  name_slug  = $4, \
          \  position   = $5, \
          \  bat_side   = $6, \
          \  pitch_hand = $7, \
          \  active     = $8, \
          \  current_team_id      = $9, \
          \  last_synced_provider = $10, \
          \  last_synced_at       = $11, \
          \  updated_at = NOW() \
          \WHERE id = $1"
    encoder = (fst >$< encDbPlayerId) <> (snd >$< playerFieldsEncoder)

selectByIdStmt :: Stmt.Statement DbPlayerId (Maybe PlayerRow)
selectByIdStmt = Stmt.Statement sql encDbPlayerId (D.rowMaybe rowDecoder) True
  where
    sql = "SELECT id, first_name, last_name, name_slug, position, \
          \       bat_side, pitch_hand, active, current_team_id, \
          \       last_synced_provider, last_synced_at \
          \FROM player WHERE id = $1"

selectAllStmt :: Stmt.Statement () (V.Vector PlayerRow)
selectAllStmt = Stmt.Statement sql E.noParams (D.rowVector rowDecoder) True
  where
    sql = "SELECT id, first_name, last_name, name_slug, position, \
          \       bat_side, pitch_hand, active, current_team_id, \
          \       last_synced_provider, last_synced_at \
          \FROM player \
          \ORDER BY last_name, first_name"

selectActiveStmt :: Stmt.Statement () (V.Vector PlayerRow)
selectActiveStmt = Stmt.Statement sql E.noParams (D.rowVector rowDecoder) True
  where
    sql = "SELECT id, first_name, last_name, name_slug, position, \
          \       bat_side, pitch_hand, active, current_team_id, \
          \       last_synced_provider, last_synced_at \
          \FROM player WHERE active = TRUE \
          \ORDER BY last_name, first_name"

linkExternalIdStmt :: Stmt.Statement (DbPlayerId, ProviderName, Text) ()
linkExternalIdStmt = Stmt.Statement sql encoder D.noResult True
  where
    sql = "INSERT INTO player_external_id (player_id, provider, external_id) \
          \VALUES ($1, $2, $3) \
          \ON CONFLICT (provider, external_id) DO NOTHING"
    encoder =
         ((\(a,_,_) -> a) >$< encDbPlayerId)
      <> ((\(_,b,_) -> b) >$< encProvider)
      <> ((\(_,_,c) -> c) >$< encText)

lookupByExternalIdStmt :: Stmt.Statement (ProviderName, Text) (Maybe DbPlayerId)
lookupByExternalIdStmt = Stmt.Statement sql encoder (D.rowMaybe decDbPlayerId) True
  where
    sql = "SELECT player_id FROM player_external_id \
          \WHERE provider = $1 AND external_id = $2"
    encoder = (fst >$< encProvider) <> (snd >$< encText)

getExternalIdStmt :: Stmt.Statement (DbPlayerId, ProviderName) (Maybe Text)
getExternalIdStmt = Stmt.Statement sql encoder (D.rowMaybe decText) True
  where
    sql = "SELECT external_id FROM player_external_id \
          \WHERE player_id = $1 AND provider = $2"
    encoder = (fst >$< encDbPlayerId) <> (snd >$< encProvider)