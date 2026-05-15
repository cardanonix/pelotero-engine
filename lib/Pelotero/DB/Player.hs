{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pelotero.DB.Player
  ( Player (..)
  , playerSchema
  , playerExternalIdSchema
  , PlayerRow (..)
  , LoadedPlayerRow (..)
  , playerRowToLoaded
  , insertPlayerT
  , updatePlayerT
  , getByIdT
  , getAllT
  , getActiveT
  , linkExternalIdT
  , lookupByExternalIdT
  , getExternalIdT
  , upsertByExternalIdT
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

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified Hasql.Transaction as Tx
import Rel8 hiding (fromResult)

import Pelotero.DB.Pool (DBError, Pool, runTransaction)
import qualified Pelotero.DB.ProviderKeyed as PK
import Pelotero.DB.ProviderKeyed (ExternalIdE (..), ProviderKeyed (..))
import Pelotero.DB.Provider (ProviderName)
import Pelotero.DB.Rel8Instances ()
import Pelotero.Domain.Id (DbPlayerId, DbTeamId)

-- ---------------------------------------------------------------------
-- Rel8 entity
-- ---------------------------------------------------------------------

data Player f = Player
  { _playerId                 :: Column f DbPlayerId
  , _playerFirstName          :: Column f Text
  , _playerLastName           :: Column f Text
  , _playerNameSlug           :: Column f Text
  , _playerPosition           :: Column f (Maybe Text)
  , _playerBatSide            :: Column f (Maybe Text)
  , _playerPitchHand          :: Column f (Maybe Text)
  , _playerActive             :: Column f Bool
  , _playerCurrentTeamId      :: Column f (Maybe DbTeamId)
  , _playerLastSyncedProvider :: Column f (Maybe ProviderName)
  , _playerLastSyncedAt       :: Column f (Maybe UTCTime)
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

-- ---------------------------------------------------------------------
-- Public row types
-- ---------------------------------------------------------------------

-- | Write-path row. 'playerRowId' is 'Nothing' on insert (the DB assigns
-- the surrogate id) and 'Just' on update. Reads return 'LoadedPlayerRow'
-- instead, where the id is total.
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

-- | Read-path row. Every row returned by a SELECT against the 'player'
-- table has an id by construction; modeling that totality at the type
-- level removes the partial-function smell at every read site.
data LoadedPlayerRow = LoadedPlayerRow
  { lprId                 :: !DbPlayerId
  , lprFirstName          :: !Text
  , lprLastName           :: !Text
  , lprNameSlug           :: !Text
  , lprPosition           :: !(Maybe Text)
  , lprBatSide            :: !(Maybe Char)
  , lprPitchHand          :: !(Maybe Char)
  , lprActive             :: !Bool
  , lprCurrentTeamId      :: !(Maybe DbTeamId)
  , lprLastSyncedProvider :: !(Maybe ProviderName)
  , lprLastSyncedAt       :: !(Maybe UTCTime)
  }
  deriving stock (Show, Eq)

-- | Project a 'PlayerRow' onto a 'LoadedPlayerRow' given a known id.
-- Used by the in-memory effect interpreter where the id is assigned
-- outside of the SQL roundtrip.
playerRowToLoaded :: DbPlayerId -> PlayerRow -> LoadedPlayerRow
playerRowToLoaded pid r = LoadedPlayerRow
  { lprId                 = pid
  , lprFirstName          = playerRowFirstName r
  , lprLastName           = playerRowLastName r
  , lprNameSlug           = playerRowNameSlug r
  , lprPosition           = playerRowPosition r
  , lprBatSide            = playerRowBatSide r
  , lprPitchHand          = playerRowPitchHand r
  , lprActive             = playerRowActive r
  , lprCurrentTeamId      = playerRowCurrentTeamId r
  , lprLastSyncedProvider = playerRowLastSyncedProvider r
  , lprLastSyncedAt       = playerRowLastSyncedAt r
  }

-- ---------------------------------------------------------------------
-- Schemas
-- ---------------------------------------------------------------------

playerSchema :: TableSchema (Player Name)
playerSchema = TableSchema
  { name    = "player"
  , columns = Player
      { _playerId                 = "id"
      , _playerFirstName          = "first_name"
      , _playerLastName           = "last_name"
      , _playerNameSlug           = "name_slug"
      , _playerPosition           = "position"
      , _playerBatSide            = "bat_side"
      , _playerPitchHand          = "pitch_hand"
      , _playerActive             = "active"
      , _playerCurrentTeamId      = "current_team_id"
      , _playerLastSyncedProvider = "last_synced_provider"
      , _playerLastSyncedAt       = "last_synced_at"
      }
  }

playerExternalIdSchema :: TableSchema (ExternalIdE DbPlayerId Name)
playerExternalIdSchema = TableSchema
  { name    = "player_external_id"
  , columns = ExternalIdE
      { _eidEntityId   = "player_id"
      , _eidProvider   = "provider"
      , _eidExternalId = "external_id"
      , _eidFetchedAt  = "fetched_at"
      }
  }

-- ---------------------------------------------------------------------
-- Char <-> Text helpers
-- ---------------------------------------------------------------------

charToText :: Maybe Char -> Maybe Text
charToText = fmap T.singleton

textToChar :: Maybe Text -> Maybe Char
textToChar = (>>= fmap fst . T.uncons)

-- ---------------------------------------------------------------------
-- Result -> public row (read path)
-- ---------------------------------------------------------------------

fromResult :: Player Result -> LoadedPlayerRow
fromResult Player{..} = LoadedPlayerRow
  { lprId                 = _playerId
  , lprFirstName          = _playerFirstName
  , lprLastName           = _playerLastName
  , lprNameSlug           = _playerNameSlug
  , lprPosition           = _playerPosition
  , lprBatSide            = textToChar _playerBatSide
  , lprPitchHand          = textToChar _playerPitchHand
  , lprActive             = _playerActive
  , lprCurrentTeamId      = _playerCurrentTeamId
  , lprLastSyncedProvider = _playerLastSyncedProvider
  , lprLastSyncedAt       = _playerLastSyncedAt
  }

playerRowToExpr :: PlayerRow -> Player Expr
playerRowToExpr PlayerRow{..} = Player
  { _playerId                 = case playerRowId of
                                  Nothing  -> unsafeDefault
                                  Just pid -> lit pid
  , _playerFirstName          = lit playerRowFirstName
  , _playerLastName           = lit playerRowLastName
  , _playerNameSlug           = lit playerRowNameSlug
  , _playerPosition           = lit playerRowPosition
  , _playerBatSide            = lit (charToText playerRowBatSide)
  , _playerPitchHand          = lit (charToText playerRowPitchHand)
  , _playerActive             = lit playerRowActive
  , _playerCurrentTeamId      = lit playerRowCurrentTeamId
  , _playerLastSyncedProvider = lit playerRowLastSyncedProvider
  , _playerLastSyncedAt       = lit playerRowLastSyncedAt
  }

-- ---------------------------------------------------------------------
-- Insert / Update
-- ---------------------------------------------------------------------

insertPlayerT :: PlayerRow -> Tx.Transaction DbPlayerId
insertPlayerT row =
  Tx.statement () $ run1 $ insert Insert
    { into       = playerSchema
    , rows       = values [playerRowToExpr row]
    , onConflict = Abort
    , returning  = Returning _playerId
    }

updatePlayerT :: DbPlayerId -> PlayerRow -> Tx.Transaction ()
updatePlayerT pid row =
  Tx.statement () $ run_ $ update Update
    { target      = playerSchema
    , from        = pure ()
    , set         = \_ _ -> (playerRowToExpr row) { _playerId = lit pid }
    , updateWhere = \_ p -> _playerId p ==. lit pid
    , returning   = NoReturning
    }

-- ---------------------------------------------------------------------
-- Reads
-- ---------------------------------------------------------------------

getByIdT :: DbPlayerId -> Tx.Transaction (Maybe LoadedPlayerRow)
getByIdT pid = do
  rows <- Tx.statement () $ run $ select $ do
    p <- each playerSchema
    where_ $ _playerId p ==. lit pid
    pure p
  pure $ case rows of
    (r : _) -> Just (fromResult r)
    []      -> Nothing

getAllT :: Tx.Transaction [LoadedPlayerRow]
getAllT = do
  rows <- Tx.statement () $ run $ select $ each playerSchema
  pure (map fromResult rows)

getActiveT :: Tx.Transaction [LoadedPlayerRow]
getActiveT = do
  rows <- Tx.statement () $ run $ select $ do
    p <- each playerSchema
    where_ $ _playerActive p ==. lit True
    pure p
  pure (map fromResult rows)

-- ---------------------------------------------------------------------
-- ProviderKeyed instance
-- ---------------------------------------------------------------------

instance ProviderKeyed PlayerRow where
  type RowEntity PlayerRow = Player
  type RowId     PlayerRow = DbPlayerId

  rowSchema        = playerSchema
  externalIdSchema = playerExternalIdSchema
  rowIdColumn      = _playerId
  insertRowT       = insertPlayerT
  updateRowByIdT   = updatePlayerT

-- ---------------------------------------------------------------------
-- External-id facades
-- ---------------------------------------------------------------------

linkExternalIdT :: DbPlayerId -> ProviderName -> Text -> Tx.Transaction ()
linkExternalIdT = PK.linkExternalIdT @PlayerRow

lookupByExternalIdT :: ProviderName -> Text -> Tx.Transaction (Maybe DbPlayerId)
lookupByExternalIdT = PK.lookupByExternalIdT @PlayerRow

getExternalIdT :: DbPlayerId -> ProviderName -> Tx.Transaction (Maybe Text)
getExternalIdT = PK.getExternalIdT @PlayerRow

upsertByExternalIdT :: ProviderName -> Text -> PlayerRow -> Tx.Transaction DbPlayerId
upsertByExternalIdT = PK.upsertByExternalIdT @PlayerRow

-- ---------------------------------------------------------------------
-- Pool-flavored wrappers
-- ---------------------------------------------------------------------

insertPlayer :: Pool -> PlayerRow -> IO (Either DBError DbPlayerId)
insertPlayer pool row = runTransaction pool (insertPlayerT row)

updatePlayer :: Pool -> DbPlayerId -> PlayerRow -> IO (Either DBError ())
updatePlayer pool pid row = runTransaction pool (updatePlayerT pid row)

getById :: Pool -> DbPlayerId -> IO (Either DBError (Maybe LoadedPlayerRow))
getById pool pid = runTransaction pool (getByIdT pid)

getAll :: Pool -> IO (Either DBError [LoadedPlayerRow])
getAll pool = runTransaction pool getAllT

getActive :: Pool -> IO (Either DBError [LoadedPlayerRow])
getActive pool = runTransaction pool getActiveT

linkExternalId :: Pool -> DbPlayerId -> ProviderName -> Text -> IO (Either DBError ())
linkExternalId = PK.linkExternalId @PlayerRow

lookupByExternalId :: Pool -> ProviderName -> Text -> IO (Either DBError (Maybe DbPlayerId))
lookupByExternalId = PK.lookupByExternalId @PlayerRow

getExternalId :: Pool -> DbPlayerId -> ProviderName -> IO (Either DBError (Maybe Text))
getExternalId = PK.getExternalId @PlayerRow

upsertByExternalId :: Pool -> ProviderName -> Text -> PlayerRow -> IO (Either DBError DbPlayerId)
upsertByExternalId = PK.upsertByExternalId @PlayerRow