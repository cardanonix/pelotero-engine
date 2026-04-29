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

module Pelotero.DB.Player
  ( PlayerRow(..)
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

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
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
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.DB.Rel8Instances ()
import Pelotero.Domain.Id    (DbPlayerId(..), DbTeamId(..))

-- ============================================================================
-- player
-- ============================================================================

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

deriving stock instance f ~ Result => Show (Player f)
deriving stock instance f ~ Result => Eq   (Player f)

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

-- ============================================================================
-- player_external_id
-- ============================================================================

data PlayerExternalId f = PlayerExternalId
  { _peidPlayerId   :: Column f DbPlayerId
  , _peidProvider   :: Column f ProviderName
  , _peidExternalId :: Column f Text
  , _peidFetchedAt  :: Column f UTCTime
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

playerExternalIdSchema :: TableSchema (PlayerExternalId Name)
playerExternalIdSchema = TableSchema
  { name    = "player_external_id"
  , columns = PlayerExternalId
      { _peidPlayerId   = "player_id"
      , _peidProvider   = "provider"
      , _peidExternalId = "external_id"
      , _peidFetchedAt  = "fetched_at"
      }
  }

-- ============================================================================
-- Public row type (API compatibility with old hasql module)
-- ============================================================================

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

charToText :: Maybe Char -> Maybe Text
charToText = fmap T.singleton

textToChar :: Maybe Text -> Maybe Char
textToChar = (>>= safeHead)
  where
    safeHead t = case T.uncons t of
      Just (c, _) -> Just c
      Nothing     -> Nothing

fromResult :: Player Result -> PlayerRow
fromResult Player{..} = PlayerRow
  { playerRowId                 = Just _playerId
  , playerRowFirstName          = _playerFirstName
  , playerRowLastName           = _playerLastName
  , playerRowNameSlug           = _playerNameSlug
  , playerRowPosition           = _playerPosition
  , playerRowBatSide            = textToChar _playerBatSide
  , playerRowPitchHand          = textToChar _playerPitchHand
  , playerRowActive             = _playerActive
  , playerRowCurrentTeamId      = _playerCurrentTeamId
  , playerRowLastSyncedProvider = _playerLastSyncedProvider
  , playerRowLastSyncedAt       = _playerLastSyncedAt
  }

-- ============================================================================
-- Transaction-flavored CRUD
-- ============================================================================

insertPlayerT :: PlayerRow -> Tx.Transaction DbPlayerId
insertPlayerT row = Tx.statement () $ R.run1 $ R.insert R.Insert
  { R.into       = playerSchema
  , R.rows       = R.values
      [ Player
          { _playerId                 = R.unsafeDefault
          , _playerFirstName          = R.lit (playerRowFirstName row)
          , _playerLastName           = R.lit (playerRowLastName row)
          , _playerNameSlug           = R.lit (playerRowNameSlug row)
          , _playerPosition           = R.lit (playerRowPosition row)
          , _playerBatSide            = R.lit (charToText (playerRowBatSide row))
          , _playerPitchHand          = R.lit (charToText (playerRowPitchHand row))
          , _playerActive             = R.lit (playerRowActive row)
          , _playerCurrentTeamId      = R.lit (playerRowCurrentTeamId row)
          , _playerLastSyncedProvider = R.lit (playerRowLastSyncedProvider row)
          , _playerLastSyncedAt       = R.lit (playerRowLastSyncedAt row)
          }
      ]
  , R.onConflict = R.Abort
  , R.returning  = R.Returning _playerId
  }

updatePlayerT :: DbPlayerId -> PlayerRow -> Tx.Transaction ()
updatePlayerT pid row = Tx.statement () $ R.run_ $ R.update R.Update
  { R.target      = playerSchema
  , R.from        = pure ()
  , R.set         = \_ p -> p
      { _playerFirstName          = R.lit (playerRowFirstName row)
      , _playerLastName           = R.lit (playerRowLastName row)
      , _playerNameSlug           = R.lit (playerRowNameSlug row)
      , _playerPosition           = R.lit (playerRowPosition row)
      , _playerBatSide            = R.lit (charToText (playerRowBatSide row))
      , _playerPitchHand          = R.lit (charToText (playerRowPitchHand row))
      , _playerActive             = R.lit (playerRowActive row)
      , _playerCurrentTeamId      = R.lit (playerRowCurrentTeamId row)
      , _playerLastSyncedProvider = R.lit (playerRowLastSyncedProvider row)
      , _playerLastSyncedAt       = R.lit (playerRowLastSyncedAt row)
      }
  , R.updateWhere = \_ p -> _playerId p ==. R.lit pid
  , R.returning   = R.NoReturning
  }

getByIdT :: DbPlayerId -> Tx.Transaction (Maybe PlayerRow)
getByIdT pid = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    p <- R.each playerSchema
    R.where_ (_playerId p ==. R.lit pid)
    pure p
  pure $ case rows of
    (p : _) -> Just (fromResult p)
    []      -> Nothing

getAllT :: Tx.Transaction [PlayerRow]
getAllT = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy ((_playerLastName >$< R.asc) <> (_playerFirstName >$< R.asc))
              (R.each playerSchema)
  pure (map fromResult rows)

getActiveT :: Tx.Transaction [PlayerRow]
getActiveT = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy ((_playerLastName >$< R.asc) <> (_playerFirstName >$< R.asc)) $ do
      p <- R.each playerSchema
      R.where_ (_playerActive p)
      pure p
  pure (map fromResult rows)

linkExternalIdT :: DbPlayerId -> ProviderName -> Text -> Tx.Transaction ()
linkExternalIdT pid provider extId = Tx.statement () $ R.run_ $ R.insert R.Insert
  { R.into       = playerExternalIdSchema
  , R.rows       = R.values
      [ PlayerExternalId
          { _peidPlayerId   = R.lit pid
          , _peidProvider   = R.lit provider
          , _peidExternalId = R.lit extId
          , _peidFetchedAt  = R.unsafeDefault
          }
      ]
  , R.onConflict = R.DoNothing
  , R.returning  = R.NoReturning
  }

lookupByExternalIdT :: ProviderName -> Text -> Tx.Transaction (Maybe DbPlayerId)
lookupByExternalIdT provider extId = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    e <- R.each playerExternalIdSchema
    R.where_ (_peidProvider   e ==. R.lit provider)
    R.where_ (_peidExternalId e ==. R.lit extId)
    pure (_peidPlayerId e)
  pure $ case rows of
    (pid : _) -> Just pid
    []        -> Nothing

getExternalIdT :: DbPlayerId -> ProviderName -> Tx.Transaction (Maybe Text)
getExternalIdT pid provider = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    e <- R.each playerExternalIdSchema
    R.where_ (_peidPlayerId e ==. R.lit pid)
    R.where_ (_peidProvider e ==. R.lit provider)
    pure (_peidExternalId e)
  pure $ case rows of
    (extId : _) -> Just extId
    []          -> Nothing

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

-- ============================================================================
-- Pool-flavored CRUD
-- ============================================================================

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