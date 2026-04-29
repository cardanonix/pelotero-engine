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

module Pelotero.DB.Game
  ( GameRow(..)
  , insertGameT
  , updateGameT
  , getByIdT
  , getByDateT
  , linkExternalIdT
  , lookupByExternalIdT
  , getExternalIdT
  , upsertByExternalIdT
  , insertGame
  , updateGame
  , getById
  , getByDate
  , linkExternalId
  , lookupByExternalId
  , getExternalId
  , upsertByExternalId
  ) where

import           Data.Functor.Contravariant ((>$<))
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
                                            )
import qualified Rel8                       as R

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.DB.Rel8Instances ()
import Pelotero.Domain.Id    (DbGameId(..), DbTeamId(..))

-- ============================================================================
-- game
-- ============================================================================

data Game f = Game
  { _gameId                 :: Column f DbGameId
  , _gameGameDate           :: Column f Day
  , _gameAwayTeamId         :: Column f DbTeamId
  , _gameHomeTeamId         :: Column f DbTeamId
  , _gameLastSyncedProvider :: Column f (Maybe ProviderName)
  , _gameLastSyncedAt       :: Column f (Maybe UTCTime)
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Game f)
deriving stock instance f ~ Result => Eq   (Game f)

gameSchema :: TableSchema (Game Name)
gameSchema = TableSchema
  { name    = "game"
  , columns = Game
      { _gameId                 = "id"
      , _gameGameDate           = "game_date"
      , _gameAwayTeamId         = "away_team_id"
      , _gameHomeTeamId         = "home_team_id"
      , _gameLastSyncedProvider = "last_synced_provider"
      , _gameLastSyncedAt       = "last_synced_at"
      }
  }

-- ============================================================================
-- game_external_id
-- ============================================================================

data GameExternalId f = GameExternalId
  { _geidGameId     :: Column f DbGameId
  , _geidProvider   :: Column f ProviderName
  , _geidExternalId :: Column f Text
  , _geidFetchedAt  :: Column f UTCTime
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

gameExternalIdSchema :: TableSchema (GameExternalId Name)
gameExternalIdSchema = TableSchema
  { name    = "game_external_id"
  , columns = GameExternalId
      { _geidGameId     = "game_id"
      , _geidProvider   = "provider"
      , _geidExternalId = "external_id"
      , _geidFetchedAt  = "fetched_at"
      }
  }

-- ============================================================================
-- Public row type (API compatibility with old hasql module)
-- ============================================================================

data GameRow = GameRow
  { gameRowId                 :: !(Maybe DbGameId)
  , gameRowGameDate           :: !Day
  , gameRowAwayTeamId         :: !DbTeamId
  , gameRowHomeTeamId         :: !DbTeamId
  , gameRowLastSyncedProvider :: !(Maybe ProviderName)
  , gameRowLastSyncedAt       :: !(Maybe UTCTime)
  }
  deriving stock (Show, Eq)

fromResult :: Game Result -> GameRow
fromResult Game{..} = GameRow
  { gameRowId                 = Just _gameId
  , gameRowGameDate           = _gameGameDate
  , gameRowAwayTeamId         = _gameAwayTeamId
  , gameRowHomeTeamId         = _gameHomeTeamId
  , gameRowLastSyncedProvider = _gameLastSyncedProvider
  , gameRowLastSyncedAt       = _gameLastSyncedAt
  }

-- ============================================================================
-- Transaction-flavored CRUD
-- ============================================================================

insertGameT :: GameRow -> Tx.Transaction DbGameId
insertGameT row = Tx.statement () $ R.run1 $ R.insert R.Insert
  { R.into       = gameSchema
  , R.rows       = R.values
      [ Game
          { _gameId                 = R.unsafeDefault
          , _gameGameDate           = R.lit (gameRowGameDate row)
          , _gameAwayTeamId         = R.lit (gameRowAwayTeamId row)
          , _gameHomeTeamId         = R.lit (gameRowHomeTeamId row)
          , _gameLastSyncedProvider = R.lit (gameRowLastSyncedProvider row)
          , _gameLastSyncedAt       = R.lit (gameRowLastSyncedAt row)
          }
      ]
  , R.onConflict = R.Abort
  , R.returning  = R.Returning _gameId
  }

updateGameT :: DbGameId -> GameRow -> Tx.Transaction ()
updateGameT gid row = Tx.statement () $ R.run_ $ R.update R.Update
  { R.target      = gameSchema
  , R.from        = pure ()
  , R.set         = \_ g -> g
      { _gameGameDate           = R.lit (gameRowGameDate row)
      , _gameAwayTeamId         = R.lit (gameRowAwayTeamId row)
      , _gameHomeTeamId         = R.lit (gameRowHomeTeamId row)
      , _gameLastSyncedProvider = R.lit (gameRowLastSyncedProvider row)
      , _gameLastSyncedAt       = R.lit (gameRowLastSyncedAt row)
      }
  , R.updateWhere = \_ g -> _gameId g ==. R.lit gid
  , R.returning   = R.NoReturning
  }

getByIdT :: DbGameId -> Tx.Transaction (Maybe GameRow)
getByIdT gid = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    g <- R.each gameSchema
    R.where_ (_gameId g ==. R.lit gid)
    pure g
  pure $ case rows of
    (g : _) -> Just (fromResult g)
    []      -> Nothing

getByDateT :: Day -> Tx.Transaction [GameRow]
getByDateT d = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy (_gameId >$< R.asc) $ do
      g <- R.each gameSchema
      R.where_ (_gameGameDate g ==. R.lit d)
      pure g
  pure (map fromResult rows)

linkExternalIdT :: DbGameId -> ProviderName -> Text -> Tx.Transaction ()
linkExternalIdT gid provider extId = Tx.statement () $ R.run_ $ R.insert R.Insert
  { R.into       = gameExternalIdSchema
  , R.rows       = R.values
      [ GameExternalId
          { _geidGameId     = R.lit gid
          , _geidProvider   = R.lit provider
          , _geidExternalId = R.lit extId
          , _geidFetchedAt  = R.unsafeDefault
          }
      ]
  , R.onConflict = R.DoNothing
  , R.returning  = R.NoReturning
  }

lookupByExternalIdT :: ProviderName -> Text -> Tx.Transaction (Maybe DbGameId)
lookupByExternalIdT provider extId = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    e <- R.each gameExternalIdSchema
    R.where_ (_geidProvider   e ==. R.lit provider)
    R.where_ (_geidExternalId e ==. R.lit extId)
    pure (_geidGameId e)
  pure $ case rows of
    (gid : _) -> Just gid
    []        -> Nothing

getExternalIdT :: DbGameId -> ProviderName -> Tx.Transaction (Maybe Text)
getExternalIdT gid provider = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    e <- R.each gameExternalIdSchema
    R.where_ (_geidGameId   e ==. R.lit gid)
    R.where_ (_geidProvider e ==. R.lit provider)
    pure (_geidExternalId e)
  pure $ case rows of
    (extId : _) -> Just extId
    []          -> Nothing

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

-- ============================================================================
-- Pool-flavored CRUD
-- ============================================================================

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

upsertByExternalId
  :: Pool -> ProviderName -> Text -> GameRow -> IO (Either DBError DbGameId)
upsertByExternalId pool provider extId row =
  runTransaction pool (upsertByExternalIdT provider extId row)