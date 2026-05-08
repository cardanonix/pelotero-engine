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

module Pelotero.DB.Game
  ( Game (..)
  , gameSchema
  , gameExternalIdSchema
  , GameRow (..)
  , insertGameT
  , updateGameT
  , getByIdT
  , getByDateT
  , getByDateRangeT
  , linkExternalIdT
  , lookupByExternalIdT
  , getExternalIdT
  , upsertByExternalIdT
  , insertGame
  , updateGame
  , getById
  , getByDate
  , getByDateRange
  , linkExternalId
  , lookupByExternalId
  , getExternalId
  , upsertByExternalId
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import GHC.Generics (Generic)
import qualified Hasql.Transaction as Tx
import Rel8 hiding (fromResult)

import Pelotero.DB.Pool (DBError, Pool, runTransaction)
import qualified Pelotero.DB.ProviderKeyed as PK
import Pelotero.DB.ProviderKeyed (ExternalIdE (..), ProviderKeyed (..))
import Pelotero.DB.Provider (ProviderName)
import Pelotero.DB.Rel8Instances ()
import Pelotero.Domain.Id (DbGameId, DbTeamId)

-- ---------------------------------------------------------------------
-- Rel8 entity
-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------
-- Public row type
-- ---------------------------------------------------------------------

data GameRow = GameRow
  { gameRowId                 :: !(Maybe DbGameId)
  , gameRowGameDate           :: !Day
  , gameRowAwayTeamId         :: !DbTeamId
  , gameRowHomeTeamId         :: !DbTeamId
  , gameRowLastSyncedProvider :: !(Maybe ProviderName)
  , gameRowLastSyncedAt       :: !(Maybe UTCTime)
  }
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------
-- Schemas
-- ---------------------------------------------------------------------

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

gameExternalIdSchema :: TableSchema (ExternalIdE DbGameId Name)
gameExternalIdSchema = TableSchema
  { name    = "game_external_id"
  , columns = ExternalIdE
      { _eidEntityId   = "game_id"
      , _eidProvider   = "provider"
      , _eidExternalId = "external_id"
      , _eidFetchedAt  = "fetched_at"
      }
  }

-- ---------------------------------------------------------------------
-- Result <-> public row
-- ---------------------------------------------------------------------

fromResult :: Game Result -> GameRow
fromResult Game{..} = GameRow
  { gameRowId                 = Just _gameId
  , gameRowGameDate           = _gameGameDate
  , gameRowAwayTeamId         = _gameAwayTeamId
  , gameRowHomeTeamId         = _gameHomeTeamId
  , gameRowLastSyncedProvider = _gameLastSyncedProvider
  , gameRowLastSyncedAt       = _gameLastSyncedAt
  }

gameRowToExpr :: GameRow -> Game Expr
gameRowToExpr GameRow{..} = Game
  { _gameId                 = case gameRowId of
                                Nothing  -> unsafeDefault
                                Just gid -> lit gid
  , _gameGameDate           = lit gameRowGameDate
  , _gameAwayTeamId         = lit gameRowAwayTeamId
  , _gameHomeTeamId         = lit gameRowHomeTeamId
  , _gameLastSyncedProvider = lit gameRowLastSyncedProvider
  , _gameLastSyncedAt       = lit gameRowLastSyncedAt
  }

-- ---------------------------------------------------------------------
-- Insert / Update
-- ---------------------------------------------------------------------

insertGameT :: GameRow -> Tx.Transaction DbGameId
insertGameT row =
  Tx.statement () $ run1 $ insert Insert
    { into       = gameSchema
    , rows       = values [gameRowToExpr row]
    , onConflict = Abort
    , returning  = Returning _gameId
    }

updateGameT :: DbGameId -> GameRow -> Tx.Transaction ()
updateGameT gid row =
  Tx.statement () $ run_ $ update Update
    { target      = gameSchema
    , from        = pure ()
    , set         = \_ _ -> (gameRowToExpr row) { _gameId = lit gid }
    , updateWhere = \_ g -> _gameId g ==. lit gid
    , returning   = NoReturning
    }

-- ---------------------------------------------------------------------
-- Reads
-- ---------------------------------------------------------------------

getByIdT :: DbGameId -> Tx.Transaction (Maybe GameRow)
getByIdT gid = do
  rows <- Tx.statement () $ run $ select $ do
    g <- each gameSchema
    where_ $ _gameId g ==. lit gid
    pure g
  pure $ case rows of
    (g : _) -> Just (fromResult g)
    []      -> Nothing

getByDateT :: Day -> Tx.Transaction [GameRow]
getByDateT d = do
  rows <- Tx.statement () $ run $ select $
    orderBy (_gameId >$< asc) $ do
      g <- each gameSchema
      where_ $ _gameGameDate g ==. lit d
      pure g
  pure (map fromResult rows)

-- | Inclusive on both ends. Use this when scoring a period rather than
-- iterating 'getByDateT' day-by-day.
getByDateRangeT :: Day -> Day -> Tx.Transaction [GameRow]
getByDateRangeT startDay endDay = do
  rows <- Tx.statement () $ run $ select $
    orderBy ((_gameGameDate >$< asc) <> (_gameId >$< asc)) $ do
      g <- each gameSchema
      where_
        ( _gameGameDate g >=. lit startDay
       &&. _gameGameDate g <=. lit endDay
        )
      pure g
  pure (map fromResult rows)

-- ---------------------------------------------------------------------
-- ProviderKeyed instance
-- ---------------------------------------------------------------------

instance ProviderKeyed GameRow where
  type RowEntity GameRow = Game
  type RowId     GameRow = DbGameId

  rowSchema        = gameSchema
  externalIdSchema = gameExternalIdSchema
  rowIdColumn      = _gameId
  insertRowT       = insertGameT
  updateRowByIdT   = updateGameT

-- ---------------------------------------------------------------------
-- External-id facades
-- ---------------------------------------------------------------------

linkExternalIdT :: DbGameId -> ProviderName -> Text -> Tx.Transaction ()
linkExternalIdT = PK.linkExternalIdT @GameRow

lookupByExternalIdT :: ProviderName -> Text -> Tx.Transaction (Maybe DbGameId)
lookupByExternalIdT = PK.lookupByExternalIdT @GameRow

getExternalIdT :: DbGameId -> ProviderName -> Tx.Transaction (Maybe Text)
getExternalIdT = PK.getExternalIdT @GameRow

upsertByExternalIdT :: ProviderName -> Text -> GameRow -> Tx.Transaction DbGameId
upsertByExternalIdT = PK.upsertByExternalIdT @GameRow

-- ---------------------------------------------------------------------
-- Pool-flavored wrappers
-- ---------------------------------------------------------------------

insertGame :: Pool -> GameRow -> IO (Either DBError DbGameId)
insertGame pool row = runTransaction pool (insertGameT row)

updateGame :: Pool -> DbGameId -> GameRow -> IO (Either DBError ())
updateGame pool gid row = runTransaction pool (updateGameT gid row)

getById :: Pool -> DbGameId -> IO (Either DBError (Maybe GameRow))
getById pool gid = runTransaction pool (getByIdT gid)

getByDate :: Pool -> Day -> IO (Either DBError [GameRow])
getByDate pool d = runTransaction pool (getByDateT d)

getByDateRange :: Pool -> Day -> Day -> IO (Either DBError [GameRow])
getByDateRange pool s e = runTransaction pool (getByDateRangeT s e)

linkExternalId :: Pool -> DbGameId -> ProviderName -> Text -> IO (Either DBError ())
linkExternalId = PK.linkExternalId @GameRow

lookupByExternalId :: Pool -> ProviderName -> Text -> IO (Either DBError (Maybe DbGameId))
lookupByExternalId = PK.lookupByExternalId @GameRow

getExternalId :: Pool -> DbGameId -> ProviderName -> IO (Either DBError (Maybe Text))
getExternalId = PK.getExternalId @GameRow

upsertByExternalId :: Pool -> ProviderName -> Text -> GameRow -> IO (Either DBError DbGameId)
upsertByExternalId = PK.upsertByExternalId @GameRow