{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}

-- | The 'Games' capability. Used by schedule sync and (soon) boxscore sync.
-- 'GetExternalId' lets the boxscore fetcher go from a DB game back to the
-- upstream identifier needed to construct the boxscore URL.
module Pelotero.Effects.Games
  ( Games(..)
  , upsertGameByExternalId
  , lookupGameByExternalId
  , getGameExternalId
  , getGameById
  , getGamesByDate
  , runGamesDB
  , runGamesInMemory
  ) where

import Data.IORef             (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text              (Text)
import Data.Time.Calendar     (Day)

import Effectful (Effect, IOE, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import Pelotero.DB.Pool      (DBError, runTransaction)
import Pelotero.DB.Game      (GameRow(..))
import qualified Pelotero.DB.Game as GameRepo
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.Domain.Id    (DbGameId(..))
import Pelotero.Effects.DbPool (DbPool, getPool)

--------------------------------------------------------------------------------
-- Capability

data Games :: Effect where
  UpsertGameByExternalId :: ProviderName -> Text -> GameRow -> Games m DbGameId
  LookupGameByExternalId :: ProviderName -> Text -> Games m (Maybe DbGameId)
  GetGameExternalId      :: DbGameId -> ProviderName -> Games m (Maybe Text)
  GetGameById            :: DbGameId -> Games m (Maybe GameRow)
  GetGamesByDate         :: Day -> Games m [GameRow]

type instance DispatchOf Games = 'Dynamic

--------------------------------------------------------------------------------
-- Operations

upsertGameByExternalId
  :: Games E.:> es => ProviderName -> Text -> GameRow -> E.Eff es DbGameId
upsertGameByExternalId provider extId row =
  send (UpsertGameByExternalId provider extId row)

lookupGameByExternalId
  :: Games E.:> es => ProviderName -> Text -> E.Eff es (Maybe DbGameId)
lookupGameByExternalId provider extId =
  send (LookupGameByExternalId provider extId)

-- | Retrieve the upstream identifier for a game. Used by boxscore sync to
-- construct the fetch URL from a DB game row.
getGameExternalId
  :: Games E.:> es => DbGameId -> ProviderName -> E.Eff es (Maybe Text)
getGameExternalId gid provider = send (GetGameExternalId gid provider)

getGameById :: Games E.:> es => DbGameId -> E.Eff es (Maybe GameRow)
getGameById = send . GetGameById

getGamesByDate :: Games E.:> es => Day -> E.Eff es [GameRow]
getGamesByDate = send . GetGamesByDate

--------------------------------------------------------------------------------
-- DB interpreter

runGamesDB
  :: (IOE E.:> es, DbPool E.:> es)
  => E.Eff (Games : es) a
  -> E.Eff es a
runGamesDB = interpret_ $ \case
  UpsertGameByExternalId provider extId row -> do
    pool <- getPool
    runOrThrow $ runTransaction pool
      (GameRepo.upsertByExternalIdT provider extId row)
  LookupGameByExternalId provider extId -> do
    pool <- getPool
    runOrThrow $ runTransaction pool
      (GameRepo.lookupByExternalIdT provider extId)
  GetGameExternalId gid provider -> do
    pool <- getPool
    runOrThrow $ runTransaction pool
      (GameRepo.getExternalIdT gid provider)
  GetGameById gid -> do
    pool <- getPool
    runOrThrow $ runTransaction pool (GameRepo.getByIdT gid)
  GetGamesByDate day -> do
    pool <- getPool
    runOrThrow $ runTransaction pool (GameRepo.getByDateT day)
  where
    runOrThrow :: IOE E.:> es' => IO (Either DBError a) -> E.Eff es' a
    runOrThrow io = E.liftIO io >>= \case
      Right a  -> pure a
      Left err -> E.liftIO (ioError (userError ("DB error: " <> show err)))

--------------------------------------------------------------------------------
-- In-memory interpreter

data GameStore = GameStore
  { gameExternalIdToDb :: !(Map.Map (ProviderName, Text) DbGameId)
  , gameDbToExternalId :: !(Map.Map (DbGameId, ProviderName) Text)
  , gameRowsByDb       :: !(Map.Map DbGameId GameRow)
  , gameNextId         :: !Int
  }

emptyGameStore :: GameStore
emptyGameStore = GameStore Map.empty Map.empty Map.empty 1

runGamesInMemory
  :: IOE E.:> es
  => E.Eff (Games : es) a
  -> E.Eff es a
runGamesInMemory action = do
  ref <- E.liftIO (newIORef emptyGameStore)
  interpret_ (handler ref) action
  where
    handler :: IOE E.:> es => IORef GameStore -> Games m b -> E.Eff es b
    handler ref = \case
      UpsertGameByExternalId provider extId row ->
        E.liftIO $ atomicModifyIORef' ref (upsertOp provider extId row)
      LookupGameByExternalId provider extId -> do
        store <- E.liftIO (readIORef ref)
        pure (Map.lookup (provider, extId) (gameExternalIdToDb store))
      GetGameExternalId gid provider -> do
        store <- E.liftIO (readIORef ref)
        pure (Map.lookup (gid, provider) (gameDbToExternalId store))
      GetGameById gid -> do
        store <- E.liftIO (readIORef ref)
        pure (Map.lookup gid (gameRowsByDb store))
      GetGamesByDate day -> do
        store <- E.liftIO (readIORef ref)
        pure [ r | r <- Map.elems (gameRowsByDb store)
                 , gameRowGameDate r == day ]

    upsertOp provider extId incoming store =
      case Map.lookup (provider, extId) (gameExternalIdToDb store) of
        Just gid ->
          let updated = incoming { gameRowId = Just gid }
              store'  = store
                { gameRowsByDb = Map.insert gid updated (gameRowsByDb store) }
          in (store', gid)
        Nothing ->
          let gid    = DbGameId (fromIntegral (gameNextId store))
              stored = incoming { gameRowId = Just gid }
              store' = store
                { gameExternalIdToDb =
                    Map.insert (provider, extId) gid (gameExternalIdToDb store)
                , gameDbToExternalId =
                    Map.insert (gid, provider) extId (gameDbToExternalId store)
                , gameRowsByDb = Map.insert gid stored (gameRowsByDb store)
                , gameNextId   = gameNextId store + 1
                }
          in (store', gid)