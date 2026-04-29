{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}

module Pelotero.Effects.Players
  ( Players(..)
  , upsertPlayerByExternalId
  , lookupPlayerByExternalId
  , getPlayerById
  , getActivePlayers
  , runPlayersDB
  , runPlayersInMemory
  ) where

import Data.IORef             (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text              (Text)

import Effectful (Effect, IOE, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import Pelotero.DB.Pool      (DBError, runTransaction)
import Pelotero.DB.Player    (PlayerRow(..))
import qualified Pelotero.DB.Player as PlayerRepo
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.Domain.Id    (DbPlayerId(..))
import Pelotero.Effects.DbPool (DbPool, getPool)

--------------------------------------------------------------------------------
-- Capability

data Players :: Effect where
  UpsertPlayerByExternalId :: ProviderName -> Text -> PlayerRow -> Players m DbPlayerId
  LookupPlayerByExternalId :: ProviderName -> Text -> Players m (Maybe DbPlayerId)
  GetPlayerById            :: DbPlayerId -> Players m (Maybe PlayerRow)
  GetActivePlayers         :: Players m [PlayerRow]

type instance DispatchOf Players = 'Dynamic

--------------------------------------------------------------------------------
-- Operations

upsertPlayerByExternalId
  :: Players E.:> es
  => ProviderName -> Text -> PlayerRow -> E.Eff es DbPlayerId
upsertPlayerByExternalId provider extId row =
  send (UpsertPlayerByExternalId provider extId row)

lookupPlayerByExternalId
  :: Players E.:> es
  => ProviderName -> Text -> E.Eff es (Maybe DbPlayerId)
lookupPlayerByExternalId provider extId =
  send (LookupPlayerByExternalId provider extId)

getPlayerById :: Players E.:> es => DbPlayerId -> E.Eff es (Maybe PlayerRow)
getPlayerById = send . GetPlayerById

getActivePlayers :: Players E.:> es => E.Eff es [PlayerRow]
getActivePlayers = send GetActivePlayers

--------------------------------------------------------------------------------
-- DB interpreter

runPlayersDB
  :: (IOE E.:> es, DbPool E.:> es)
  => E.Eff (Players : es) a
  -> E.Eff es a
runPlayersDB = interpret_ $ \case
  UpsertPlayerByExternalId provider extId row -> do
    pool <- getPool
    runOrThrow $ runTransaction pool
      (PlayerRepo.upsertByExternalIdT provider extId row)
  LookupPlayerByExternalId provider extId -> do
    pool <- getPool
    runOrThrow $ runTransaction pool
      (PlayerRepo.lookupByExternalIdT provider extId)
  GetPlayerById pid -> do
    pool <- getPool
    runOrThrow $ runTransaction pool (PlayerRepo.getByIdT pid)
  GetActivePlayers -> do
    pool <- getPool
    runOrThrow $ runTransaction pool PlayerRepo.getActiveT
  where
    runOrThrow :: IOE E.:> es' => IO (Either DBError a) -> E.Eff es' a
    runOrThrow io = E.liftIO io >>= \case
      Right a  -> pure a
      Left err -> E.liftIO (ioError (userError ("DB error: " <> show err)))

--------------------------------------------------------------------------------
-- In-memory interpreter

data PlayerStore = PlayerStore
  { externalIdToDb :: !(Map.Map (ProviderName, Text) DbPlayerId)
  , rowsByDb       :: !(Map.Map DbPlayerId PlayerRow)
  , nextId         :: !Int
  }

emptyStore :: PlayerStore
emptyStore = PlayerStore Map.empty Map.empty 1

runPlayersInMemory
  :: IOE E.:> es
  => E.Eff (Players : es) a
  -> E.Eff es a
runPlayersInMemory action = do
  ref <- E.liftIO (newIORef emptyStore)
  interpret_ (handler ref) action
  where
    handler :: IOE E.:> es => IORef PlayerStore -> Players m b -> E.Eff es b
    handler ref = \case
      UpsertPlayerByExternalId provider extId row ->
        E.liftIO $ atomicModifyIORef' ref (upsertOp provider extId row)
      LookupPlayerByExternalId provider extId -> do
        store <- E.liftIO (readIORef ref)
        pure (Map.lookup (provider, extId) (externalIdToDb store))
      GetPlayerById pid -> do
        store <- E.liftIO (readIORef ref)
        pure (Map.lookup pid (rowsByDb store))
      GetActivePlayers -> do
        store <- E.liftIO (readIORef ref)
        pure $ filter playerRowActive $ map snd $ Map.toList (rowsByDb store)

    upsertOp provider extId incoming store =
      case Map.lookup (provider, extId) (externalIdToDb store) of
        Just pid ->
          let updated = incoming { playerRowId = Just pid }
              store'  = store
                { rowsByDb = Map.insert pid updated (rowsByDb store) }
          in (store', pid)
        Nothing ->
          let pid     = DbPlayerId (fromIntegral (nextId store))
              stored  = incoming { playerRowId = Just pid }
              store'  = PlayerStore
                { externalIdToDb = Map.insert (provider, extId) pid (externalIdToDb store)
                , rowsByDb       = Map.insert pid stored (rowsByDb store)
                , nextId         = nextId store + 1
                }
          in (store', pid)