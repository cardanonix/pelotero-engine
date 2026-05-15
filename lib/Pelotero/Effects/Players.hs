{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

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

import Pelotero.DB.Player    (PlayerRow, LoadedPlayerRow(..), playerRowToLoaded)
import qualified Pelotero.DB.Player as PlayerRepo
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.Domain.Id    (DbPlayerId(..))
import Pelotero.Effects.Database (Database, runTx)

data Players :: Effect where
  UpsertPlayerByExternalId :: ProviderName -> Text -> PlayerRow -> Players m DbPlayerId
  LookupPlayerByExternalId :: ProviderName -> Text -> Players m (Maybe DbPlayerId)
  GetPlayerById            :: DbPlayerId -> Players m (Maybe LoadedPlayerRow)
  GetActivePlayers         :: Players m [LoadedPlayerRow]

type instance DispatchOf Players = 'Dynamic

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

getPlayerById :: Players E.:> es => DbPlayerId -> E.Eff es (Maybe LoadedPlayerRow)
getPlayerById = send . GetPlayerById

getActivePlayers :: Players E.:> es => E.Eff es [LoadedPlayerRow]
getActivePlayers = send GetActivePlayers

runPlayersDB
  :: Database E.:> es
  => E.Eff (Players : es) a
  -> E.Eff es a
runPlayersDB = interpret_ $ \case
  UpsertPlayerByExternalId provider extId row ->
    runTx (PlayerRepo.upsertByExternalIdT provider extId row)
  LookupPlayerByExternalId provider extId ->
    runTx (PlayerRepo.lookupByExternalIdT provider extId)
  GetPlayerById pid ->
    runTx (PlayerRepo.getByIdT pid)
  GetActivePlayers ->
    runTx PlayerRepo.getActiveT

data PlayerStore = PlayerStore
  { externalIdToDb :: !(Map.Map (ProviderName, Text) DbPlayerId)
  , loadedByDb     :: !(Map.Map DbPlayerId LoadedPlayerRow)
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
        pure (Map.lookup pid (loadedByDb store))
      GetActivePlayers -> do
        store <- E.liftIO (readIORef ref)
        pure $ filter lprActive $ Map.elems (loadedByDb store)

    upsertOp provider extId incoming store =
      case Map.lookup (provider, extId) (externalIdToDb store) of
        Just pid ->
          let updated = playerRowToLoaded pid incoming
              store'  = store
                { loadedByDb = Map.insert pid updated (loadedByDb store) }
          in (store', pid)
        Nothing ->
          let pid     = DbPlayerId (fromIntegral (nextId store))
              stored  = playerRowToLoaded pid incoming
              store'  = PlayerStore
                { externalIdToDb = Map.insert (provider, extId) pid (externalIdToDb store)
                , loadedByDb     = Map.insert pid stored (loadedByDb store)
                , nextId         = nextId store + 1
                }
          in (store', pid)