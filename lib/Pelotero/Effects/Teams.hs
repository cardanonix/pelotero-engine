{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}

module Pelotero.Effects.Teams
  ( Teams(..)
  , upsertTeamByExternalId
  , lookupTeamByExternalId
  , getTeamById
  , getAllTeams
  , runTeamsDB
  , runTeamsInMemory
  ) where

import Data.IORef             (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text              (Text)

import Effectful (Effect, IOE, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import Pelotero.DB.Pool      (DBError, runTransaction)
import Pelotero.DB.Team      (TeamRow(..))
import qualified Pelotero.DB.Team as TeamRepo
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.Domain.Id    (DbTeamId(..))
import Pelotero.Effects.DbPool (DbPool, getPool)

data Teams :: Effect where
  UpsertTeamByExternalId :: ProviderName -> Text -> TeamRow -> Teams m DbTeamId
  LookupTeamByExternalId :: ProviderName -> Text -> Teams m (Maybe DbTeamId)
  GetTeamById            :: DbTeamId -> Teams m (Maybe TeamRow)
  GetAllTeams            :: Teams m [TeamRow]

type instance DispatchOf Teams = 'Dynamic

upsertTeamByExternalId
  :: Teams E.:> es => ProviderName -> Text -> TeamRow -> E.Eff es DbTeamId
upsertTeamByExternalId provider extId row =
  send (UpsertTeamByExternalId provider extId row)

lookupTeamByExternalId
  :: Teams E.:> es => ProviderName -> Text -> E.Eff es (Maybe DbTeamId)
lookupTeamByExternalId provider extId =
  send (LookupTeamByExternalId provider extId)

getTeamById :: Teams E.:> es => DbTeamId -> E.Eff es (Maybe TeamRow)
getTeamById = send . GetTeamById

getAllTeams :: Teams E.:> es => E.Eff es [TeamRow]
getAllTeams = send GetAllTeams

runTeamsDB
  :: (IOE E.:> es, DbPool E.:> es)
  => E.Eff (Teams : es) a
  -> E.Eff es a
runTeamsDB = interpret_ $ \case
  UpsertTeamByExternalId provider extId row -> do
    pool <- getPool
    runOrThrow $ runTransaction pool
      (TeamRepo.upsertByExternalIdT provider extId row)
  LookupTeamByExternalId provider extId -> do
    pool <- getPool
    runOrThrow $ runTransaction pool
      (TeamRepo.lookupByExternalIdT provider extId)
  GetTeamById tid -> do
    pool <- getPool
    runOrThrow $ runTransaction pool (TeamRepo.getByIdT tid)
  GetAllTeams -> do
    pool <- getPool
    runOrThrow $ runTransaction pool TeamRepo.getAllT
  where
    runOrThrow :: IOE E.:> es' => IO (Either DBError a) -> E.Eff es' a
    runOrThrow io = E.liftIO io >>= \case
      Right a  -> pure a
      Left err -> E.liftIO (ioError (userError ("DB error: " <> show err)))

data TeamStore = TeamStore
  { teamExternalIdToDb :: !(Map.Map (ProviderName, Text) DbTeamId)
  , teamRowsByDb       :: !(Map.Map DbTeamId TeamRow)
  , teamNextId         :: !Int
  }

emptyTeamStore :: TeamStore
emptyTeamStore = TeamStore Map.empty Map.empty 1

runTeamsInMemory
  :: IOE E.:> es
  => E.Eff (Teams : es) a
  -> E.Eff es a
runTeamsInMemory action = do
  ref <- E.liftIO (newIORef emptyTeamStore)
  interpret_ (handler ref) action
  where
    handler :: IOE E.:> es => IORef TeamStore -> Teams m b -> E.Eff es b
    handler ref = \case
      UpsertTeamByExternalId provider extId row ->
        E.liftIO $ atomicModifyIORef' ref (upsertOp provider extId row)
      LookupTeamByExternalId provider extId -> do
        store <- E.liftIO (readIORef ref)
        pure (Map.lookup (provider, extId) (teamExternalIdToDb store))
      GetTeamById tid -> do
        store <- E.liftIO (readIORef ref)
        pure (Map.lookup tid (teamRowsByDb store))
      GetAllTeams -> do
        store <- E.liftIO (readIORef ref)
        pure (map snd (Map.toList (teamRowsByDb store)))

    upsertOp provider extId incoming store =
      case Map.lookup (provider, extId) (teamExternalIdToDb store) of
        Just tid ->
          let updated = incoming { teamRowId = Just tid }
              store'  = store
                { teamRowsByDb = Map.insert tid updated (teamRowsByDb store) }
          in (store', tid)
        Nothing ->
          let tid    = DbTeamId (fromIntegral (teamNextId store))
              stored = incoming { teamRowId = Just tid }
              store' = TeamStore
                { teamExternalIdToDb =
                    Map.insert (provider, extId) tid (teamExternalIdToDb store)
                , teamRowsByDb = Map.insert tid stored (teamRowsByDb store)
                , teamNextId   = teamNextId store + 1
                }
          in (store', tid)