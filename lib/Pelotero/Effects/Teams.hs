{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

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

import Pelotero.DB.Team      (LoadedTeamRow, TeamRow, teamRowToLoaded)
import qualified Pelotero.DB.Team as TeamRepo
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.Domain.Id    (DbTeamId(..))
import Pelotero.Effects.Database (Database, runTx)

data Teams :: Effect where
  UpsertTeamByExternalId :: ProviderName -> Text -> TeamRow -> Teams m DbTeamId
  LookupTeamByExternalId :: ProviderName -> Text -> Teams m (Maybe DbTeamId)
  GetTeamById            :: DbTeamId -> Teams m (Maybe LoadedTeamRow)
  GetAllTeams            :: Teams m [LoadedTeamRow]

type instance DispatchOf Teams = 'Dynamic

upsertTeamByExternalId
  :: Teams E.:> es => ProviderName -> Text -> TeamRow -> E.Eff es DbTeamId
upsertTeamByExternalId provider extId row =
  send (UpsertTeamByExternalId provider extId row)

lookupTeamByExternalId
  :: Teams E.:> es => ProviderName -> Text -> E.Eff es (Maybe DbTeamId)
lookupTeamByExternalId provider extId =
  send (LookupTeamByExternalId provider extId)

getTeamById :: Teams E.:> es => DbTeamId -> E.Eff es (Maybe LoadedTeamRow)
getTeamById = send . GetTeamById

getAllTeams :: Teams E.:> es => E.Eff es [LoadedTeamRow]
getAllTeams = send GetAllTeams

runTeamsDB
  :: Database E.:> es
  => E.Eff (Teams : es) a
  -> E.Eff es a
runTeamsDB = interpret_ $ \case
  UpsertTeamByExternalId provider extId row ->
    runTx (TeamRepo.upsertByExternalIdT provider extId row)
  LookupTeamByExternalId provider extId ->
    runTx (TeamRepo.lookupByExternalIdT provider extId)
  GetTeamById tid ->
    runTx (TeamRepo.getByIdT tid)
  GetAllTeams ->
    runTx TeamRepo.getAllT

data TeamStore = TeamStore
  { teamExternalIdToDb :: !(Map.Map (ProviderName, Text) DbTeamId)
  , teamLoadedByDb     :: !(Map.Map DbTeamId LoadedTeamRow)
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
        pure (Map.lookup tid (teamLoadedByDb store))
      GetAllTeams -> do
        store <- E.liftIO (readIORef ref)
        pure (map snd (Map.toList (teamLoadedByDb store)))

    upsertOp provider extId incoming store =
      case Map.lookup (provider, extId) (teamExternalIdToDb store) of
        Just tid ->
          let updated = teamRowToLoaded tid incoming
              store'  = store
                { teamLoadedByDb = Map.insert tid updated (teamLoadedByDb store) }
          in (store', tid)
        Nothing ->
          let tid    = DbTeamId (fromIntegral (teamNextId store))
              stored = teamRowToLoaded tid incoming
              store' = TeamStore
                { teamExternalIdToDb =
                    Map.insert (provider, extId) tid (teamExternalIdToDb store)
                , teamLoadedByDb = Map.insert tid stored (teamLoadedByDb store)
                , teamNextId   = teamNextId store + 1
                }
          in (store', tid)