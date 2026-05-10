{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

module Pelotero.Effects.LeagueTeam
  ( LeagueTeam(..)
  , insertLeagueTeam
  , updateLeagueTeam
  , getById
  , lookupByKey
  , getForLeague
  , delete
  , runLeagueTeamDB
  ) where

import Data.Text (Text)

import Effectful (Effect, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import qualified Pelotero.DB.LeagueTeam as LTRepo
import           Pelotero.DB.LeagueTeam (LeagueTeamRow, LoadedLeagueTeam)
import           Pelotero.Domain.Id     (DbLeagueConfigId, DbLeagueTeamId)
import           Pelotero.Effects.Database (Database, runTx)

data LeagueTeam :: Effect where
  InsertLeagueTeam :: LeagueTeamRow                      -> LeagueTeam m DbLeagueTeamId
  UpdateLeagueTeam :: DbLeagueTeamId -> LeagueTeamRow    -> LeagueTeam m ()
  GetById          :: DbLeagueTeamId                     -> LeagueTeam m (Maybe LoadedLeagueTeam)
  LookupByKey      :: DbLeagueConfigId -> Text           -> LeagueTeam m (Maybe LoadedLeagueTeam)
  GetForLeague     :: DbLeagueConfigId                   -> LeagueTeam m [LoadedLeagueTeam]
  Delete           :: DbLeagueTeamId                     -> LeagueTeam m ()

type instance DispatchOf LeagueTeam = 'Dynamic

insertLeagueTeam :: LeagueTeam E.:> es => LeagueTeamRow -> E.Eff es DbLeagueTeamId
insertLeagueTeam = send . InsertLeagueTeam

updateLeagueTeam :: LeagueTeam E.:> es => DbLeagueTeamId -> LeagueTeamRow -> E.Eff es ()
updateLeagueTeam ltid row = send (UpdateLeagueTeam ltid row)

getById :: LeagueTeam E.:> es => DbLeagueTeamId -> E.Eff es (Maybe LoadedLeagueTeam)
getById = send . GetById

lookupByKey
  :: LeagueTeam E.:> es
  => DbLeagueConfigId -> Text -> E.Eff es (Maybe LoadedLeagueTeam)
lookupByKey lcid key = send (LookupByKey lcid key)

getForLeague :: LeagueTeam E.:> es => DbLeagueConfigId -> E.Eff es [LoadedLeagueTeam]
getForLeague = send . GetForLeague

delete :: LeagueTeam E.:> es => DbLeagueTeamId -> E.Eff es ()
delete = send . Delete

runLeagueTeamDB
  :: Database E.:> es
  => E.Eff (LeagueTeam : es) a
  -> E.Eff es a
runLeagueTeamDB = interpret_ $ \case
  InsertLeagueTeam row     -> runTx (LTRepo.insertLeagueTeamT row)
  UpdateLeagueTeam ltid r  -> runTx (LTRepo.updateLeagueTeamT ltid r)
  GetById ltid             -> runTx (LTRepo.getByIdT ltid)
  LookupByKey lcid key     -> runTx (LTRepo.lookupByKeyT lcid key)
  GetForLeague lcid        -> runTx (LTRepo.getForLeagueT lcid)
  Delete ltid              -> runTx (LTRepo.deleteT ltid)