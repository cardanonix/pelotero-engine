{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

module Pelotero.Effects.LeagueConfig
  ( LeagueConfig(..)
  , insertLeagueConfig
  , updateLeagueConfig
  , getById
  , getByLeagueId
  , runLeagueConfigDB
  ) where

import Data.Text (Text)

import Effectful (Effect, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import qualified Pelotero.DB.LeagueConfig as LCRepo
import           Pelotero.DB.LeagueConfig (LeagueConfigRow)
import           Pelotero.Domain.Id       (DbLeagueConfigId)
import           Pelotero.Effects.Database (Database, runTx)

data LeagueConfig :: Effect where
  InsertLeagueConfig :: LeagueConfigRow                       -> LeagueConfig m DbLeagueConfigId
  UpdateLeagueConfig :: DbLeagueConfigId -> LeagueConfigRow   -> LeagueConfig m ()
  GetById            :: DbLeagueConfigId                      -> LeagueConfig m (Maybe LeagueConfigRow)
  GetByLeagueId      :: Text                                  -> LeagueConfig m (Maybe LeagueConfigRow)

type instance DispatchOf LeagueConfig = 'Dynamic

insertLeagueConfig :: LeagueConfig E.:> es => LeagueConfigRow -> E.Eff es DbLeagueConfigId
insertLeagueConfig = send . InsertLeagueConfig

updateLeagueConfig :: LeagueConfig E.:> es => DbLeagueConfigId -> LeagueConfigRow -> E.Eff es ()
updateLeagueConfig lcid row = send (UpdateLeagueConfig lcid row)

getById :: LeagueConfig E.:> es => DbLeagueConfigId -> E.Eff es (Maybe LeagueConfigRow)
getById = send . GetById

getByLeagueId :: LeagueConfig E.:> es => Text -> E.Eff es (Maybe LeagueConfigRow)
getByLeagueId = send . GetByLeagueId

runLeagueConfigDB
  :: Database E.:> es
  => E.Eff (LeagueConfig : es) a
  -> E.Eff es a
runLeagueConfigDB = interpret_ $ \case
  InsertLeagueConfig row     -> runTx (LCRepo.insertLeagueConfigT row)
  UpdateLeagueConfig lcid r  -> runTx (LCRepo.updateLeagueConfigT lcid r)
  GetById lcid               -> runTx (LCRepo.getByIdT lcid)
  GetByLeagueId lid          -> runTx (LCRepo.getByLeagueIdT lid)