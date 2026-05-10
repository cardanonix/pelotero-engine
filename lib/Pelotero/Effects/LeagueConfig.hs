{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Pelotero.Effects.LeagueConfig
  ( LeagueConfig
  , insertLeagueConfig
  , updateLeagueConfig
  , getById
  , getByLeagueId
  , getAll
  , runLeagueConfigDB
  ) where

import           Data.Text                  (Text)
import           Effectful
import           Effectful.Dispatch.Dynamic
import qualified Pelotero.DB.LeagueConfig   as LCRepo
import           Pelotero.DB.LeagueConfig   (LeagueConfigRow, LoadedLeagueConfig)
import           Pelotero.Domain.Id
import           Pelotero.Effects.Database

data LeagueConfig :: Effect where
  InsertLeagueConfig :: LeagueConfigRow                     -> LeagueConfig m DbLeagueConfigId
  UpdateLeagueConfig :: DbLeagueConfigId -> LeagueConfigRow -> LeagueConfig m ()
  GetById            :: DbLeagueConfigId                    -> LeagueConfig m (Maybe LoadedLeagueConfig)
  GetByLeagueId      :: Text                                -> LeagueConfig m (Maybe LoadedLeagueConfig)
  GetAll             ::                                        LeagueConfig m [LoadedLeagueConfig]

type instance DispatchOf LeagueConfig = Dynamic

insertLeagueConfig
  :: LeagueConfig :> es
  => LeagueConfigRow
  -> Eff es DbLeagueConfigId
insertLeagueConfig r = send (InsertLeagueConfig r)

updateLeagueConfig
  :: LeagueConfig :> es
  => DbLeagueConfigId
  -> LeagueConfigRow
  -> Eff es ()
updateLeagueConfig lcid r = send (UpdateLeagueConfig lcid r)

getById
  :: LeagueConfig :> es
  => DbLeagueConfigId
  -> Eff es (Maybe LoadedLeagueConfig)
getById lcid = send (GetById lcid)

getByLeagueId
  :: LeagueConfig :> es
  => Text
  -> Eff es (Maybe LoadedLeagueConfig)
getByLeagueId t = send (GetByLeagueId t)

getAll :: LeagueConfig :> es => Eff es [LoadedLeagueConfig]
getAll = send GetAll

runLeagueConfigDB
  :: Database :> es
  => Eff (LeagueConfig : es) a
  -> Eff es a
runLeagueConfigDB = interpret $ \_ -> \case
  InsertLeagueConfig r       -> runTx (LCRepo.insertLeagueConfigT r)
  UpdateLeagueConfig lcid r  -> runTx (LCRepo.updateLeagueConfigT lcid r)
  GetById lcid               -> runTx (LCRepo.getByIdT lcid)
  GetByLeagueId t            -> runTx (LCRepo.getByLeagueIdT t)
  GetAll                     -> runTx LCRepo.getAllT