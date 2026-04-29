{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

module Pelotero.Effects.DraftPick
  ( DraftPick(..)
  , recordPick
  , getPicksForLeague
  , getPickCount
  , runDraftPickDB
  ) where

import Data.Int (Int64)

import Effectful (Effect, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import qualified Pelotero.DB.DraftPick as DPRepo
import           Pelotero.DB.DraftPick (DraftPickRow)
import           Pelotero.Domain.Id    (DbDraftPickId, DbLeagueConfigId)
import           Pelotero.Effects.Database (Database, runTx)

data DraftPick :: Effect where
  RecordPick        :: DraftPickRow      -> DraftPick m DbDraftPickId
  GetPicksForLeague :: DbLeagueConfigId  -> DraftPick m [DraftPickRow]
  GetPickCount      :: DbLeagueConfigId  -> DraftPick m Int64

type instance DispatchOf DraftPick = 'Dynamic

recordPick :: DraftPick E.:> es => DraftPickRow -> E.Eff es DbDraftPickId
recordPick = send . RecordPick

getPicksForLeague
  :: DraftPick E.:> es
  => DbLeagueConfigId -> E.Eff es [DraftPickRow]
getPicksForLeague = send . GetPicksForLeague

getPickCount
  :: DraftPick E.:> es
  => DbLeagueConfigId -> E.Eff es Int64
getPickCount = send . GetPickCount

runDraftPickDB
  :: Database E.:> es
  => E.Eff (DraftPick : es) a
  -> E.Eff es a
runDraftPickDB = interpret_ $ \case
  RecordPick row     -> runTx (DPRepo.recordPickT row)
  GetPicksForLeague lcid -> runTx (DPRepo.getPicksForLeagueT lcid)
  GetPickCount lcid  -> runTx (DPRepo.getPickCountT lcid)