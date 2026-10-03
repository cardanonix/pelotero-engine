{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

-- |
-- Module      : Pelotero.Effects.DraftPick
-- Description : Draft pick persistence.
--
-- 'recordPickWithSlot' is the operation the draft loop uses. It writes
-- the @draft_pick@ row and the matching @roster_slot@ row in a single
-- database transaction, so a pick is never stored without its roster
-- row and a roster row is never stored without its pick.
module Pelotero.Effects.DraftPick
  ( DraftPick(..)
  , recordPick
  , recordPickWithSlot
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
import qualified Pelotero.DB.RosterSlot as RSRepo
import           Pelotero.DB.RosterSlot (RosterSlotRow)
import           Pelotero.Domain.Id    (DbDraftPickId, DbLeagueConfigId)
import           Pelotero.Effects.Database (Database, runTx)

data DraftPick :: Effect where
  RecordPick         :: DraftPickRow                  -> DraftPick m DbDraftPickId
  RecordPickWithSlot :: DraftPickRow -> RosterSlotRow -> DraftPick m DbDraftPickId
  GetPicksForLeague :: DbLeagueConfigId  -> DraftPick m [DraftPickRow]
  GetPickCount      :: DbLeagueConfigId  -> DraftPick m Int64

type instance DispatchOf DraftPick = 'Dynamic

recordPick :: DraftPick E.:> es => DraftPickRow -> E.Eff es DbDraftPickId
recordPick = send . RecordPick

-- | Record a pick and the roster slot it fills, atomically.
recordPickWithSlot
  :: DraftPick E.:> es
  => DraftPickRow -> RosterSlotRow -> E.Eff es DbDraftPickId
recordPickWithSlot pick slot = send (RecordPickWithSlot pick slot)

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
  RecordPickWithSlot pick slot -> runTx $ do
    pickId <- DPRepo.recordPickT pick
    RSRepo.addSlotT slot
    pure pickId
  GetPicksForLeague lcid -> runTx (DPRepo.getPicksForLeagueT lcid)
  GetPickCount lcid  -> runTx (DPRepo.getPickCountT lcid)