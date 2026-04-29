{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

module Pelotero.Effects.RosterSlot
  ( RosterSlot(..)
  , addSlot
  , removeSlot
  , clearTeamRoster
  , replaceTeamRoster
  , getSlotsForTeam
  , countBySlot
  , runRosterSlotDB
  ) where

import Data.Int  (Int64)
import Data.Text (Text)

import Effectful (Effect, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import qualified Pelotero.DB.RosterSlot as RSRepo
import           Pelotero.DB.RosterSlot (RosterSlotRow)
import           Pelotero.Domain.Id     (DbLeagueTeamId, DbPlayerId)
import           Pelotero.Effects.Database (Database, runTx)

data RosterSlot :: Effect where
  AddSlot           :: RosterSlotRow                         -> RosterSlot m ()
  RemoveSlot        :: DbLeagueTeamId -> DbPlayerId          -> RosterSlot m ()
  ClearTeamRoster   :: DbLeagueTeamId                        -> RosterSlot m ()
  ReplaceTeamRoster :: DbLeagueTeamId -> [RosterSlotRow]     -> RosterSlot m ()
  GetSlotsForTeam   :: DbLeagueTeamId                        -> RosterSlot m [RosterSlotRow]
  CountBySlot       :: DbLeagueTeamId -> Text                -> RosterSlot m Int64

type instance DispatchOf RosterSlot = 'Dynamic

addSlot :: RosterSlot E.:> es => RosterSlotRow -> E.Eff es ()
addSlot = send . AddSlot

removeSlot
  :: RosterSlot E.:> es
  => DbLeagueTeamId -> DbPlayerId -> E.Eff es ()
removeSlot ltid pid = send (RemoveSlot ltid pid)

clearTeamRoster :: RosterSlot E.:> es => DbLeagueTeamId -> E.Eff es ()
clearTeamRoster = send . ClearTeamRoster

replaceTeamRoster
  :: RosterSlot E.:> es
  => DbLeagueTeamId -> [RosterSlotRow] -> E.Eff es ()
replaceTeamRoster ltid rows = send (ReplaceTeamRoster ltid rows)

getSlotsForTeam :: RosterSlot E.:> es => DbLeagueTeamId -> E.Eff es [RosterSlotRow]
getSlotsForTeam = send . GetSlotsForTeam

countBySlot
  :: RosterSlot E.:> es
  => DbLeagueTeamId -> Text -> E.Eff es Int64
countBySlot ltid slot = send (CountBySlot ltid slot)

runRosterSlotDB
  :: Database E.:> es
  => E.Eff (RosterSlot : es) a
  -> E.Eff es a
runRosterSlotDB = interpret_ $ \case
  AddSlot row              -> runTx (RSRepo.addSlotT row)
  RemoveSlot ltid pid      -> runTx (RSRepo.removeSlotT ltid pid)
  ClearTeamRoster ltid     -> runTx (RSRepo.clearTeamRosterT ltid)
  ReplaceTeamRoster ltid rs -> runTx (RSRepo.replaceTeamRosterT ltid rs)
  GetSlotsForTeam ltid     -> runTx (RSRepo.getSlotsForTeamT ltid)
  CountBySlot ltid slot    -> runTx (RSRepo.countBySlotT ltid slot)