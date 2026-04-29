{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

module Pelotero.Effects.LineupSlot
  ( LineupSlot(..)
  , addSlot
  , removeSlot
  , clearTeamLineup
  , replaceTeamLineup
  , getSlotsForTeam
  , runLineupSlotDB
  ) where

import Effectful (Effect, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

import qualified Pelotero.DB.LineupSlot as LSRepo
import           Pelotero.DB.LineupSlot (LineupSlotRow)
import           Pelotero.Domain.Id     (DbLeagueTeamId, DbPlayerId)
import           Pelotero.Effects.Database (Database, runTx)

data LineupSlot :: Effect where
  AddSlot           :: LineupSlotRow                         -> LineupSlot m ()
  RemoveSlot        :: DbLeagueTeamId -> DbPlayerId          -> LineupSlot m ()
  ClearTeamLineup   :: DbLeagueTeamId                        -> LineupSlot m ()
  ReplaceTeamLineup :: DbLeagueTeamId -> [LineupSlotRow]     -> LineupSlot m ()
  GetSlotsForTeam   :: DbLeagueTeamId                        -> LineupSlot m [LineupSlotRow]

type instance DispatchOf LineupSlot = 'Dynamic

addSlot :: LineupSlot E.:> es => LineupSlotRow -> E.Eff es ()
addSlot = send . AddSlot

removeSlot
  :: LineupSlot E.:> es
  => DbLeagueTeamId -> DbPlayerId -> E.Eff es ()
removeSlot ltid pid = send (RemoveSlot ltid pid)

clearTeamLineup :: LineupSlot E.:> es => DbLeagueTeamId -> E.Eff es ()
clearTeamLineup = send . ClearTeamLineup

replaceTeamLineup
  :: LineupSlot E.:> es
  => DbLeagueTeamId -> [LineupSlotRow] -> E.Eff es ()
replaceTeamLineup ltid rows = send (ReplaceTeamLineup ltid rows)

getSlotsForTeam :: LineupSlot E.:> es => DbLeagueTeamId -> E.Eff es [LineupSlotRow]
getSlotsForTeam = send . GetSlotsForTeam

runLineupSlotDB
  :: Database E.:> es
  => E.Eff (LineupSlot : es) a
  -> E.Eff es a
runLineupSlotDB = interpret_ $ \case
  AddSlot row               -> runTx (LSRepo.addSlotT row)
  RemoveSlot ltid pid       -> runTx (LSRepo.removeSlotT ltid pid)
  ClearTeamLineup ltid      -> runTx (LSRepo.clearTeamLineupT ltid)
  ReplaceTeamLineup ltid rs -> runTx (LSRepo.replaceTeamLineupT ltid rs)
  GetSlotsForTeam ltid      -> runTx (LSRepo.getSlotsForTeamT ltid)