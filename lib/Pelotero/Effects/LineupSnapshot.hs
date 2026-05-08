{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Pelotero.Effects.LineupSnapshot
  ( LineupSnapshot
  , writeSnapshots
  , getSnapshotForTeamGame
  , snapshotExistsForTeamGame
  , getSnapshotsForGame
  , runLineupSnapshotDB
  ) where

import           Effectful
import           Effectful.Dispatch.Dynamic
import qualified Pelotero.DB.LineupSnapshot as Repo
import           Pelotero.DB.LineupSnapshot (LineupSnapshotRow)
import           Pelotero.Domain.Id
import           Pelotero.Effects.Database

data LineupSnapshot :: Effect where
  WriteSnapshots            :: [LineupSnapshotRow] -> LineupSnapshot m ()
  GetSnapshotForTeamGame    :: DbLeagueTeamId -> DbGameId -> LineupSnapshot m [LineupSnapshotRow]
  SnapshotExistsForTeamGame :: DbLeagueTeamId -> DbGameId -> LineupSnapshot m Bool
  GetSnapshotsForGame       :: DbGameId -> LineupSnapshot m [LineupSnapshotRow]

type instance DispatchOf LineupSnapshot = Dynamic

writeSnapshots :: LineupSnapshot :> es => [LineupSnapshotRow] -> Eff es ()
writeSnapshots rs = send (WriteSnapshots rs)

getSnapshotForTeamGame
  :: LineupSnapshot :> es
  => DbLeagueTeamId -> DbGameId -> Eff es [LineupSnapshotRow]
getSnapshotForTeamGame ltid gid = send (GetSnapshotForTeamGame ltid gid)

snapshotExistsForTeamGame
  :: LineupSnapshot :> es
  => DbLeagueTeamId -> DbGameId -> Eff es Bool
snapshotExistsForTeamGame ltid gid = send (SnapshotExistsForTeamGame ltid gid)

getSnapshotsForGame :: LineupSnapshot :> es => DbGameId -> Eff es [LineupSnapshotRow]
getSnapshotsForGame gid = send (GetSnapshotsForGame gid)

runLineupSnapshotDB
  :: Database :> es
  => Eff (LineupSnapshot : es) a
  -> Eff es a
runLineupSnapshotDB = interpret $ \_ -> \case
  WriteSnapshots rs              -> runTx (Repo.writeSnapshotsT rs)
  GetSnapshotForTeamGame l g     -> runTx (Repo.getSnapshotForTeamGameT l g)
  SnapshotExistsForTeamGame l g  -> runTx (Repo.snapshotExistsForTeamGameT l g)
  GetSnapshotsForGame g          -> runTx (Repo.getSnapshotsForGameT g)