{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Pelotero.Lineup.Snapshot
-- Description : Orchestration for taking lineup snapshots before games.
--
-- 'snapshotLineupsForTeam' is the primitive: snapshot one team's
-- current lineup for one game, idempotent on (team, game). If a
-- snapshot already exists for that pair, leave it alone; never
-- overwrite (a snapshot is the lineup AT GAME START, by contract).
--
-- 'snapshotLineupsForGame' is the operator-facing entry point:
-- iterate over all teams in all leagues whose status is "active",
-- and snapshot each. Returns a 'SnapshotResult' summary so the
-- caller can log or surface counts.
module Pelotero.Lineup.Snapshot
  ( PerTeamResult (..)
  , SnapshotResult (..)
  , snapshotLineupsForTeam
  , snapshotLineupsForGame
  ) where

import qualified Data.Text                       as T
import           Effectful

import qualified Pelotero.DB.LeagueConfig        as DBLC
import qualified Pelotero.DB.LeagueTeam          as DBLT
import qualified Pelotero.DB.LineupSlot          as DBLS
import qualified Pelotero.DB.LineupSnapshot      as DBSnap
import           Pelotero.Domain.Id
import qualified Pelotero.Effects.LeagueConfig   as LC
import           Pelotero.Effects.LeagueConfig   (LeagueConfig)
import qualified Pelotero.Effects.LeagueTeam     as LT
import           Pelotero.Effects.LeagueTeam     (LeagueTeam)
import qualified Pelotero.Effects.LineupSlot     as LS
import           Pelotero.Effects.LineupSlot     (LineupSlot)
import qualified Pelotero.Effects.LineupSnapshot as LSnap
import           Pelotero.Effects.LineupSnapshot (LineupSnapshot)
import           Pelotero.Effects.Logging
                     (Logging, Severity (..), logFM)

-- | Per-team outcome of a snapshot attempt for a single (team, game).
data PerTeamResult
  = TeamSnapshotted !Int
    -- ^ n rows submitted to the snapshot table. May overcount under
    --   concurrent snapshot jobs (ON CONFLICT DO NOTHING silently
    --   drops duplicates), but in normal single-operator use n
    --   equals the actual insert count.
  | TeamAlreadyHasSnapshot
    -- ^ a snapshot already existed for this (team, game); nothing changed
  deriving stock (Show, Eq)

-- | Aggregate summary across all teams snapshotted for one game.
data SnapshotResult = SnapshotResult
  { snapTeamsSnapshotted :: !Int  -- ^ teams with a new snapshot written
  , snapTeamsAlreadyDone :: !Int  -- ^ teams whose snapshot already existed
  , snapRowsInserted     :: !Int  -- ^ total rows submitted across all teams
  }
  deriving stock (Show, Eq)

instance Semigroup SnapshotResult where
  a <> b = SnapshotResult
    { snapTeamsSnapshotted = snapTeamsSnapshotted a + snapTeamsSnapshotted b
    , snapTeamsAlreadyDone = snapTeamsAlreadyDone a + snapTeamsAlreadyDone b
    , snapRowsInserted     = snapRowsInserted a     + snapRowsInserted b
    }

instance Monoid SnapshotResult where
  mempty = SnapshotResult 0 0 0

-- | Snapshot one team's current lineup for one game. Idempotent:
-- if a snapshot exists for this (team, game), returns
-- 'TeamAlreadyHasSnapshot' without writing anything.
snapshotLineupsForTeam
  :: ( LineupSlot     :> es
     , LineupSnapshot :> es
     , Logging        :> es
     )
  => DbLeagueTeamId
  -> DbGameId
  -> Eff es PerTeamResult
snapshotLineupsForTeam ltid gid = do
  exists <- LSnap.snapshotExistsForTeamGame ltid gid
  if exists
    then do
      logFM DebugS $ "snapshot exists; skipping team=" <> tshow ltid
                  <> " game=" <> tshow gid
      pure TeamAlreadyHasSnapshot
    else do
      slots <- LS.getSlotsForTeam ltid
      let snapshotRows = map (toSnapshot gid) slots
          n            = length snapshotRows
      LSnap.writeSnapshots snapshotRows
      logFM InfoS $ "snapshotted team=" <> tshow ltid
                 <> " game=" <> tshow gid
                 <> " rows=" <> tshow n
      pure (TeamSnapshotted n)

-- | Snapshot every team in every active league for the given game.
-- A league is "active" iff 'llcStatus' equals @"active"@.
snapshotLineupsForGame
  :: ( LeagueConfig   :> es
     , LeagueTeam     :> es
     , LineupSlot     :> es
     , LineupSnapshot :> es
     , Logging        :> es
     )
  => DbGameId
  -> Eff es SnapshotResult
snapshotLineupsForGame gid = do
  configs <- LC.getAll
  let activeIds = [DBLC.llcId c | c <- configs, DBLC.llcStatus c == "active"]
  perLeague <- traverse (snapshotLeagueTeams gid) activeIds
  let summary = mconcat perLeague
  logFM InfoS $ "snapshot complete game=" <> tshow gid
             <> " teams_new=" <> tshow (snapTeamsSnapshotted summary)
             <> " teams_already=" <> tshow (snapTeamsAlreadyDone summary)
             <> " rows=" <> tshow (snapRowsInserted summary)
  pure summary

snapshotLeagueTeams
  :: ( LeagueTeam     :> es
     , LineupSlot     :> es
     , LineupSnapshot :> es
     , Logging        :> es
     )
  => DbGameId
  -> DbLeagueConfigId
  -> Eff es SnapshotResult
snapshotLeagueTeams gid lcid = do
  teams <- LT.getForLeague lcid
  let teamIds = map DBLT.lltId teams
  results <- traverse (\ltid -> snapshotLineupsForTeam ltid gid) teamIds
  pure (foldr accumulate mempty results)
  where
    accumulate (TeamSnapshotted n) r = r
      { snapTeamsSnapshotted = snapTeamsSnapshotted r + 1
      , snapRowsInserted     = snapRowsInserted r + n
      }
    accumulate TeamAlreadyHasSnapshot r = r
      { snapTeamsAlreadyDone = snapTeamsAlreadyDone r + 1
      }

toSnapshot :: DbGameId -> DBLS.LineupSlotRow -> DBSnap.LineupSnapshotRow
toSnapshot gid s = DBSnap.LineupSnapshotRow
  { DBSnap.lsnapLeagueTeamId = DBLS.lsLeagueTeamId s
  , DBSnap.lsnapGameId       = gid
  , DBSnap.lsnapSlot         = DBLS.lsSlot s
  , DBSnap.lsnapPlayerId     = DBLS.lsPlayerId s
  }

tshow :: Show a => a -> T.Text
tshow = T.pack . show