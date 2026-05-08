{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Effectful handler that drives the 'Pelotero.Draft.Machine' state
-- machine and persists its events. Two layers:
--
-- * 'applyAndPersist' is the per-command primitive — used both by
--   'runAutoDraft' and (eventually) by a manual-pick UI.
--
-- * 'runAutoDraft' is the auto-pick loop — drives the state machine
--   from start to finish, picking for whichever team is up by reading
--   their 'PlayerRanking'.
--
-- League lifecycle (transitioning @league_config.status@ from
-- @"draft"@ to @"active"@ on completion) is intentionally NOT done
-- here; that's the orchestrator's job.
module Pelotero.Draft.Run
  ( -- * Errors
    AutoDraftError (..)
    -- * Per-command persistence primitive
  , applyAndPersist
    -- * Auto-draft loop
  , runAutoDraft
  ) where

import           Data.Foldable            (traverse_)
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import           Effectful
import qualified Katip                    as K

import           Pelotero.DB.DraftPick    (DraftPickRow (..))
import           Pelotero.DB.PlayerRanking (PlayerRankingRow (..))
import           Pelotero.Domain.Id
                     ( DbLeagueConfigId
                     , DbLeagueTeamId
                     , DraftPickNumber (..)
                     )
import           Pelotero.Draft
                     ( DraftCommand (..)
                     , DraftContext (..)
                     , DraftError
                     , DraftEvent (..)
                     , DraftPickEntry (..)
                     , DraftPlan (..)
                     , DraftSummary (..)
                     )
import           Pelotero.Draft.Machine
                     ( DraftStateG (..)
                     , DraftVertex (..)
                     , SomeDraftStateG (..)
                     , initialMachineState
                     , runDraftCommand
                     )
import           Pelotero.Effects.Clock         (Clock)
import qualified Pelotero.Effects.Clock         as Clock
import           Pelotero.Effects.DraftPick     (DraftPick)
import qualified Pelotero.Effects.DraftPick     as DP
import           Pelotero.Effects.Logging       (Logging)
import qualified Pelotero.Effects.Logging       as Logging
import           Pelotero.Effects.PlayerRanking (PlayerRanking)
import qualified Pelotero.Effects.PlayerRanking as PR

-- ---------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------

-- | Errors specific to the auto-draft loop. Distinct from
-- 'DraftError' (the pure state-machine rejections) because the loop
-- has its own ways to fail beyond what the state machine cares about.
data AutoDraftError
  = AutoDraftRejected    !DraftError
    -- ^ State machine rejected a command issued by the loop itself —
    --   indicates a bug in this module, not a user-facing problem.
  | AutoDraftNoCandidate !DbLeagueTeamId
    -- ^ Team's ranking is empty AND no player remains in the
    --   available pool. Genuine end-of-draft if it happens before
    --   the pick order runs out.
  | AutoDraftStuck       !DraftVertex
    -- ^ Loop saw the state machine in an unexpected vertex
    --   (e.g. 'WaitingToStartV' after StartDraft was supposed to
    --   succeed). Should be unreachable; included as a defensive
    --   catch.
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------
-- Per-command persistence primitive
-- ---------------------------------------------------------------------

-- | Apply a single command and persist whichever events come out.
-- Rejections are logged at WarningS and the state is unchanged
-- (relies on 'DraftTopology'\'s self-loops).
--
-- The caller threads 'SomeDraftStateG' across calls explicitly — no
-- effect for state holding. Keeps the primitive testable with the
-- existing in-memory effect interpreters.
applyAndPersist
  :: ( DraftPick :> es
     , Clock     :> es
     , Logging   :> es
     )
  => DbLeagueConfigId
  -> SomeDraftStateG
  -> DraftCommand
  -> Eff es (Either DraftError [DraftEvent], SomeDraftStateG)
applyAndPersist league someSt cmd = do
  let (out, someSt') = runDraftCommand someSt cmd
  case out of
    Left err -> do
      Logging.logFM K.WarningS $
        "Draft command rejected: " <> tshow err
      pure (Left err, someSt')
    Right events -> do
      traverse_ (persistEvent league) events
      pure (Right events, someSt')

-- | Persist one event. 'PickRecorded' becomes a 'DP.recordPick' with
-- a server-supplied @picked_at@; 'DraftStarted' and 'DraftCompleted'
-- are log-only.
persistEvent
  :: ( DraftPick :> es
     , Clock     :> es
     , Logging   :> es
     )
  => DbLeagueConfigId
  -> DraftEvent
  -> Eff es ()
persistEvent league = \case
  DraftStarted leagueId teams ->
    Logging.logFM K.InfoS $
      "Draft started for league " <> tshow leagueId
        <> " with " <> tshow (length teams) <> " teams"

  PickRecorded DraftPickEntry{..} -> do
    now <- Clock.now
    let row = DraftPickRow
          { dpId             = Nothing
          , dpLeagueConfigId = league
          , dpPickNumber     = fromIntegral (unDraftPickNumber dpePickNumber)
          , dpLeagueTeamId   = dpeTeam
          , dpPlayerId       = dpePlayer
          , dpPickedAt       = Just now
          }
    _ <- DP.recordPick row
    Logging.logFM K.DebugS $
      "Recorded pick #" <> tshow (unDraftPickNumber dpePickNumber)
        <> " for team " <> tshow dpeTeam

  DraftCompleted summary ->
    Logging.logFM K.InfoS $
      "Draft completed for league " <> tshow (dsLeague summary)
        <> " with " <> tshow (length (dsPicks summary)) <> " picks"

-- ---------------------------------------------------------------------
-- Auto-draft loop
-- ---------------------------------------------------------------------

-- | Drive a draft from 'initialMachineState' to 'CompleteG' via
-- auto-picks. Persists each pick as it happens. Returns the
-- 'DraftSummary' on completion or an 'AutoDraftError' on failure.
--
-- The 'forall es.' is load-bearing: it brings @es@ into scope for the
-- 'driveLoop' helper's own type signature in the @where@ clause, so
-- the effect constraints carry through.
--
-- Single-process, in-memory state. If the loop dies mid-draft the
-- already-inserted 'DraftPickRow's must be cleaned up before retry —
-- 'DP.recordPick' uses Abort-on-conflict and a naive retry will fail
-- at the first already-recorded pick.
runAutoDraft
  :: forall es.
     ( DraftPick     :> es
     , PlayerRanking :> es
     , Clock         :> es
     , Logging       :> es
     )
  => DraftPlan
  -> Eff es (Either AutoDraftError DraftSummary)
runAutoDraft plan = do
  Logging.logFM K.InfoS $
    "Starting auto-draft for league " <> tshow (dpLeague plan)
      <> " with " <> tshow (length (dpOrder plan)) <> " picks"
  (out, st1) <- applyAndPersist
                  (dpLeague plan)
                  initialMachineState
                  (StartDraft plan)
  case out of
    Left err -> pure (Left (AutoDraftRejected err))
    Right _  -> driveLoop st1
  where
    driveLoop
      :: SomeDraftStateG
      -> Eff es (Either AutoDraftError DraftSummary)
    driveLoop someSt@(SomeDraftStateG _ st) = case st of
      WaitingToStartG ->
        pure (Left (AutoDraftStuck WaitingToStartV))
      CompleteG summary ->
        pure (Right summary)
      DraftingG ctx -> do
        autoCmd <- autoPickCommand ctx
        case autoCmd of
          Left err  -> pure (Left err)
          Right cmd -> do
            (out', st') <- applyAndPersist (dpLeague plan) someSt cmd
            case out' of
              Left rejErr -> pure (Left (AutoDraftRejected rejErr))
              Right _     -> driveLoop st'

-- | Pick a player for the team whose turn it currently is. Reads the
-- team's 'PlayerRanking', filters to 'dcAvailable', picks the head;
-- falls back to lowest-id available with a 'WarningS' log line if
-- the ranking is empty or exhausted.
autoPickCommand
  :: ( PlayerRanking :> es
     , Logging       :> es
     )
  => DraftContext
  -> Eff es (Either AutoDraftError DraftCommand)
autoPickCommand ctx = case dcRemaining ctx of
  [] ->
    -- Defensive: a Drafting state with empty remaining shouldn't
    -- reach this branch in normal flow.
    pure (Left (AutoDraftStuck DraftingV))
  (team, _) : _ -> do
    rankings <- PR.getRankingsForTeam team
    let rankedAvail =
          [ prPlayerId pr
          | pr <- rankings
          , Set.member (prPlayerId pr) (dcAvailable ctx)
          ]
    case rankedAvail of
      (pid : _) ->
        pure (Right (MakePick team pid))
      [] -> case Set.lookupMin (dcAvailable ctx) of
        Just pid -> do
          Logging.logFM K.WarningS $
            "No ranked candidate for team " <> tshow team
              <> "; falling back to lowest-id available player"
          pure (Right (MakePick team pid))
        Nothing ->
          pure (Left (AutoDraftNoCandidate team))

-- ---------------------------------------------------------------------
-- Internal helper
-- ---------------------------------------------------------------------

tshow :: Show a => a -> T.Text
tshow = T.pack . show