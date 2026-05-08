{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The pure draft state machine. Tracks the lifecycle of a fantasy
-- draft from waiting-to-start through pick-by-pick drafting to a
-- final summary, without any effect dependencies.
--
-- This module owns the transition logic; 'Pelotero.Draft.Run' (Phase
-- D.1c) is the effectful handler that wraps it with database I/O.
-- 'Pelotero.Domain.Draft' is the upstream-id-keyed kernel that
-- generates the pick order from a strategy + team list and is shared
-- between this module's tests and any future analyses that don't need
-- DB ids.
module Pelotero.Draft
  ( -- * State
    DraftState (..)
  , DraftContext (..)
  , DraftPickEntry (..)
  , DraftSummary (..)
  , initialState
    -- * Plan, commands, events, errors
  , DraftPlan (..)
  , DraftCommand (..)
  , DraftEvent (..)
  , DraftError (..)
    -- * Transition
  , applyCommand
    -- * Inspection helpers
  , currentPicker
  , picksRemaining
  , isPlayerAvailable
  ) where

import           Data.List (nub)
import           Data.Set  (Set)
import qualified Data.Set  as Set

import           Pelotero.Domain.Id
                     ( DbLeagueConfigId
                     , DbLeagueTeamId
                     , DbPlayerId
                     , DraftPickNumber
                     )

-- ---------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------

-- | The state machine has three phases: pre-start, mid-draft, and
-- done. 'WaitingToStart' carries no data — the plan arrives with the
-- 'StartDraft' command. 'Drafting' carries the working context.
-- 'Complete' carries the final summary.
data DraftState
  = WaitingToStart
  | Drafting !DraftContext
  | Complete !DraftSummary
  deriving stock (Show, Eq)

-- | Working context maintained during the 'Drafting' phase.
--
-- 'dcOrder' is the full pick-by-pick order computed at draft start
-- time, kept around so a 'DraftSummary' or any audit trail can
-- reconstruct it. 'dcRemaining' is the suffix of 'dcOrder' that
-- hasn't been picked yet — its head is whose turn it is.
--
-- 'dcAvailable' is the set of players still in the pool. Membership
-- is the authoritative check for "can this player be picked?";
-- 'dcPicksMade' is the audit trail. Together they enforce that no
-- player is picked twice.
--
-- 'dcPicksMade' is in pick order, oldest first (snoc-appended).
data DraftContext = DraftContext
  { dcLeague    :: !DbLeagueConfigId
  , dcOrder     :: ![(DbLeagueTeamId, DraftPickNumber)]
  , dcRemaining :: ![(DbLeagueTeamId, DraftPickNumber)]
  , dcAvailable :: !(Set DbPlayerId)
  , dcPicksMade :: ![DraftPickEntry]
  }
  deriving stock (Show, Eq)

-- | A single recorded pick within the in-memory state. Persistence
-- to 'Pelotero.DB.DraftPick' (with @picked_at@) happens in the
-- effectful handler (D.1c).
data DraftPickEntry = DraftPickEntry
  { dpePickNumber :: !DraftPickNumber
  , dpeTeam       :: !DbLeagueTeamId
  , dpePlayer     :: !DbPlayerId
  }
  deriving stock (Show, Eq)

-- | Final summary produced when the last pick is made or 'EndDraft'
-- is invoked. Picks are in pick order, oldest first.
data DraftSummary = DraftSummary
  { dsLeague :: !DbLeagueConfigId
  , dsPicks  :: ![DraftPickEntry]
  }
  deriving stock (Show, Eq)

initialState :: DraftState
initialState = WaitingToStart

-- ---------------------------------------------------------------------
-- Plan, commands, events, errors
-- ---------------------------------------------------------------------

-- | Inputs needed to start a draft. Built externally and passed in
-- via 'StartDraft'.
data DraftPlan = DraftPlan
  { dpLeague    :: !DbLeagueConfigId
  , dpOrder     :: ![(DbLeagueTeamId, DraftPickNumber)]
  , dpAvailable :: !(Set DbPlayerId)
  }
  deriving stock (Show, Eq)

-- | The state machine's input alphabet. Notably 'AutoPick' is not
-- here — auto-picking is a wrapper at the effect layer (D.1c) that
-- resolves a team's ranking against 'dcAvailable' and issues a
-- 'MakePick' with the chosen player. Keeps the pure logic with one
-- place-a-player path rather than two.
data DraftCommand
  = StartDraft !DraftPlan
  | MakePick   !DbLeagueTeamId !DbPlayerId
  | EndDraft
  deriving stock (Show, Eq)

-- | Events emitted by successful transitions. The effect handler
-- writes these out: 'PickRecorded' becomes a 'DraftPickRow' insert,
-- 'DraftCompleted' becomes a league_config status update.
--
-- The last 'MakePick' of a draft emits both 'PickRecorded' and
-- 'DraftCompleted', in that order. The handler must persist them in
-- that order so a crash between events leaves the pick recorded with
-- the league still in @drafting@ status — the recoverable state.
data DraftEvent
  = DraftStarted   !DbLeagueConfigId ![DbLeagueTeamId]
  | PickRecorded   !DraftPickEntry
  | DraftCompleted !DraftSummary
  deriving stock (Show, Eq)

-- | All the things 'applyCommand' can refuse to do.
data DraftError
  = DraftNotStarted
  | DraftAlreadyStarted
  | DraftAlreadyComplete
  | EmptyDraftPlan
  | NotYourTurn          !DbLeagueTeamId !DbLeagueTeamId
    -- ^ expected, actual
  | PlayerAlreadyDrafted !DbPlayerId
  | PlayerNotInPool      !DbPlayerId
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------
-- Transition
-- ---------------------------------------------------------------------

-- | The transition function. Pure; no effects. Returns the new state
-- plus events that the handler should persist.
--
-- Errors short-circuit without changing state. 'NotYourTurn' carries
-- both expected and actual team so the caller can render a helpful
-- message; the same applies to player-not-pickable errors.
applyCommand
  :: DraftCommand
  -> DraftState
  -> Either DraftError (DraftState, [DraftEvent])
applyCommand cmd state = case (cmd, state) of

  -- StartDraft: only legal from WaitingToStart, only with a non-empty
  -- pick order.
  (StartDraft plan, WaitingToStart)
    | null (dpOrder plan) -> Left EmptyDraftPlan
    | otherwise ->
        let teams = nub (map fst (dpOrder plan))
            ctx   = DraftContext
              { dcLeague    = dpLeague plan
              , dcOrder     = dpOrder plan
              , dcRemaining = dpOrder plan
              , dcAvailable = dpAvailable plan
              , dcPicksMade = []
              }
        in Right (Drafting ctx, [DraftStarted (dpLeague plan) teams])
  (StartDraft _, Drafting _) -> Left DraftAlreadyStarted
  (StartDraft _, Complete _) -> Left DraftAlreadyComplete

  -- MakePick: only legal from Drafting, only by the team whose turn
  -- it is, only on an available player.
  (MakePick _ _, WaitingToStart) -> Left DraftNotStarted
  (MakePick _ _, Complete _)     -> Left DraftAlreadyComplete
  (MakePick team player, Drafting ctx) ->
    case dcRemaining ctx of
      [] ->
        -- Empty remaining list shouldn't be reachable in a Drafting
        -- state — applyCommand transitions to Complete on the last
        -- pick — but guard against a hand-constructed DraftContext.
        Left DraftAlreadyComplete
      (expectedTeam, pickNum) : rest
        | team /= expectedTeam ->
            Left (NotYourTurn expectedTeam team)
        | not (Set.member player (dcAvailable ctx)) ->
            Left $
              if any ((player ==) . dpePlayer) (dcPicksMade ctx)
                then PlayerAlreadyDrafted player
                else PlayerNotInPool player
        | otherwise ->
            let entry  = DraftPickEntry pickNum team player
                picks' = dcPicksMade ctx ++ [entry]
                ctx'   = ctx
                  { dcRemaining = rest
                  , dcAvailable = Set.delete player (dcAvailable ctx)
                  , dcPicksMade = picks'
                  }
            in case rest of
                 [] ->
                   let summary = DraftSummary
                         { dsLeague = dcLeague ctx
                         , dsPicks  = picks'
                         }
                   in Right
                        ( Complete summary
                        , [PickRecorded entry, DraftCompleted summary]
                        )
                 _  -> Right (Drafting ctx', [PickRecorded entry])

  -- EndDraft: legal from Drafting (commissioner ends the draft early
  -- with whatever picks have been made). Not legal otherwise.
  (EndDraft, WaitingToStart) -> Left DraftNotStarted
  (EndDraft, Complete _)     -> Left DraftAlreadyComplete
  (EndDraft, Drafting ctx)   ->
    let summary = DraftSummary
          { dsLeague = dcLeague ctx
          , dsPicks  = dcPicksMade ctx
          }
    in Right (Complete summary, [DraftCompleted summary])

-- ---------------------------------------------------------------------
-- Inspection helpers
-- ---------------------------------------------------------------------

-- | Whose pick is up? 'Nothing' if the draft hasn't started or has
-- finished.
currentPicker :: DraftState -> Maybe (DbLeagueTeamId, DraftPickNumber)
currentPicker = \case
  WaitingToStart -> Nothing
  Drafting ctx   -> case dcRemaining ctx of
    p : _ -> Just p
    []    -> Nothing
  Complete _     -> Nothing

-- | How many picks remain. Zero outside the 'Drafting' phase.
picksRemaining :: DraftState -> Int
picksRemaining = \case
  WaitingToStart -> 0
  Drafting ctx   -> length (dcRemaining ctx)
  Complete _     -> 0

-- | Is this player still in the available pool? 'False' outside the
-- 'Drafting' phase.
isPlayerAvailable :: DbPlayerId -> DraftState -> Bool
isPlayerAvailable pid = \case
  WaitingToStart -> False
  Drafting ctx   -> Set.member pid (dcAvailable ctx)
  Complete _     -> False