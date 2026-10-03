-- |
-- Module      : Pelotero.Draft
-- Description : Draft data types shared between the state machine and persistence.
--
-- The transition logic lives in 'Pelotero.Draft.Machine' (a crem state
-- machine with type-level guarantees on which commands are valid in which
-- states). This module holds the data the machine works on, the plan
-- validation it runs before a draft starts, the rule that decides whether
-- a pick is accepted, and the read-only helpers callers use to inspect a
-- draft in progress.
--
-- A draft is roster-aware. The plan carries each draftable player's
-- position and the league's 'RosterLimits'; the context tracks how full
-- every team's roster is. A pick is only accepted when the player fits an
-- open slot on the picking team, and the accepted pick records which slot
-- that was. A finished draft therefore always describes rosters that
-- satisfy the league's limits.
--
-- A draft can also always be finished. 'validatePlan' refuses a plan
-- whose picks cannot all be placed with the pool it has, and a pick is
-- refused when it fits but would leave the remaining picks impossible to
-- place ('PickStrandsDraft'). Every context the machine reaches is
-- therefore one where 'isCompletable' holds, and the team on the clock
-- always has at least one pick that 'acceptedSlot' accepts.
-- "Pelotero.Draft.Feasibility" decides completability.
module Pelotero.Draft
  ( DraftState (..)
  , DraftContext (..)
  , DraftPickEntry (..)
  , DraftSummary (..)
  , initialState

  , DraftPlan (..)
  , DraftCommand (..)
  , DraftEvent (..)
  , DraftError (..)

  , validatePlan
  , startContext

  , applyPick
  , isCompletable
  , acceptedSlot

  , currentPicker
  , picksRemaining
  , isPlayerAvailable
  , rosterCountsFor
  , slotForPick
  ) where

import           Control.Monad               (guard)
import           Data.List                   (find)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (listToMaybe)

import           Pelotero.Domain.Eligibility
                     ( SlotCounts
                     , emptySlotCounts
                     , occupySlot
                     , openSlotFor
                     )
import           Pelotero.Domain.Id
                     ( DbLeagueConfigId
                     , DbLeagueTeamId
                     , DbPlayerId
                     , DraftPickNumber (..)
                     )
import           Pelotero.Domain.Position    (Position)
import           Pelotero.Domain.Roster
                     ( RosterLimits
                     , RosterSlot
                     , totalRosterSize
                     )
import           Pelotero.Draft.Feasibility  (TeamNeed (..), placeablePicks)

data DraftState
  = WaitingToStart
  | Drafting !DraftContext
  | Complete !DraftSummary
  deriving stock (Show, Eq)

data DraftContext = DraftContext
  { dcLeague    :: !DbLeagueConfigId
  , dcOrder     :: ![(DbLeagueTeamId, DraftPickNumber)]
  , dcRemaining :: ![(DbLeagueTeamId, DraftPickNumber)]
  , dcPool      :: !(Map DbPlayerId Position)
    -- ^ Players still available, with the position that decides which
    --   slots they may fill.
  , dcLimits    :: !RosterLimits
  , dcRosters   :: !(Map DbLeagueTeamId SlotCounts)
    -- ^ How full each team's roster is. A team with no picks yet has no
    --   entry; read through 'rosterCountsFor'.
  , dcPicksMade :: ![DraftPickEntry]
  }
  deriving stock (Show, Eq)

data DraftPickEntry = DraftPickEntry
  { dpePickNumber :: !DraftPickNumber
  , dpeTeam       :: !DbLeagueTeamId
  , dpePlayer     :: !DbPlayerId
  , dpeSlot       :: !RosterSlot
    -- ^ The roster slot the player was placed in.
  }
  deriving stock (Show, Eq)

data DraftSummary = DraftSummary
  { dsLeague :: !DbLeagueConfigId
  , dsPicks  :: ![DraftPickEntry]
  }
  deriving stock (Show, Eq)

initialState :: DraftState
initialState = WaitingToStart

data DraftPlan = DraftPlan
  { dpLeague :: !DbLeagueConfigId
  , dpOrder  :: ![(DbLeagueTeamId, DraftPickNumber)]
  , dpPool   :: !(Map DbPlayerId Position)
  , dpLimits :: !RosterLimits
  }
  deriving stock (Show, Eq)

data DraftCommand
  = StartDraft !DraftPlan
  | MakePick   !DbLeagueTeamId !DbPlayerId
  | EndDraft
  deriving stock (Show, Eq)

data DraftEvent
  = DraftStarted   !DbLeagueConfigId ![DbLeagueTeamId]
  | PickRecorded   !DraftPickEntry
  | DraftCompleted !DraftSummary
  deriving stock (Show, Eq)

data DraftError
  = DraftNotStarted
  | DraftAlreadyStarted
  | DraftAlreadyComplete
  | EmptyDraftPlan
  | PlanPickNumbersNotSequential
    -- ^ Pick numbers in the plan are not exactly 1, 2, 3, ... in order.
  | PlanOverfillsRoster  !DbLeagueTeamId !Int !Int
    -- ^ Team, picks the plan gives it, and the roster capacity. Such a
    --   plan could never finish, so it is refused before it starts.
  | PlanInfeasible       !Int !Int
    -- ^ Picks the pool can place at once, and picks the plan makes. The
    --   pool cannot fill the rosters the plan asks for, so the draft
    --   could never finish and is refused before it starts. Which slots
    --   go unfilled is not reported because it differs between equally
    --   good assignments; only these two numbers are fixed.
  | NotYourTurn          !DbLeagueTeamId !DbLeagueTeamId
  | PlayerAlreadyDrafted !DbPlayerId
  | PlayerNotInPool      !DbPlayerId
  | NoOpenSlot           !DbLeagueTeamId !DbPlayerId
    -- ^ Every slot the player is eligible for is full on that team.
  | PickStrandsDraft     !DbLeagueTeamId !DbPlayerId
    -- ^ The player fits an open slot, but after taking him the
    --   remaining picks could no longer all be placed, so the draft
    --   could not finish.
  deriving stock (Show, Eq)

-- | Check a plan before the draft starts. 'Nothing' means the plan is
-- acceptable, which includes that some sequence of picks completes it.
validatePlan :: DraftPlan -> Maybe DraftError
validatePlan plan
  | null order                             = Just EmptyDraftPlan
  | map snd order /= expectedNumbers       = Just PlanPickNumbersNotSequential
  | Just (team, n) <- find overfull counts = Just (PlanOverfillsRoster team n capacity)
  | placeable < needed                     = Just (PlanInfeasible placeable needed)
  | otherwise                              = Nothing
  where
    order           = dpOrder plan
    expectedNumbers = map DraftPickNumber [1 .. length order]
    capacity        = totalRosterSize (dpLimits plan)
    counts          = Map.toAscList (Map.fromListWith (+) [(team, 1 :: Int) | (team, _) <- order])
    overfull (_, n) = n > capacity
    (placeable, needed) = remainingCapacity (startContext plan)

-- | The context a validated plan starts with.
startContext :: DraftPlan -> DraftContext
startContext plan = DraftContext
  { dcLeague    = dpLeague plan
  , dcOrder     = dpOrder plan
  , dcRemaining = dpOrder plan
  , dcPool      = dpPool plan
  , dcLimits    = dpLimits plan
  , dcRosters   = Map.empty
  , dcPicksMade = []
  }

-- | The context after a pick: the scheduled pick is used up, the player
-- leaves the pool, the team's roster gains the entry's slot, and the
-- entry is recorded. Does not check the pick; the machine checks it with
-- 'slotForPick' and 'isCompletable' first.
applyPick :: DraftPickEntry -> DraftContext -> DraftContext
applyPick entry ctx = ctx
  { dcRemaining = drop 1 (dcRemaining ctx)
  , dcPool      = Map.delete (dpePlayer entry) (dcPool ctx)
  , dcRosters   = Map.insert team
                    (occupySlot (dpeSlot entry) (rosterCountsFor team ctx))
                    (dcRosters ctx)
  , dcPicksMade = dcPicksMade ctx ++ [entry]
  }
  where
    team = dpeTeam entry

-- | Whether every remaining pick can still be placed with the players
-- left in the pool.
isCompletable :: DraftContext -> Bool
isCompletable ctx = placeable == needed
  where
    (placeable, needed) = remainingCapacity ctx

-- | How many of the remaining picks can be placed at once, and how many
-- remain.
remainingCapacity :: DraftContext -> (Int, Int)
remainingCapacity ctx = (placeablePicks (dcLimits ctx) supply needs, length (dcRemaining ctx))
  where
    supply   = Map.fromListWith (+) [(position, 1) | position <- Map.elems (dcPool ctx)]
    picksFor = Map.fromListWith (+) [(team, 1) | (team, _) <- dcRemaining ctx]
    needs    = [ TeamNeed (rosterCountsFor team ctx) n | (team, n) <- Map.toList picksFor ]

-- | The slot the machine would put this player in if this team picked
-- him now, or 'Nothing' when the machine would refuse the pick: it is
-- not the team's turn, the player is not available, he fits no open
-- slot, or taking him would strand the rest of the draft. A pick for
-- which this returns @Just slot@ is accepted by 'MakePick' and recorded
-- in @slot@.
acceptedSlot :: DraftContext -> DbLeagueTeamId -> DbPlayerId -> Maybe RosterSlot
acceptedSlot ctx team player = do
  (onTheClock, pickNumber) <- listToMaybe (dcRemaining ctx)
  guard (onTheClock == team)
  slot <- slotForPick ctx team player
  guard (isCompletable (applyPick (DraftPickEntry pickNumber team player slot) ctx))
  pure slot

-- | The team scheduled to make the next pick, if the draft is in progress.
currentPicker :: DraftState -> Maybe (DbLeagueTeamId, DraftPickNumber)
currentPicker = \case
  WaitingToStart -> Nothing
  Drafting ctx   -> case dcRemaining ctx of
    p : _ -> Just p
    []    -> Nothing
  Complete _     -> Nothing

-- | How many picks remain. Zero before start and after completion.
picksRemaining :: DraftState -> Int
picksRemaining = \case
  WaitingToStart -> 0
  Drafting ctx   -> length (dcRemaining ctx)
  Complete _     -> 0

-- | Whether the given player is still in the available pool.
isPlayerAvailable :: DbPlayerId -> DraftState -> Bool
isPlayerAvailable pid = \case
  WaitingToStart -> False
  Drafting ctx   -> Map.member pid (dcPool ctx)
  Complete _     -> False

-- | How full a team's roster is so far.
rosterCountsFor :: DbLeagueTeamId -> DraftContext -> SlotCounts
rosterCountsFor team = Map.findWithDefault emptySlotCounts team . dcRosters

-- | The slot an available player would take on a team's roster, or
-- 'Nothing' when the player is not in the pool or does not fit. This is
-- only the fit rule. The machine also refuses a fitting pick that would
-- strand the draft; 'acceptedSlot' applies both rules.
slotForPick :: DraftContext -> DbLeagueTeamId -> DbPlayerId -> Maybe RosterSlot
slotForPick ctx team player = do
  position <- Map.lookup player (dcPool ctx)
  openSlotFor (dcLimits ctx) (rosterCountsFor team ctx) position