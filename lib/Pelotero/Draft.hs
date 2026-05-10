{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Pelotero.Draft
-- Description : Draft data types shared between the state machine and persistence.
--
-- The transition logic lives in 'Pelotero.Draft.Machine' (a crem state machine
-- with type-level guarantees on which commands are valid in which states).
-- This module exports only the data types; it does not expose a pure
-- @applyCommand@ function. Drive the draft via 'Pelotero.Draft.Machine.runDraftCommand'.
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

  , currentPicker
  , picksRemaining
  , isPlayerAvailable
  ) where

import           Data.Set  (Set)
import qualified Data.Set  as Set

import           Pelotero.Domain.Id
                     ( DbLeagueConfigId
                     , DbLeagueTeamId
                     , DbPlayerId
                     , DraftPickNumber
                     )

data DraftState
  = WaitingToStart
  | Drafting !DraftContext
  | Complete !DraftSummary
  deriving stock (Show, Eq)

data DraftContext = DraftContext
  { dcLeague    :: !DbLeagueConfigId
  , dcOrder     :: ![(DbLeagueTeamId, DraftPickNumber)]
  , dcRemaining :: ![(DbLeagueTeamId, DraftPickNumber)]
  , dcAvailable :: !(Set DbPlayerId)
  , dcPicksMade :: ![DraftPickEntry]
  }
  deriving stock (Show, Eq)

data DraftPickEntry = DraftPickEntry
  { dpePickNumber :: !DraftPickNumber
  , dpeTeam       :: !DbLeagueTeamId
  , dpePlayer     :: !DbPlayerId
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
  { dpLeague    :: !DbLeagueConfigId
  , dpOrder     :: ![(DbLeagueTeamId, DraftPickNumber)]
  , dpAvailable :: !(Set DbPlayerId)
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
  | NotYourTurn          !DbLeagueTeamId !DbLeagueTeamId
  | PlayerAlreadyDrafted !DbPlayerId
  | PlayerNotInPool      !DbPlayerId
  deriving stock (Show, Eq)

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
  Drafting ctx   -> Set.member pid (dcAvailable ctx)
  Complete _     -> False