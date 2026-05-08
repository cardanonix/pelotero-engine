{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-star-is-type #-}

-- | The crem state machine wrapping 'Pelotero.Draft'\'s pure
-- transition logic. Adds a vertex-tagged GADT state, a typed
-- topology declaration, and a runtime existential — the pattern
-- lifted from cheeblr's @State.TransactionMachine@.
--
-- Three vertices, two edges of forward progress:
--
-- > WaitingToStart --(StartDraft)--> Drafting --(MakePick last | EndDraft)--> Complete
--
-- Self-loops on every vertex are deliberate — they're how a rejected
-- command stays put without violating the topology.
module Pelotero.Draft.Machine
  ( -- * Vertex kind and singletons
    DraftVertex (..)
  , SDraftVertex (..)
    -- * Topology
  , DraftTopology
    -- * GADT state
  , DraftStateG (..)
  , SomeDraftStateG (..)
  , initialMachineState
  , draftStateVertex
    -- * Conversions to and from the plain Pelotero.Draft.DraftState
  , fromDraftState
  , toDraftState
  , toSomeDraftStateG
    -- * Transition
  , draftAction
  , runDraftCommand
  ) where

import           Crem.BaseMachine               (ActionResult (..), pureResult)
import           Crem.Render.RenderableVertices (RenderableVertices (..))
import           Crem.Topology                  (Topology (..))
import           Data.Functor.Identity          (Identity, runIdentity)
import           Data.List                      (nub)
import qualified Data.Set                       as Set
import           Data.Singletons.Base.TH

import           Pelotero.Draft
                     ( DraftCommand (..)
                     , DraftContext (..)
                     , DraftError (..)
                     , DraftEvent (..)
                     , DraftPickEntry (..)
                     , DraftPlan (..)
                     , DraftState (..)
                     , DraftSummary (..)
                     )

-- ---------------------------------------------------------------------
-- Vertex kind, singletons, topology
-- ---------------------------------------------------------------------

$( singletons
     [d|
       data DraftVertex
         = WaitingToStartV
         | DraftingV
         | CompleteV
         deriving (Eq, Show)
       |]
 )

deriving instance Enum DraftVertex
deriving instance Bounded DraftVertex

instance RenderableVertices DraftVertex where
  vertices = [minBound .. maxBound]

-- | Each vertex includes itself in its successor list so a rejected
-- command can stay-put without violating the topology. Forward
-- progress is one edge each: WaitingToStart -> Drafting -> Complete.
type DraftTopology =
  'Topology
    '[ '( 'WaitingToStartV, '[ 'WaitingToStartV, 'DraftingV ])
     , '( 'DraftingV,       '[ 'DraftingV,       'CompleteV ])
     , '( 'CompleteV,       '[ 'CompleteV ])
     ]

-- ---------------------------------------------------------------------
-- GADT state
-- ---------------------------------------------------------------------

-- | The state machine state, vertex-tagged. 'DraftingG' carries the
-- working context; 'CompleteG' carries the final summary.
data DraftStateG (v :: DraftVertex) where
  WaitingToStartG :: DraftStateG 'WaitingToStartV
  DraftingG       :: !DraftContext -> DraftStateG 'DraftingV
  CompleteG       :: !DraftSummary -> DraftStateG 'CompleteV

-- | Existential wrapper used at the runtime boundary. The
-- 'SDraftVertex' singleton lets a caller inspect which vertex the
-- state is in without losing the typed payload. Mirrors cheeblr's
-- 'SomeTxState'.
data SomeDraftStateG = forall v.
  SomeDraftStateG (SDraftVertex v) (DraftStateG v)

initialMachineState :: SomeDraftStateG
initialMachineState = SomeDraftStateG SWaitingToStartV WaitingToStartG

-- | Plain enum projection of which vertex a 'SomeDraftStateG' is in.
-- Useful for callers that want to render the state without inspecting
-- its payload.
draftStateVertex :: SomeDraftStateG -> DraftVertex
draftStateVertex (SomeDraftStateG sv _) = case sv of
  SWaitingToStartV -> WaitingToStartV
  SDraftingV       -> DraftingV
  SCompleteV       -> CompleteV

-- ---------------------------------------------------------------------
-- Conversions
-- ---------------------------------------------------------------------

-- | Wrap a typed state in the existential, picking the right
-- singleton.
toSomeDraftStateG :: DraftStateG v -> SomeDraftStateG
toSomeDraftStateG = \case
  s@WaitingToStartG -> SomeDraftStateG SWaitingToStartV s
  s@(DraftingG _)   -> SomeDraftStateG SDraftingV s
  s@(CompleteG _)   -> SomeDraftStateG SCompleteV s

-- | Bridge from 'Pelotero.Draft.DraftState' (plain ADT) to the
-- existential GADT.
fromDraftState :: DraftState -> SomeDraftStateG
fromDraftState = \case
  WaitingToStart   -> SomeDraftStateG SWaitingToStartV WaitingToStartG
  Drafting ctx     -> SomeDraftStateG SDraftingV (DraftingG ctx)
  Complete summary -> SomeDraftStateG SCompleteV (CompleteG summary)

-- | Bridge from a typed GADT state back to plain 'DraftState'. Useful
-- for reusing the inspection helpers in 'Pelotero.Draft'
-- ('currentPicker', 'picksRemaining', 'isPlayerAvailable').
toDraftState :: DraftStateG v -> DraftState
toDraftState = \case
  WaitingToStartG -> WaitingToStart
  DraftingG ctx   -> Drafting ctx
  CompleteG summ  -> Complete summ

-- ---------------------------------------------------------------------
-- Transition
-- ---------------------------------------------------------------------

-- | The vertex-typed transition function. crem's 'ActionResult'
-- carries the topology constraint: the second argument to
-- 'pureResult' must be a state in a vertex that 'DraftTopology'
-- allows from the input vertex @v@.
--
-- Output is @Either DraftError [DraftEvent]@. 'Left' signals a
-- rejected command (state stays put); 'Right' carries the events the
-- handler should persist, in order.
--
-- The transition logic is reimplemented per-vertex rather than going
-- through 'Pelotero.Draft.applyCommand' so each branch can name the
-- specific result vertex; D.1d will add a property test asserting
-- the two functions agree on every (state, command) pair.
draftAction
  :: DraftStateG v
  -> DraftCommand
  -> ActionResult Identity DraftTopology DraftStateG v (Either DraftError [DraftEvent])

-- WaitingToStart: only StartDraft makes progress.
draftAction WaitingToStartG (StartDraft plan)
  | null (dpOrder plan) =
      pureResult (Left EmptyDraftPlan) WaitingToStartG
  | otherwise =
      let teams = nub (map fst (dpOrder plan))
          ctx   = DraftContext
            { dcLeague    = dpLeague plan
            , dcOrder     = dpOrder plan
            , dcRemaining = dpOrder plan
            , dcAvailable = dpAvailable plan
            , dcPicksMade = []
            }
      in pureResult
           (Right [DraftStarted (dpLeague plan) teams])
           (DraftingG ctx)
draftAction WaitingToStartG (MakePick _ _) =
  pureResult (Left DraftNotStarted) WaitingToStartG
draftAction WaitingToStartG EndDraft =
  pureResult (Left DraftNotStarted) WaitingToStartG

-- Drafting: MakePick advances or completes; EndDraft completes early.
draftAction (DraftingG ctx) (StartDraft _) =
  pureResult (Left DraftAlreadyStarted) (DraftingG ctx)
draftAction (DraftingG ctx) (MakePick team player) =
  case dcRemaining ctx of
    [] ->
      -- Defensive: reachable only via a hand-built DraftContext.
      pureResult (Left DraftAlreadyComplete) (DraftingG ctx)
    (expectedTeam, pickNum) : rest
      | team /= expectedTeam ->
          pureResult
            (Left (NotYourTurn expectedTeam team))
            (DraftingG ctx)
      | not (Set.member player (dcAvailable ctx)) ->
          let err = if any ((player ==) . dpePlayer) (dcPicksMade ctx)
                     then PlayerAlreadyDrafted player
                     else PlayerNotInPool player
          in pureResult (Left err) (DraftingG ctx)
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
                 in pureResult
                      (Right [PickRecorded entry, DraftCompleted summary])
                      (CompleteG summary)
               _ ->
                 pureResult
                   (Right [PickRecorded entry])
                   (DraftingG ctx')
draftAction (DraftingG ctx) EndDraft =
  let summary = DraftSummary
        { dsLeague = dcLeague ctx
        , dsPicks  = dcPicksMade ctx
        }
  in pureResult
       (Right [DraftCompleted summary])
       (CompleteG summary)

-- Complete: terminal vertex; every command is rejected.
draftAction (CompleteG summ) (StartDraft _) =
  pureResult (Left DraftAlreadyComplete) (CompleteG summ)
draftAction (CompleteG summ) (MakePick _ _) =
  pureResult (Left DraftAlreadyComplete) (CompleteG summ)
draftAction (CompleteG summ) EndDraft =
  pureResult (Left DraftAlreadyComplete) (CompleteG summ)

-- | Runtime API: applies a command to an existential state and
-- returns both the action's output and the updated existential.
-- Mirrors cheeblr's 'runTxCommand'.
--
-- Rejection (Left) cases leave the state untouched; success (Right)
-- cases reflect the transition.
runDraftCommand
  :: SomeDraftStateG
  -> DraftCommand
  -> (Either DraftError [DraftEvent], SomeDraftStateG)
runDraftCommand (SomeDraftStateG _ st) cmd =
  case draftAction st cmd of
    ActionResult m ->
      let (out, nextSt) = runIdentity m
      in (out, toSomeDraftStateG nextSt)