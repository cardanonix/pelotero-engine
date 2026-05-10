module Pelotero.Draft.MachineSpec (spec) where

import qualified Data.Set as Set
import Hedgehog (Gen, annotate, failure, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Pelotero.Domain.Id
  ( DbLeagueConfigId(..)
  , DbLeagueTeamId(..)
  , DbPlayerId(..)
  , DraftPickNumber(..)
  )
import qualified Pelotero.Draft         as D
import qualified Pelotero.Draft.Machine as M

spec :: Spec
spec = describe "Pelotero.Draft.Machine" $ do

  it "rejected commands leave the state unchanged" $
    hedgehog $ do
      state <- forAll genReachableState
      cmd   <- forAll genCommand
      case M.runDraftCommand (M.fromDraftState state) cmd of
        (Left _, M.SomeDraftStateG _ st') ->
          M.toDraftState st' === state
        (Right _, _) ->
          pure ()

  it "Complete is absorbing: every command from Complete is rejected" $
    hedgehog $ do
      summary <- forAll genCompleteSummary
      cmd     <- forAll genCommand
      let state = D.Complete summary
      case M.runDraftCommand (M.fromDraftState state) cmd of
        (Left D.DraftAlreadyComplete, M.SomeDraftStateG _ st') ->
          M.toDraftState st' === state
        other -> do
          annotate ("expected DraftAlreadyComplete; got: " <> show other)
          failure

-- | Drive the machine and project the result into the pure shape, used
-- only by the generators below to construct reachable states.
runMachine
  :: D.DraftState
  -> D.DraftCommand
  -> Either D.DraftError (D.DraftState, [D.DraftEvent])
runMachine state cmd =
  case M.runDraftCommand (M.fromDraftState state) cmd of
    (Left err, _) ->
      Left err
    (Right evs, M.SomeDraftStateG _ st') ->
      Right (M.toDraftState st', evs)

-- | Small but realistic draft plan: 2-5 teams, 2-6 rounds, with a pool
-- strictly larger than the pick count. Player ids are deterministic
-- (1..poolSize) so 'Set.lookupMin' in 'genReachableState' always finds
-- an available pick — keeps the generators discard-free in normal flow.
genDraftPlan :: Gen D.DraftPlan
genDraftPlan = do
  teamCount <- Gen.int (Range.linear 2 5)
  rounds    <- Gen.int (Range.linear 2 6)
  let league = DbLeagueConfigId 1
      teams  = [DbLeagueTeamId (fromIntegral t) | t <- [1 .. teamCount]]
      order  = zipWith (\n t -> (t, DraftPickNumber n))
                       [1 ..]
                       (concat (replicate rounds teams))
      totalPicks = length order
  extraPlayers <- Gen.int (Range.linear 0 10)
  let poolSize = totalPicks + extraPlayers
      players  = Set.fromList
                   [DbPlayerId (fromIntegral p) | p <- [1 .. poolSize]]
  pure (D.DraftPlan league order players)

-- | A 'DraftState' reachable from 'WaitingToStart' via some sequence
-- of valid commands.
genReachableState :: Gen D.DraftState
genReachableState = Gen.choice
  [ pure D.WaitingToStart
  , genDraftingState
  , genCompleteState
  ]
  where
    genDraftingState = do
      plan <- genDraftPlan
      case runMachine D.WaitingToStart (D.StartDraft plan) of
        Right (D.Drafting ctx, _) -> do
          let total = length (D.dcRemaining ctx)
          n <- Gen.int (Range.linear 0 (total - 1))
          applyNPicks n (D.Drafting ctx)
        _ -> Gen.discard

    genCompleteState = do
      plan <- genDraftPlan
      case runMachine D.WaitingToStart (D.StartDraft plan) of
        Right (state1, _) ->
          applyNPicks (length (D.dpOrder plan)) state1
        _ -> Gen.discard

    applyNPicks 0 st = pure st
    applyNPicks n st = case st of
      D.Drafting ctx -> case D.dcRemaining ctx of
        []            -> pure st
        (team, _) : _ -> case Set.lookupMin (D.dcAvailable ctx) of
          Just player -> case runMachine st (D.MakePick team player) of
            Right (st', _) -> applyNPicks (n - 1) st'
            Left _         -> Gen.discard
          Nothing -> Gen.discard
      _ -> pure st

-- | A summary representing a completed draft, for testing the
-- absorbing 'Complete' state.
genCompleteSummary :: Gen D.DraftSummary
genCompleteSummary = do
  plan <- genDraftPlan
  case runMachine D.WaitingToStart (D.StartDraft plan) of
    Right (s1, _) -> drainPicks (length (D.dpOrder plan)) s1
    _             -> Gen.discard
  where
    drainPicks 0 st = case st of
      D.Complete summary -> pure summary
      _                  -> Gen.discard
    drainPicks n st = case st of
      D.Drafting ctx -> case D.dcRemaining ctx of
        []            -> Gen.discard
        (team, _) : _ -> case Set.lookupMin (D.dcAvailable ctx) of
          Just player -> case runMachine st (D.MakePick team player) of
            Right (st', _) -> drainPicks (n - 1) st'
            Left _         -> Gen.discard
          Nothing -> Gen.discard
      D.Complete summary -> pure summary
      _ -> Gen.discard

-- | Free-form command generator. Teams and players are drawn from id
-- ranges that may or may not overlap with whatever plan produced the
-- state — that's deliberate, since most generated commands hit
-- rejection paths and the rejection-invariance property earns its keep on them.
genCommand :: Gen D.DraftCommand
genCommand = Gen.choice
  [ D.StartDraft <$> genDraftPlan
  , D.MakePick   <$> (DbLeagueTeamId <$> Gen.int64 (Range.linear 1 10))
                 <*> (DbPlayerId     <$> Gen.int64 (Range.linear 1 100))
  , pure D.EndDraft
  ]