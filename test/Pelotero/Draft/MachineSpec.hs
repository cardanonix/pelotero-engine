module Pelotero.Draft.MachineSpec (spec) where

import           Data.Foldable               (for_)
import           Data.List                   (find, nub)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (isJust)
import           Hedgehog
                     ( Gen
                     , PropertyT
                     , annotate
                     , annotateShow
                     , assert
                     , failure
                     , forAll
                     , success
                     , (===)
                     )
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Test.Hspec                  (Spec, describe, it)
import           Test.Hspec.Hedgehog         (hedgehog)

import           Pelotero.Domain.Eligibility (isEligibleFor)
import           Pelotero.Domain.Id
                     ( DbLeagueConfigId (..)
                     , DbLeagueTeamId (..)
                     , DbPlayerId (..)
                     , DraftPickNumber (..)
                     )
import           Pelotero.Domain.Position    (Position (..))
import           Pelotero.Domain.Roster
                     ( RosterLimits (..)
                     , RosterSlot (..)
                     , allRosterSlots
                     , rosterLimitFor
                     , totalRosterSize
                     )
import qualified Pelotero.Draft              as D
import qualified Pelotero.Draft.Machine      as M

spec :: Spec
spec = describe "Pelotero.Draft.Machine" $ do

  it "rejected commands leave the state unchanged" $
    hedgehog $ do
      (_, state) <- forAll genReachable
      cmd        <- forAll genCommand
      case M.runDraftCommand (M.fromDraftState state) cmd of
        (Left _, M.SomeDraftStateG _ st') ->
          M.toDraftState st' === state
        (Right _, _) ->
          pure ()

  it "Complete is absorbing: every command from Complete is rejected" $
    hedgehog $ do
      (_, state) <- forAll genCompleted
      cmd        <- forAll genCommand
      case M.runDraftCommand (M.fromDraftState state) cmd of
        (Left D.DraftAlreadyComplete, M.SomeDraftStateG _ st') ->
          M.toDraftState st' === state
        other -> do
          annotate ("expected DraftAlreadyComplete; got: " <> show other)
          failure

  it "every reachable state respects the roster limits" $
    hedgehog $ do
      (plan, state) <- forAll genReachable
      let picks  = picksOf state
          limits = D.dpLimits plan
      -- No player is drafted twice.
      nub (map D.dpePlayer picks) === map D.dpePlayer picks
      -- Every pick sits in a slot its position is eligible for.
      assert $ and
        [ maybe False (`isEligibleFor` D.dpeSlot p)
            (Map.lookup (D.dpePlayer p) (D.dpPool plan))
        | p <- picks
        ]
      -- No team holds more players in a slot than the limit allows.
      assert $ and
        [ n <= rosterLimitFor slot limits
        | ((_, slot), n) <- Map.toList (slotTally picks)
        ]

  it "a completed draft fills every slot of every roster exactly" $
    hedgehog $ do
      (plan, state) <- forAll genCompleted
      let picks  = picksOf state
          limits = D.dpLimits plan
          teams  = nub (map fst (D.dpOrder plan))
          tally  = slotTally picks
      length picks === length (D.dpOrder plan)
      sequence_
        [ Map.findWithDefault 0 (team, slot) tally === rosterLimitFor slot limits
        | team <- teams
        , slot <- allRosterSlots
        ]

  it "StartDraft accepts exactly the plans some sequence of picks completes" $
    hedgehog $ do
      plan <- forAll genSmallPlan
      let completes = completable (D.startContext plan)
      annotateShow completes
      case D.validatePlan plan of
        Nothing                     -> assert completes
        Just (D.PlanInfeasible _ _) -> assert (not completes)
        Just other                  -> annotateShow other >> failure

  it "the team on the clock always has an accepted pick, and MakePick refuses exactly the stranding ones" $
    hedgehog $ do
      plan <- forAll genSmallPlan
      case runMachine D.WaitingToStart (D.StartDraft plan) of
        Left _           -> success
        Right (state, _) -> driveToCompletion state

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

-- | Whether some sequence of fitting picks finishes the draft from this
-- context, by exhaustive search. Players of one position are
-- interchangeable, so trying one player of each position is enough.
-- Independent of "Pelotero.Draft.Feasibility": it uses only the fit
-- rule ('D.slotForPick') and 'D.applyPick'.
completable :: D.DraftContext -> Bool
completable ctx = case D.dcRemaining ctx of
  [] -> True
  (team, pickNumber) : _ -> any tryPlayer onePerPosition
    where
      onePerPosition =
        Map.elems (Map.fromList [(pos, pid) | (pid, pos) <- Map.toList (D.dcPool ctx)])
      tryPlayer player = case D.slotForPick ctx team player of
        Nothing   -> False
        Just slot -> completable (D.applyPick (D.DraftPickEntry pickNumber team player slot) ctx)

-- | From a started draft, check at every step that 'D.acceptedSlot' and
-- 'D.MakePick' agree on every available player, that a fitting pick is
-- refused exactly when no completion follows it, and that at least one
-- pick is accepted; then make a random accepted pick and continue until
-- the draft completes.
driveToCompletion :: D.DraftState -> PropertyT IO ()
driveToCompletion = \case
  D.Complete _     -> success
  D.WaitingToStart -> annotate "draft fell back to WaitingToStart" >> failure
  state@(D.Drafting ctx) -> case D.dcRemaining ctx of
    [] -> annotate "Drafting with no picks left" >> failure
    (team, pickNumber) : _ -> do
      let players = Map.keys (D.dcPool ctx)
      for_ players $ \player -> do
        case (D.acceptedSlot ctx team player, runMachine state (D.MakePick team player)) of
          (Just slot, Right (_, D.PickRecorded entry : _)) -> D.dpeSlot entry === slot
          (Nothing, Left _)                                  -> success
          other -> annotateShow (player, other) >> failure
        case D.slotForPick ctx team player of
          Nothing   -> success
          Just slot ->
            isJust (D.acceptedSlot ctx team player)
              === completable (D.applyPick (D.DraftPickEntry pickNumber team player slot) ctx)
      case filter (isJust . D.acceptedSlot ctx team) players of
        []       -> annotate "no accepted pick for the team on the clock" >> failure
        accepted -> do
          player <- forAll (Gen.element accepted)
          case runMachine state (D.MakePick team player) of
            Right (next, _) -> driveToCompletion next
            Left err        -> annotateShow err >> failure

-- | Drive the machine and project the result into the pure shape.
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

picksOf :: D.DraftState -> [D.DraftPickEntry]
picksOf = \case
  D.WaitingToStart   -> []
  D.Drafting ctx     -> D.dcPicksMade ctx
  D.Complete summary -> D.dsPicks summary

-- | Players held per (team, slot).
slotTally :: [D.DraftPickEntry] -> Map.Map (DbLeagueTeamId, RosterSlot) Int
slotTally picks =
  Map.fromListWith (+) [((D.dpeTeam p, D.dpeSlot p), 1) | p <- picks]

-- ---------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------

-- | Largest per-slot limit the generators use.
maxSlotLimit :: Int
maxSlotLimit = 2

-- | Roster limits with every slot between 0 and 'maxSlotLimit', and at
-- least one slot overall.
genLimits :: Gen RosterLimits
genLimits = do
  counts <- traverse (const (Gen.int (Range.linear 0 maxSlotLimit))) allRosterSlots
  let limits = RosterLimits (Map.fromList (zip allRosterSlots counts))
  pure $ if totalRosterSize limits == 0
    then RosterLimits (Map.fromList [(SlotUtility, 1)])
    else limits

-- | A plan that can always be completed, whatever order the teams pick
-- in. Each team gets exactly as many picks as its roster holds, in
-- round-robin order. The pool holds @teams * (maxSlotLimit + 2)@ players
-- at every position: enough to fill the position's own slot on every
-- team even after other teams have spent players of that position on
-- their utility slots (pitchers need @teams * 2 * maxSlotLimit@, which is
-- the same number).
genDraftPlan :: Gen D.DraftPlan
genDraftPlan = do
  teamCount <- Gen.int (Range.linear 2 4)
  limits    <- genLimits
  let league      = DbLeagueConfigId 1
      teams       = [DbLeagueTeamId (fromIntegral t) | t <- [1 .. teamCount]]
      rounds      = totalRosterSize limits
      order       = zipWith (\n t -> (t, DraftPickNumber n))
                            [1 ..]
                            (concat (replicate rounds teams))
      perPosition = teamCount * (maxSlotLimit + 2)
      positions   = concatMap (replicate perPosition) [minBound .. maxBound :: Position]
      pool        = Map.fromList (zip (map DbPlayerId [1 ..]) positions)
  pure (D.DraftPlan league order pool limits)

-- | Make up to @n@ valid picks from a state, choosing each player at
-- random among the picks the machine accepts for the picking team.
applyPicks :: Int -> D.DraftState -> Gen D.DraftState
applyPicks n st
  | n <= 0    = pure st
  | otherwise = case st of
      D.Drafting ctx -> case D.dcRemaining ctx of
        []            -> pure st
        (team, _) : _ -> do
          -- Shuffle the players who fit, then take the first the machine
          -- accepts. That is a uniform choice among accepted picks, and it
          -- runs the completability check on one or two players per step
          -- rather than on the whole pool.
          fitting <- Gen.shuffle (filter (fits ctx team) (Map.keys (D.dcPool ctx)))
          case find (isJust . D.acceptedSlot ctx team) fitting of
            Nothing     -> Gen.discard
            Just player ->
              case runMachine st (D.MakePick team player) of
                Right (st', _) -> applyPicks (n - 1) st'
                Left _         -> Gen.discard
      _ -> pure st
  where
    fits ctx team player = isJust (D.slotForPick ctx team player)

startedFrom :: D.DraftPlan -> Gen D.DraftState
startedFrom plan = case runMachine D.WaitingToStart (D.StartDraft plan) of
  Right (state, _) -> pure state
  Left _           -> Gen.discard

-- | A plan together with a state reachable from 'D.WaitingToStart' under
-- that plan: not started, part-way through, or completed.
genReachable :: Gen (D.DraftPlan, D.DraftState)
genReachable = do
  plan <- genDraftPlan
  let total = length (D.dpOrder plan)
  state <- Gen.choice
    [ pure D.WaitingToStart
    , do n <- Gen.int (Range.linear 0 (total - 1))
         startedFrom plan >>= applyPicks n
    , startedFrom plan >>= applyPicks total
    ]
  pure (plan, state)

-- | A plan together with its completed draft.
genCompleted :: Gen (D.DraftPlan, D.DraftState)
genCompleted = do
  plan  <- genDraftPlan
  state <- startedFrom plan >>= applyPicks (length (D.dpOrder plan))
  case state of
    D.Complete _ -> pure (plan, state)
    _            -> Gen.discard

-- | Slots and positions for 'genSmallPlan': a catcher, a first baseman,
-- an outfielder, a utility hitter, both pitcher slots, and positions that
-- reach each of them, so position slots, the shared utility slot and the
-- two pitcher slots all compete for players.
smallSlots :: [RosterSlot]
smallSlots =
  [ SlotCatcher, SlotFirstBase, SlotOutfield, SlotUtility
  , SlotStartingPitcher, SlotReliefPitcher
  ]

smallPositions :: [Position]
smallPositions = [Pitcher, Catcher, FirstBase, LeftField, DesignatedHitter]

-- | A small plan whose pool may or may not be able to complete it, small
-- enough for 'completable' to search exhaustively. Pick numbers are
-- sequential and no team gets more picks than its roster holds, so the
-- only reason 'D.validatePlan' can refuse it is 'D.PlanInfeasible'. Half
-- the plans give every team a full roster of picks; the other half give
-- teams any number up to that, so some teams choose which slots to fill.
genSmallPlan :: Gen D.DraftPlan
genSmallPlan = do
  teamCount <- Gen.int (Range.linear 2 3)
  slots     <- Gen.list (Range.linear 1 3) (Gen.element smallSlots)
  let limits   = RosterLimits (Map.fromListWith (+) [(slot, 1) | slot <- slots])
      capacity = totalRosterSize limits
      teams    = [DbLeagueTeamId (fromIntegral t) | t <- [1 .. teamCount]]
  picksPer <- Gen.choice
    [ pure (map (const capacity) teams)
    , traverse (const (Gen.int (Range.linear 0 capacity))) teams
    ]
  let picksPer' = if sum picksPer == 0 then capacity : drop 1 picksPer else picksPer
  pickers   <- Gen.shuffle (concat (zipWith replicate picksPer' teams))
  counts    <- traverse (const (Gen.int (Range.linear 0 3))) smallPositions
  positions <- Gen.shuffle (concat (zipWith replicate counts smallPositions))
  let order = zipWith (\n t -> (t, DraftPickNumber n)) [1 ..] pickers
      pool  = Map.fromList (zip (map DbPlayerId [1 ..]) positions)
  pure (D.DraftPlan (DbLeagueConfigId 1) order pool limits)

-- | Free-form command generator. Teams and players are drawn from id
-- ranges that may or may not overlap with whatever plan produced the
-- state. That is deliberate: most generated commands hit rejection
-- paths, and the rejection-invariance property earns its keep on them.
genCommand :: Gen D.DraftCommand
genCommand = Gen.choice
  [ D.StartDraft <$> genDraftPlan
  , D.MakePick   <$> (DbLeagueTeamId <$> Gen.int64 (Range.linear 1 10))
                 <*> (DbPlayerId     <$> Gen.int64 (Range.linear 1 100))
  , pure D.EndDraft
  ]