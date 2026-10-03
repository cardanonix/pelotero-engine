module Pelotero.DraftSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Pelotero.Domain.Id
  ( DbLeagueConfigId(..)
  , DbLeagueTeamId(..)
  , DbPlayerId(..)
  , DraftPickNumber(..)
  )
import Pelotero.Domain.Position (Position(..))
import Pelotero.Domain.Roster (RosterLimits(..), RosterSlot(..))
import Pelotero.Draft
  ( DraftCommand(..)
  , DraftContext(..)
  , DraftError(..)
  , DraftEvent(..)
  , DraftPickEntry(..)
  , DraftPlan(..)
  , DraftState(..)
  , DraftSummary(..)
  , acceptedSlot
  , currentPicker
  , isPlayerAvailable
  , picksRemaining
  , slotForPick
  )
import qualified Pelotero.Draft.Machine as M

-- | Drive the machine and project results back into the pure
-- @Either DraftError (DraftState, [DraftEvent])@ shape that these
-- behavioural tests want to assert against.
runMachine
  :: DraftState
  -> DraftCommand
  -> Either DraftError (DraftState, [DraftEvent])
runMachine state cmd =
  case M.runDraftCommand (M.fromDraftState state) cmd of
    (Left err, _) ->
      Left err
    (Right evs, M.SomeDraftStateG _ st') ->
      Right (M.toDraftState st', evs)

spec :: Spec
spec = do
  describe "Pelotero.Draft (driven via Pelotero.Draft.Machine)" $ do
    let leagueId = DbLeagueConfigId 1
        team1    = DbLeagueTeamId 1
        team2    = DbLeagueTeamId 2
        plan     = DraftPlan
          { dpLeague = leagueId
          , dpOrder  = [ (team1, DraftPickNumber 1)
                       , (team2, DraftPickNumber 2)
                       ]
          , dpPool   = outfielders [100, 101, 102]
          , dpLimits = oneOutfieldOneUtility
          }

    it "rejects StartDraft with an empty plan" $
      runMachine
        WaitingToStart
        (StartDraft (DraftPlan leagueId [] Map.empty oneOutfieldOneUtility))
        `shouldBe` Left EmptyDraftPlan

    it "rejects StartDraft when pick numbers are not 1, 2, 3, ..." $
      runMachine
        WaitingToStart
        (StartDraft plan
          { dpOrder = [ (team1, DraftPickNumber 1)
                      , (team2, DraftPickNumber 3)
                      ]
          })
        `shouldBe` Left PlanPickNumbersNotSequential

    it "rejects StartDraft when a team has more picks than roster capacity" $
      runMachine
        WaitingToStart
        (StartDraft plan
          { dpOrder = [ (team1, DraftPickNumber 1)
                      , (team1, DraftPickNumber 2)
                      , (team1, DraftPickNumber 3)
                      ]
          })
        `shouldBe` Left (PlanOverfillsRoster team1 3 2)

    it "transitions WaitingToStart -> Drafting on a valid StartDraft" $
      case runMachine WaitingToStart (StartDraft plan) of
        Right (Drafting ctx, [DraftStarted lid teams]) -> do
          lid             `shouldBe` leagueId
          teams           `shouldBe` [team1, team2]
          dcRemaining ctx `shouldBe` dpOrder plan
        other -> expectationFailure ("got " <> show other)

    it "rejects StartDraft from Drafting" $
      case runMachine WaitingToStart (StartDraft plan) of
        Right (state, _) ->
          runMachine state (StartDraft plan)
            `shouldBe` Left DraftAlreadyStarted
        other -> expectationFailure ("setup: " <> show other)

    it "rejects MakePick from WaitingToStart" $
      runMachine WaitingToStart (MakePick team1 (DbPlayerId 100))
        `shouldBe` Left DraftNotStarted

    it "rejects EndDraft from WaitingToStart" $
      runMachine WaitingToStart EndDraft
        `shouldBe` Left DraftNotStarted

    it "rejects a pick by the wrong team" $
      case runMachine WaitingToStart (StartDraft plan) of
        Right (state, _) ->
          runMachine state (MakePick team2 (DbPlayerId 100))
            `shouldBe` Left (NotYourTurn team1 team2)
        other -> expectationFailure ("setup: " <> show other)

    it "rejects a pick of an already-drafted player" $
      case runMachine WaitingToStart (StartDraft plan) of
        Right (s1, _) ->
          case runMachine s1 (MakePick team1 (DbPlayerId 100)) of
            Right (s2, _) ->
              runMachine s2 (MakePick team2 (DbPlayerId 100))
                `shouldBe` Left (PlayerAlreadyDrafted (DbPlayerId 100))
            other -> expectationFailure ("first pick: " <> show other)
        other -> expectationFailure ("setup: " <> show other)

    it "rejects a pick of a player not in the pool" $
      case runMachine WaitingToStart (StartDraft plan) of
        Right (state, _) ->
          runMachine state (MakePick team1 (DbPlayerId 999))
            `shouldBe` Left (PlayerNotInPool (DbPlayerId 999))
        other -> expectationFailure ("setup: " <> show other)

    it "emits PickRecorded then DraftCompleted on the last pick" $ do
      let smallPlan = plan { dpPool = outfielders [100, 101] }
      case runMachine WaitingToStart (StartDraft smallPlan) of
        Right (s1, _) ->
          case runMachine s1 (MakePick team1 (DbPlayerId 100)) of
            Right (s2, _) ->
              case runMachine s2 (MakePick team2 (DbPlayerId 101)) of
                Right ( Complete summary
                      , [PickRecorded entry, DraftCompleted summary']
                      ) -> do
                  summary                  `shouldBe` summary'
                  dpePlayer entry          `shouldBe` DbPlayerId 101
                  length (dsPicks summary) `shouldBe` 2
                other -> expectationFailure ("last pick: " <> show other)
            other -> expectationFailure ("first pick: " <> show other)
        other -> expectationFailure ("setup: " <> show other)

    it "EndDraft from Drafting transitions to Complete" $
      case runMachine WaitingToStart (StartDraft plan) of
        Right (state, _) ->
          case runMachine state EndDraft of
            Right (Complete _, [DraftCompleted _]) -> pure ()
            other -> expectationFailure ("got " <> show other)
        other -> expectationFailure ("setup: " <> show other)

    it "rejects every command from Complete" $ do
      let smallPlan = plan
            { dpOrder = [(team1, DraftPickNumber 1)]
            , dpPool  = outfielders [100]
            }
      case runMachine WaitingToStart (StartDraft smallPlan) of
        Right (s1, _) ->
          case runMachine s1 (MakePick team1 (DbPlayerId 100)) of
            Right (completeSt@(Complete _), _) -> do
              runMachine completeSt (StartDraft plan)
                `shouldBe` Left DraftAlreadyComplete
              runMachine completeSt (MakePick team1 (DbPlayerId 200))
                `shouldBe` Left DraftAlreadyComplete
              runMachine completeSt EndDraft
                `shouldBe` Left DraftAlreadyComplete
            other -> expectationFailure ("complete setup: " <> show other)
        other -> expectationFailure ("start setup: " <> show other)

  describe "roster placement" $ do
    let leagueId = DbLeagueConfigId 1
        team1    = DbLeagueTeamId 1
        soloPlan pool = DraftPlan
          { dpLeague = leagueId
          , dpOrder  = [ (team1, DraftPickNumber 1)
                       , (team1, DraftPickNumber 2)
                       ]
          , dpPool   = pool
          , dpLimits = oneOutfieldOneUtility
          }
        pickSlots state players = case players of
          []       -> Right []
          p : rest -> case runMachine state (MakePick team1 (DbPlayerId p)) of
            Left err -> Left err
            Right (state', PickRecorded entry : _) ->
              (dpeSlot entry :) <$> pickSlots state' rest
            Right (_, _) -> Right []

    it "places the first outfielder at outfield and the second at utility" $
      case runMachine WaitingToStart (StartDraft (soloPlan (outfielders [100, 101]))) of
        Right (state, _) ->
          pickSlots state [100, 101] `shouldBe` Right [SlotOutfield, SlotUtility]
        other -> expectationFailure ("setup: " <> show other)

    it "rejects a player whose position has no slot in the limits" $ do
      -- Two left fielders, so the plan itself can be completed and the
      -- refusal comes from the pick, not from plan validation.
      let pool = Map.fromList
            [ (DbPlayerId 100, Pitcher)
            , (DbPlayerId 101, LeftField)
            , (DbPlayerId 102, LeftField)
            ]
      case runMachine WaitingToStart (StartDraft (soloPlan pool)) of
        Right (state, _) ->
          runMachine state (MakePick team1 (DbPlayerId 100))
            `shouldBe` Left (NoOpenSlot team1 (DbPlayerId 100))
        other -> expectationFailure ("setup: " <> show other)

    it "records the roster count in the context after a pick" $
      case runMachine WaitingToStart (StartDraft (soloPlan (outfielders [100, 101]))) of
        Right (s1, _) ->
          case runMachine s1 (MakePick team1 (DbPlayerId 100)) of
            Right (Drafting ctx, _) -> do
              slotForPick ctx team1 (DbPlayerId 101) `shouldBe` Just SlotUtility
              slotForPick ctx team1 (DbPlayerId 100) `shouldBe` Nothing
            other -> expectationFailure ("first pick: " <> show other)
        other -> expectationFailure ("setup: " <> show other)

  describe "completability" $ do
    let leagueId = DbLeagueConfigId 1
        team1    = DbLeagueTeamId 1
        team2    = DbLeagueTeamId 2
        -- Each team needs one catcher and one utility player. Two catchers
        -- and two designated hitters fill both rosters only if each team
        -- takes exactly one catcher.
        plan     = DraftPlan
          { dpLeague = leagueId
          , dpOrder  = [ (team1, DraftPickNumber 1)
                       , (team1, DraftPickNumber 2)
                       , (team2, DraftPickNumber 3)
                       , (team2, DraftPickNumber 4)
                       ]
          , dpPool   = Map.fromList
              [ (DbPlayerId 1, Catcher)
              , (DbPlayerId 2, Catcher)
              , (DbPlayerId 3, DesignatedHitter)
              , (DbPlayerId 4, DesignatedHitter)
              ]
          , dpLimits = oneCatcherOneUtility
          }
        afterFirstCatcher = case runMachine WaitingToStart (StartDraft plan) of
          Right (s1, _) -> runMachine s1 (MakePick team1 (DbPlayerId 1))
          other         -> other

    it "rejects StartDraft when the pool cannot fill every roster" $
      -- One catcher for two catcher slots: three of the four picks can be
      -- placed.
      runMachine
        WaitingToStart
        (StartDraft plan
          { dpPool = Map.fromList
              [ (DbPlayerId 1, Catcher)
              , (DbPlayerId 3, DesignatedHitter)
              , (DbPlayerId 4, DesignatedHitter)
              ]
          })
        `shouldBe` Left (PlanInfeasible 3 4)

    it "refuses a pick that fits but leaves another team unable to fill its roster" $
      case afterFirstCatcher of
        Right (s2, _) ->
          -- The second catcher fits team1's utility slot, but team2 would
          -- then have no catcher for its catcher slot.
          runMachine s2 (MakePick team1 (DbPlayerId 2))
            `shouldBe` Left (PickStrandsDraft team1 (DbPlayerId 2))
        other -> expectationFailure ("setup: " <> show other)

    it "acceptedSlot accepts the picks MakePick accepts and refuses the rest" $
      case afterFirstCatcher of
        Right (Drafting ctx, _) -> do
          slotForPick  ctx team1 (DbPlayerId 2) `shouldBe` Just SlotUtility
          acceptedSlot ctx team1 (DbPlayerId 2) `shouldBe` Nothing
          acceptedSlot ctx team1 (DbPlayerId 3) `shouldBe` Just SlotUtility
          -- Not team2's turn.
          acceptedSlot ctx team2 (DbPlayerId 3) `shouldBe` Nothing
        other -> expectationFailure ("setup: " <> show other)

  describe "inspection helpers" $ do
    let leagueId = DbLeagueConfigId 1
        team1    = DbLeagueTeamId 1
        team2    = DbLeagueTeamId 2
        plan     = DraftPlan
          { dpLeague = leagueId
          , dpOrder  = [ (team1, DraftPickNumber 1)
                       , (team2, DraftPickNumber 2)
                       ]
          , dpPool   = outfielders [100, 101]
          , dpLimits = oneOutfieldOneUtility
          }

    it "currentPicker is Nothing in WaitingToStart" $
      currentPicker WaitingToStart `shouldBe` Nothing

    it "currentPicker returns the head of dcRemaining in Drafting" $
      case runMachine WaitingToStart (StartDraft plan) of
        Right (state, _) ->
          currentPicker state `shouldBe` Just (team1, DraftPickNumber 1)
        other -> expectationFailure ("setup: " <> show other)

    it "picksRemaining is 0 outside Drafting" $
      picksRemaining WaitingToStart `shouldBe` 0

    it "picksRemaining counts dcRemaining in Drafting" $
      case runMachine WaitingToStart (StartDraft plan) of
        Right (state, _) -> picksRemaining state `shouldBe` 2
        other            -> expectationFailure ("setup: " <> show other)

    it "isPlayerAvailable reflects dcPool membership in Drafting" $
      case runMachine WaitingToStart (StartDraft plan) of
        Right (state, _) -> do
          isPlayerAvailable (DbPlayerId 100) state `shouldBe` True
          isPlayerAvailable (DbPlayerId 999) state `shouldBe` False
        other -> expectationFailure ("setup: " <> show other)

    it "isPlayerAvailable is False outside Drafting" $
      isPlayerAvailable (DbPlayerId 100) WaitingToStart `shouldBe` False

-- | One outfield slot and one utility slot: a two-player roster.
oneOutfieldOneUtility :: RosterLimits
oneOutfieldOneUtility = RosterLimits $ Map.fromList
  [ (SlotOutfield, 1)
  , (SlotUtility,  1)
  ]

-- | One catcher slot and one utility slot: a two-player roster.
oneCatcherOneUtility :: RosterLimits
oneCatcherOneUtility = RosterLimits $ Map.fromList
  [ (SlotCatcher, 1)
  , (SlotUtility, 1)
  ]

-- | A pool in which every listed player is a left fielder.
outfielders :: [Int] -> Map.Map DbPlayerId Position
outfielders ids = Map.fromList [(DbPlayerId (fromIntegral i), LeftField) | i <- ids]