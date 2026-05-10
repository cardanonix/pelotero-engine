module Pelotero.DraftSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Pelotero.Domain.Id
  ( DbLeagueConfigId(..)
  , DbLeagueTeamId(..)
  , DbPlayerId(..)
  , DraftPickNumber(..)
  )
import Pelotero.Draft
  ( DraftCommand(..)
  , DraftContext(..)
  , DraftError(..)
  , DraftEvent(..)
  , DraftPickEntry(..)
  , DraftPlan(..)
  , DraftState(..)
  , DraftSummary(..)
  , currentPicker
  , isPlayerAvailable
  , picksRemaining
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
          { dpLeague    = leagueId
          , dpOrder     = [ (team1, DraftPickNumber 1)
                          , (team2, DraftPickNumber 2)
                          ]
          , dpAvailable = Set.fromList
              [DbPlayerId 100, DbPlayerId 101, DbPlayerId 102]
          }

    it "rejects StartDraft with an empty plan" $
      runMachine
        WaitingToStart
        (StartDraft (DraftPlan leagueId [] Set.empty))
        `shouldBe` Left EmptyDraftPlan

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
      let smallPlan = DraftPlan
            { dpLeague    = leagueId
            , dpOrder     = [ (team1, DraftPickNumber 1)
                            , (team2, DraftPickNumber 2)
                            ]
            , dpAvailable = Set.fromList [DbPlayerId 100, DbPlayerId 101]
            }
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
      let smallPlan = DraftPlan
            { dpLeague    = leagueId
            , dpOrder     = [(team1, DraftPickNumber 1)]
            , dpAvailable = Set.fromList [DbPlayerId 100]
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

  describe "inspection helpers" $ do
    let leagueId = DbLeagueConfigId 1
        team1    = DbLeagueTeamId 1
        team2    = DbLeagueTeamId 2
        plan     = DraftPlan
          { dpLeague    = leagueId
          , dpOrder     = [ (team1, DraftPickNumber 1)
                          , (team2, DraftPickNumber 2)
                          ]
          , dpAvailable = Set.fromList [DbPlayerId 100, DbPlayerId 101]
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

    it "isPlayerAvailable reflects dcAvailable membership in Drafting" $
      case runMachine WaitingToStart (StartDraft plan) of
        Right (state, _) -> do
          isPlayerAvailable (DbPlayerId 100) state `shouldBe` True
          isPlayerAvailable (DbPlayerId 999) state `shouldBe` False
        other -> expectationFailure ("setup: " <> show other)

    it "isPlayerAvailable is False outside Drafting" $
      isPlayerAvailable (DbPlayerId 100) WaitingToStart `shouldBe` False