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
  , applyCommand
  , currentPicker
  , isPlayerAvailable
  , picksRemaining
  )

spec :: Spec
spec = do
  describe "Pelotero.Draft.applyCommand" $ do
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
      applyCommand
        (StartDraft (DraftPlan leagueId [] Set.empty))
        WaitingToStart
        `shouldBe` Left EmptyDraftPlan

    it "transitions WaitingToStart -> Drafting on a valid StartDraft" $
      case applyCommand (StartDraft plan) WaitingToStart of
        Right (Drafting ctx, [DraftStarted lid teams]) -> do
          lid             `shouldBe` leagueId
          teams           `shouldBe` [team1, team2]
          dcRemaining ctx `shouldBe` dpOrder plan
        other -> expectationFailure ("got " <> show other)

    it "rejects StartDraft from Drafting" $
      case applyCommand (StartDraft plan) WaitingToStart of
        Right (state, _) ->
          applyCommand (StartDraft plan) state
            `shouldBe` Left DraftAlreadyStarted
        other -> expectationFailure ("setup: " <> show other)

    it "rejects MakePick from WaitingToStart" $
      applyCommand (MakePick team1 (DbPlayerId 100)) WaitingToStart
        `shouldBe` Left DraftNotStarted

    it "rejects EndDraft from WaitingToStart" $
      applyCommand EndDraft WaitingToStart
        `shouldBe` Left DraftNotStarted

    it "rejects a pick by the wrong team" $
      case applyCommand (StartDraft plan) WaitingToStart of
        Right (state, _) ->
          applyCommand (MakePick team2 (DbPlayerId 100)) state
            `shouldBe` Left (NotYourTurn team1 team2)
        other -> expectationFailure ("setup: " <> show other)

    it "rejects a pick of an already-drafted player" $
      case applyCommand (StartDraft plan) WaitingToStart of
        Right (s1, _) ->
          case applyCommand (MakePick team1 (DbPlayerId 100)) s1 of
            Right (s2, _) ->
              applyCommand (MakePick team2 (DbPlayerId 100)) s2
                `shouldBe` Left (PlayerAlreadyDrafted (DbPlayerId 100))
            other -> expectationFailure ("first pick: " <> show other)
        other -> expectationFailure ("setup: " <> show other)

    it "rejects a pick of a player not in the pool" $
      case applyCommand (StartDraft plan) WaitingToStart of
        Right (state, _) ->
          applyCommand (MakePick team1 (DbPlayerId 999)) state
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
      case applyCommand (StartDraft smallPlan) WaitingToStart of
        Right (s1, _) ->
          case applyCommand (MakePick team1 (DbPlayerId 100)) s1 of
            Right (s2, _) ->
              case applyCommand (MakePick team2 (DbPlayerId 101)) s2 of
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
      case applyCommand (StartDraft plan) WaitingToStart of
        Right (state, _) ->
          case applyCommand EndDraft state of
            Right (Complete _, [DraftCompleted _]) -> pure ()
            other -> expectationFailure ("got " <> show other)
        other -> expectationFailure ("setup: " <> show other)

    it "rejects every command from Complete" $ do
      let smallPlan = DraftPlan
            { dpLeague    = leagueId
            , dpOrder     = [(team1, DraftPickNumber 1)]
            , dpAvailable = Set.fromList [DbPlayerId 100]
            }
      case applyCommand (StartDraft smallPlan) WaitingToStart of
        Right (s1, _) ->
          case applyCommand (MakePick team1 (DbPlayerId 100)) s1 of
            Right (completeSt@(Complete _), _) -> do
              applyCommand (StartDraft plan) completeSt
                `shouldBe` Left DraftAlreadyComplete
              applyCommand (MakePick team1 (DbPlayerId 200)) completeSt
                `shouldBe` Left DraftAlreadyComplete
              applyCommand EndDraft completeSt
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
      case applyCommand (StartDraft plan) WaitingToStart of
        Right (state, _) ->
          currentPicker state `shouldBe` Just (team1, DraftPickNumber 1)
        other -> expectationFailure ("setup: " <> show other)

    it "picksRemaining is 0 outside Drafting" $
      picksRemaining WaitingToStart `shouldBe` 0

    it "picksRemaining counts dcRemaining in Drafting" $
      case applyCommand (StartDraft plan) WaitingToStart of
        Right (state, _) -> picksRemaining state `shouldBe` 2
        other            -> expectationFailure ("setup: " <> show other)

    it "isPlayerAvailable reflects dcAvailable membership in Drafting" $
      case applyCommand (StartDraft plan) WaitingToStart of
        Right (state, _) -> do
          isPlayerAvailable (DbPlayerId 100) state `shouldBe` True
          isPlayerAvailable (DbPlayerId 999) state `shouldBe` False
        other -> expectationFailure ("setup: " <> show other)

    it "isPlayerAvailable is False outside Drafting" $
      isPlayerAvailable (DbPlayerId 100) WaitingToStart `shouldBe` False