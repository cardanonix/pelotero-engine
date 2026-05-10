{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest.PlayerRepoSpec (spec) where

import           Test.Hspec

import qualified Pelotero.DB.Player       as Player
import qualified Pelotero.DB.Team         as Team
import           Pelotero.DB.Player       (PlayerRow(..))
import           Pelotero.DB.Pool         (Pool)
import           Pelotero.DB.Provider     (ProviderName(..))
import           Pelotero.DB.Team         (TeamRow(..))

import           IntegrationTest.Fixtures (mkPlayerRow, mkTeamRow)
import           IntegrationTest.Setup    (runRolledBack)

spec :: SpecWith Pool
spec = describe "Pelotero.DB.Player" $ do

  it "round-trips an inserted player" $ \pool -> do
    let row = (mkPlayerRow "test-player")
          { playerRowFirstName = "Test"
          , playerRowLastName  = "Player"
          , playerRowNameSlug  = "test-player"
          , playerRowPosition  = Just "1B"
          , playerRowBatSide   = Just 'R'
          }
    got <- runRolledBack pool $ do
      pid <- Player.insertPlayerT row
      Player.getByIdT pid
    case got of
      Just p -> do
        playerRowLastName p `shouldBe` "Player"
        playerRowPosition p `shouldBe` Just "1B"
      Nothing ->
        expectationFailure "round-trip read returned Nothing"

  it "links a player to a team via current_team_id" $ \pool -> do
    let teamRow = (mkTeamRow "Player Team" "PLT")
                    { teamRowLocationName = "Playerville" }
        rosterMember tid = (mkPlayerRow "roster-member")
          { playerRowFirstName     = "Roster"
          , playerRowLastName      = "Member"
          , playerRowNameSlug      = "roster-member"
          , playerRowPosition      = Just "OF"
          , playerRowBatSide       = Just 'L'
          , playerRowCurrentTeamId = Just tid
          }
    got <- runRolledBack pool $ do
      tid       <- Team.insertTeamT teamRow
      pid       <- Player.insertPlayerT (rosterMember tid)
      gotPlayer <- Player.getByIdT pid
      pure (tid, gotPlayer)
    case got of
      (tid, Just p) ->
        playerRowCurrentTeamId p `shouldBe` Just tid
      (_, Nothing) ->
        expectationFailure "round-trip read returned Nothing"

  it "upsertByExternalIdT inserts on first call, updates on second" $ \pool -> do
    let initial = (mkPlayerRow "initial-player")
          { playerRowFirstName = "Initial"
          , playerRowLastName  = "Player"
          , playerRowNameSlug  = "initial-player"
          , playerRowPosition  = Just "C"
          }
        updated = initial { playerRowFirstName = "Updated" }
    got <- runRolledBack pool $ do
      pid1   <- Player.upsertByExternalIdT ProviderMLB "p-99999" initial
      pid2   <- Player.upsertByExternalIdT ProviderMLB "p-99999" updated
      mAfter <- Player.getByIdT pid2
      pure (pid1 == pid2, mAfter)
    case got of
      (sameId, Just p) -> do
        sameId               `shouldBe` True
        playerRowFirstName p `shouldBe` "Updated"
      (_, Nothing) ->
        expectationFailure "expected to find the upserted player"