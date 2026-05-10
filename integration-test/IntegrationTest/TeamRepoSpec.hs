{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest.TeamRepoSpec (spec) where

import           Test.Hspec

import qualified Pelotero.DB.Team         as Team
import           Pelotero.DB.Pool         (Pool)
import           Pelotero.DB.Team         (TeamRow(..))
import           Pelotero.DB.Provider     (ProviderName(..))

import           IntegrationTest.Fixtures (mkTeamRow)
import           IntegrationTest.Setup    (runRolledBack)

spec :: SpecWith Pool
spec = describe "Pelotero.DB.Team" $ do

  it "round-trips an inserted team" $ \pool -> do
    let row = (mkTeamRow "Test Team" "TST") { teamRowLocationName = "Test City" }
    got <- runRolledBack pool $ do
      tid <- Team.insertTeamT row
      Team.getByIdT tid
    case got of
      Just t  -> do
        teamRowName t         `shouldBe` "Test Team"
        teamRowAbbreviation t `shouldBe` "TST"
      Nothing -> expectationFailure "round-trip read returned Nothing"

  it "links and looks up an external id" $ \pool -> do
    let extId = "ext-team-link-test"
        row   = (mkTeamRow "External Test Team" "EXT")
                  { teamRowLocationName = "Externalia" }
    result <- runRolledBack pool $ do
      tid   <- Team.insertTeamT row
      Team.linkExternalIdT tid ProviderMLB extId
      found <- Team.lookupByExternalIdT ProviderMLB extId
      pure (tid, found)
    let (tid, found) = result
    found `shouldBe` Just tid

  it "upsertByExternalIdT inserts on first call, updates on second" $ \pool -> do
    let initial = (mkTeamRow "Initial Name" "INI")
                    { teamRowLocationName = "Initialville" }
        updated = initial { teamRowName = "Updated Name" }
    result <- runRolledBack pool $ do
      tid1   <- Team.upsertByExternalIdT ProviderMLB "99999" initial
      tid2   <- Team.upsertByExternalIdT ProviderMLB "99999" updated
      mAfter <- Team.getByIdT tid2
      pure (tid1 == tid2, mAfter)
    case result of
      (sameId, Just got) -> do
        sameId          `shouldBe` True
        teamRowName got `shouldBe` "Updated Name"
      (_, Nothing) ->
        expectationFailure "expected to find the upserted team"