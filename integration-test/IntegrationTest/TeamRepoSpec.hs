module IntegrationTest.TeamRepoSpec (spec) where

import Test.Hspec

import qualified Pelotero.DB.Team     as Team
import           Pelotero.DB.Team     (TeamRow(..))
import           Pelotero.DB.Provider (ProviderName(..))

import IntegrationTest.Setup

spec :: Spec
spec = around withTestPool $
  describe "Pelotero.DB.Team" $ do

    it "round-trips an inserted team" $ \pool -> do
      let row = TeamRow
            { teamRowId = Nothing
            , teamRowName = "Test Team"
            , teamRowAbbreviation = "TST"
            , teamRowLocationName = "Test City"
            , teamRowLastSyncedProvider = Just ProviderMLB
            , teamRowLastSyncedAt = Nothing
            }
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
          row = TeamRow
            { teamRowId = Nothing
            , teamRowName = "External Test Team"
            , teamRowAbbreviation = "EXT"
            , teamRowLocationName = "Externalia"
            , teamRowLastSyncedProvider = Just ProviderMLB
            , teamRowLastSyncedAt = Nothing
            }
      result <- runRolledBack pool $ do
        tid   <- Team.insertTeamT row
        Team.linkExternalIdT tid ProviderMLB extId
        found <- Team.lookupByExternalIdT ProviderMLB extId
        pure (tid, found)
      let (tid, found) = result
      found `shouldBe` Just tid

    it "upsertByExternalIdT inserts on first call, updates on second" $ \pool -> do
      let initial = TeamRow
            { teamRowId = Nothing
            , teamRowName = "Initial Name"
            , teamRowAbbreviation = "INI"
            , teamRowLocationName = "Initialville"
            , teamRowLastSyncedProvider = Just ProviderMLB
            , teamRowLastSyncedAt = Nothing
            }
          updated = initial { teamRowName = "Updated Name" }
      result <- runRolledBack pool $ do
        tid1  <- Team.upsertByExternalIdT ProviderMLB "99999" initial
        tid2  <- Team.upsertByExternalIdT ProviderMLB "99999" updated
        after <- Team.getByIdT tid2
        pure (tid1 == tid2, after)
      case result of
        (sameId, Just got) -> do
          sameId        `shouldBe` True
          teamRowName got `shouldBe` "Updated Name"
        (_, Nothing) ->
          expectationFailure "expected to find the upserted team"