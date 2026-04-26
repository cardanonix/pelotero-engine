-- | Round-trip tests for "Pelotero.DB.FetchLog".
module IntegrationTest.FetchLogRepoSpec (spec) where

import Test.Hspec

import qualified Pelotero.DB.FetchLog as FetchLog
import           Pelotero.DB.FetchLog (FetchLogRow(..))
import           Pelotero.DB.Provider (ProviderName(..))

import IntegrationTest.Setup

spec :: Spec
spec = around withTestPool $
  describe "Pelotero.DB.FetchLog" $ do

    it "records and retrieves the last fetch" $ \pool -> do
      let row1 = FetchLogRow
            { fetchLogId            = Nothing
            , fetchLogProvider      = ProviderMLB
            , fetchLogResource      = "rosters"
            , fetchLogScope         = "fetchlog-roundtrip-test"  -- unique to this test
            , fetchLogFetchedAt     = Nothing
            , fetchLogPayloadSha256 = "deadbeef"
            , fetchLogRecordCount   = 42
            }
      result <- runRolledBack pool $ do
        FetchLog.recordFetchT row1
        FetchLog.getLastFetchT ProviderMLB "rosters" "fetchlog-roundtrip-test"
      case result of
        Just row -> do
          fetchLogProvider      row `shouldBe` ProviderMLB
          fetchLogResource      row `shouldBe` "rosters"
          fetchLogScope         row `shouldBe` "fetchlog-roundtrip-test"
          fetchLogPayloadSha256 row `shouldBe` "deadbeef"
          fetchLogRecordCount   row `shouldBe` 42
        Nothing ->
          expectationFailure "expected a recorded fetch"

    it "getLastFetch returns the newest when multiple exist" $ \pool -> do
      let mkRow sha cnt = FetchLogRow
            { fetchLogId            = Nothing
            , fetchLogProvider      = ProviderMLB
            , fetchLogResource      = "schedule"
            , fetchLogScope         = "fetchlog-newest-test"
            , fetchLogFetchedAt     = Nothing
            , fetchLogPayloadSha256 = sha
            , fetchLogRecordCount   = cnt
            }
      result <- runRolledBack pool $ do
        FetchLog.recordFetchT (mkRow "older" 1)
        FetchLog.recordFetchT (mkRow "newer" 2)
        FetchLog.getLastFetchT ProviderMLB "schedule" "fetchlog-newest-test"
      case result of
        Just row -> do
          fetchLogPayloadSha256 row `shouldBe` "newer"
          fetchLogRecordCount   row `shouldBe` 2
        Nothing ->
          expectationFailure "expected a recorded fetch"

    it "getRecentFetches returns rows newest-first up to the limit" $ \pool -> do
      let mkRow sha = FetchLogRow
            { fetchLogId            = Nothing
            , fetchLogProvider      = ProviderMLB
            , fetchLogResource      = "boxscore"
            , fetchLogScope         = "fetchlog-recent-test"
            , fetchLogFetchedAt     = Nothing
            , fetchLogPayloadSha256 = sha
            , fetchLogRecordCount   = 1
            }
      rows <- runRolledBack pool $ do
        FetchLog.recordFetchT (mkRow "first")
        FetchLog.recordFetchT (mkRow "second")
        FetchLog.recordFetchT (mkRow "third")
        FetchLog.getRecentFetchesT ProviderMLB "boxscore" 2
      length rows `shouldBe` 2
      -- Newest first: "third" before "second"
      map fetchLogPayloadSha256 rows `shouldBe` ["third", "second"]

    it "getLastFetch returns Nothing when the resource has never been fetched" $ \pool -> do
      result <- runRolledBack pool $
        FetchLog.getLastFetchT ProviderMLB "rosters" "fetchlog-never-fetched-test"
      result `shouldBe` Nothing