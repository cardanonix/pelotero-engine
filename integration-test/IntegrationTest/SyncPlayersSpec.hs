{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

module IntegrationTest.SyncPlayersSpec (spec) where

import           Test.Hspec

import           Data.Text                  (Text)
import           Data.Time                  (UTCTime(..), fromGregorian, secondsToDiffTime)

import           Effectful                  (runEff)
import qualified Effectful                  as E
import           Effectful.Error.Static     (runErrorNoCallStack)

import qualified Pelotero.DB.FetchLog       as FL
import qualified Pelotero.DB.Player         as P
import qualified Pelotero.DB.Team           as Tm
import           Pelotero.DB.FetchLog       (FetchLogRow(..))
import           Pelotero.DB.Player         (PlayerRow(..))
import           Pelotero.DB.Pool           (DBError, Pool)
import           Pelotero.DB.Team           (TeamRow(..))
import           Pelotero.DB.Provider       (ProviderName(..))
import           Pelotero.Effects.Clock     (runClockFixed)
import           Pelotero.Effects.Database  (Database, runDatabasePool, runTx)
import           Pelotero.Effects.FetchLog  (runFetchLogDB)
import           Pelotero.Effects.Logging   (runLoggingDiscard)
import           Pelotero.Effects.MLBClient
                   ( defaultFixture, fetchRosters, runMLBClientFixture )
import           Pelotero.Effects.Players   (runPlayersDB)
import           Pelotero.Effects.Teams     (runTeamsDB)
import           Pelotero.MLB.Fetch         (FetchedRosters(..))
import qualified Pelotero.Sync.Players      as Sync

import           IntegrationTest.Setup
                   ( cleanDatabase
                   , runEffectsOrFail
                   )

spec :: SpecWith Pool
spec = describe "Pelotero.Sync.Players (with fixture interpreter)" $ do

  it "syncs teams and players from a fixture into the database" $ \pool -> do
    cleanDatabase pool

    result <- runEffectsOrFail
            . runEff
            . runErrorNoCallStack @DBError
            . runLoggingDiscard
            . runClockFixed fixedTime
            . runDatabasePool pool
            . runFetchLogDB
            . runTeamsDB
            . runPlayersDB
            . runMLBClientFixture (defaultFixture fixturesDir)
            $ do
                fr <- fetchRosters 2025
                fetched <- case fr of
                  Left err -> E.liftIO (expectationFailure
                                         ("fixture fetch failed: " <> err))
                               >> error "unreachable"
                  Right f  -> pure f

                sr <- Sync.syncRosters
                        ProviderMLB
                        "2025"
                        (frPayloadSha fetched)
                        (frTeams fetched)
                        (frPlayers fetched)

                mAstros <- lookupTeamRow   ProviderMLB "117"
                mMets   <- lookupTeamRow   ProviderMLB "121"
                mAltuve <- lookupPlayerRow ProviderMLB "514888"
                mAlonso <- lookupPlayerRow ProviderMLB "624413"
                mFetch  <- runTx (FL.getLastFetchT
                                    ProviderMLB "active-rosters" "2025")

                pure ( sr
                     , frPayloadSha fetched
                     , mAstros, mMets, mAltuve, mAlonso, mFetch
                     )

    let (syncResult, payloadSha, mAstros, mMets, mAltuve, mAlonso, mFetch) = result

    Sync.syncTeamsUpserted   syncResult `shouldBe` 2
    Sync.syncPlayersUpserted syncResult `shouldBe` 2
    Sync.syncFetchSha256     syncResult `shouldBe` payloadSha

    case mAstros of
      Just t  -> teamRowName t `shouldBe` "Houston Astros"
      Nothing -> expectationFailure "Astros not in DB"

    case mMets of
      Just t  -> teamRowName t `shouldBe` "New York Mets"
      Nothing -> expectationFailure "Mets not in DB"

    case mAltuve of
      Just p  -> do
        playerRowFirstName p `shouldBe` "Jose"
        playerRowLastName  p `shouldBe` "Altuve"
        playerRowPosition  p `shouldBe` Just "2B"
      Nothing -> expectationFailure "Altuve not in DB"

    case mAlonso of
      Just p  -> do
        playerRowFirstName p `shouldBe` "Pete"
        playerRowLastName  p `shouldBe` "Alonso"
        playerRowPosition  p `shouldBe` Just "1B"
      Nothing -> expectationFailure "Alonso not in DB"

    case mFetch of
      Just fl -> do
        fetchLogProvider      fl `shouldBe` ProviderMLB
        fetchLogResource      fl `shouldBe` "active-rosters"
        fetchLogScope         fl `shouldBe` "2025"
        fetchLogPayloadSha256 fl `shouldBe` payloadSha
        fetchLogRecordCount   fl `shouldBe` 2
      Nothing -> expectationFailure "fetch log not recorded"

lookupTeamRow
  :: Database E.:> es
  => ProviderName -> Text -> E.Eff es (Maybe TeamRow)
lookupTeamRow provider extId = do
  mTid <- runTx (Tm.lookupByExternalIdT provider extId)
  case mTid of
    Nothing  -> pure Nothing
    Just tid -> runTx (Tm.getByIdT tid)

lookupPlayerRow
  :: Database E.:> es
  => ProviderName -> Text -> E.Eff es (Maybe PlayerRow)
lookupPlayerRow provider extId = do
  mPid <- runTx (P.lookupByExternalIdT provider extId)
  case mPid of
    Nothing  -> pure Nothing
    Just pid -> runTx (P.getByIdT pid)

fixturesDir :: FilePath
fixturesDir = "integration-test/fixtures/mlb"

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2025 4 15) (secondsToDiffTime 0)