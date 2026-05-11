{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

module IntegrationTest.SyncScheduleSpec (spec) where

import           Test.Hspec

import qualified Data.Text                       as T
import           Data.Text                       (Text)
import           Data.Time                       (UTCTime(..), fromGregorian, secondsToDiffTime)

import           Effectful                       (runEff)
import qualified Effectful                       as E
import           Effectful.Error.Static          (runErrorNoCallStack)

import qualified Pelotero.DB.FetchLog            as FL
import qualified Pelotero.DB.Game                as Gm
import           Pelotero.DB.FetchLog            (FetchLogRow(..))
import           Pelotero.DB.Pool                (DBError, Pool)
import           Pelotero.DB.Provider            (ProviderName(..))
import           Pelotero.Domain.Game            (gameId)
import           Pelotero.Domain.Id              (GameId(..))
import           Pelotero.Effects.BoxscoreEntry  (runBoxscoreEntryDB)
import           Pelotero.Effects.Clock          (runClockFixed)
import           Pelotero.Effects.Database       (runDatabasePool, runTx)
import           Pelotero.Effects.FetchLog       (runFetchLogDB)
import           Pelotero.Effects.Games          (runGamesDB)
import           Pelotero.Effects.Logging        (runLoggingDiscard)
import           Pelotero.Effects.MLBClient
                   ( defaultFixture, fetchRosters, fetchSchedule
                   , runMLBClientFixture )
import           Pelotero.Effects.Players        (runPlayersDB)
import           Pelotero.Effects.Teams          (runTeamsDB)
import           Pelotero.MLB.Fetch              (FetchedRosters(..), FetchedSchedule(..))
import qualified Pelotero.Sync.Players           as SyncPlayers
import qualified Pelotero.Sync.Schedule          as SyncSchedule

import           IntegrationTest.Setup           (cleanDatabase, runEffectsOrFail)

spec :: SpecWith Pool
spec = describe "Pelotero.Sync.Schedule (with real-shape MLB fixtures)" $ do

  it "syncs every game from the fixture once teams have been seeded" $ \pool -> do
    cleanDatabase pool

    (syncResult, n, mFirstInDb, mFetch) <-
      runEffectsOrFail
        . runEff
        . runErrorNoCallStack @DBError
        . runLoggingDiscard
        . runClockFixed fixedTime
        . runDatabasePool pool
        . runFetchLogDB
        . runTeamsDB
        . runPlayersDB
        . runGamesDB
        . runBoxscoreEntryDB
        . runMLBClientFixture (defaultFixture fixturesDir)
        $ do
            rr <- fetchRosters 2025
            rf <- case rr of
              Right f  -> pure f
              Left err -> failureE ("roster fixture: " <> err)
            _ <- SyncPlayers.syncRosters ProviderMLB "2025"
                   (frPayloadSha rf) (frTeams rf) (frPlayers rf)

            sr <- fetchSchedule scheduleStart scheduleEnd
            sf <- case sr of
              Right f  -> pure f
              Left err -> failureE ("schedule fixture: " <> err)
            result <- SyncSchedule.syncSchedule ProviderMLB scheduleScope
                        (fsPayloadSha sf) (fsGames sf)

            let games     = fsGames sf
                firstGame = case games of
                              (g:_) -> g
                              []    -> error "empty schedule fixture"
                GameId raw = gameId firstGame
                extId      = T.pack (show raw)

            mDb <- runTx (Gm.lookupByExternalIdT ProviderMLB extId)
            mfl <- runTx (FL.getLastFetchT ProviderMLB "schedule" scheduleScope)
            pure (result, length games, mDb, mfl)

    -- Pin the fixture size so accidental pruning fails loudly.
    n `shouldBe` 12
    SyncSchedule.schedGamesUpserted syncResult `shouldBe` n

    case mFirstInDb of
      Just _  -> pure ()
      Nothing -> expectationFailure
                   "first game from fixture not found in DB after sync"

    case mFetch of
      Just fl -> do
        fetchLogResource    fl `shouldBe` "schedule"
        fetchLogScope       fl `shouldBe` scheduleScope
        fetchLogRecordCount fl `shouldBe` fromIntegral n
      Nothing -> expectationFailure "schedule fetch log not recorded"

  it "short-circuits when the second sync sees the same SHA" $ \pool -> do
    cleanDatabase pool

    (first, second) <-
      runEffectsOrFail
        . runEff
        . runErrorNoCallStack @DBError
        . runLoggingDiscard
        . runClockFixed fixedTime
        . runDatabasePool pool
        . runFetchLogDB
        . runTeamsDB
        . runPlayersDB
        . runGamesDB
        . runBoxscoreEntryDB
        . runMLBClientFixture (defaultFixture fixturesDir)
        $ do
            rr <- fetchRosters 2025
            rf <- case rr of
              Right f  -> pure f
              Left err -> failureE ("roster fixture: " <> err)
            _ <- SyncPlayers.syncRosters ProviderMLB "2025"
                   (frPayloadSha rf) (frTeams rf) (frPlayers rf)

            sr <- fetchSchedule scheduleStart scheduleEnd
            sf <- case sr of
              Right f  -> pure f
              Left err -> failureE ("schedule fixture: " <> err)

            a <- SyncSchedule.syncSchedule ProviderMLB scheduleScope
                   (fsPayloadSha sf) (fsGames sf)
            b <- SyncSchedule.syncSchedule ProviderMLB scheduleScope
                   (fsPayloadSha sf) (fsGames sf)
            pure (a, b)

    SyncSchedule.schedGamesUpserted first  `shouldSatisfy` (> 0)
    SyncSchedule.schedGamesUpserted second `shouldBe` 0


failureE :: E.IOE E.:> es => String -> E.Eff es a
failureE msg = E.liftIO (expectationFailure msg) >> error "unreachable"

fixturesDir :: FilePath
fixturesDir = "integration-test/fixtures/mlb"

scheduleStart, scheduleEnd :: String
scheduleStart = "2025-04-01"
scheduleEnd   = "2025-04-07"

scheduleScope :: Text
scheduleScope = "2025-04-01..2025-04-07"

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2025 4 15) (secondsToDiffTime 0)