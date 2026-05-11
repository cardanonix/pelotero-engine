{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

module IntegrationTest.SyncBoxscoresSpec (spec) where

import           Test.Hspec

import qualified Data.Text                       as T
import           Data.Text                       (Text)
import           Data.Time                       (UTCTime(..), fromGregorian, secondsToDiffTime)

import           Effectful                       (runEff)
import qualified Effectful                       as E
import           Effectful.Error.Static          (runErrorNoCallStack)

import qualified Pelotero.DB.BoxscoreEntry       as Box
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
                   ( MLBClient, defaultFixture, fetchRosters, fetchSchedule
                   , runMLBClientFixture )
import           Pelotero.Effects.Players        (runPlayersDB)
import           Pelotero.Effects.Teams          (runTeamsDB)
import           Pelotero.MLB.Fetch              (FetchedRosters(..), FetchedSchedule(..))
import qualified Pelotero.Sync.Boxscores         as SyncBox
import qualified Pelotero.Sync.Players           as SyncPlayers
import qualified Pelotero.Sync.Schedule          as SyncSchedule

import           IntegrationTest.Setup           (cleanDatabase, runEffectsOrFail)

-- | The roster fixture pins four batters: Alonso + Lindor (Mets) and
-- Altuve + Alvarez (Astros).  Every boxscore in the fixture set surfaces
-- exactly two of them via the `players` map (one side is the pinned team,
-- the other is empty).  The pinned players are all position players, so
-- their `"pitching": {}` payloads should not produce pitching rows once
-- `Pelotero.MLB.Convert.convertBoxscore` filters non-appearances.
pinnedBattersPerGame :: Int
pinnedBattersPerGame = 2

spec :: SpecWith Pool
spec = describe "Pelotero.Sync.Boxscores (with real-shape MLB fixtures)" $ do

  it "syncs batting rows for the pinned players in the first game" $ \pool -> do
    cleanDatabase pool

    (bsr, rows, mFetch, extId) <-
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
            rf <- fetchRostersOrFail
            _  <- SyncPlayers.syncRosters ProviderMLB "2025"
                    (frPayloadSha rf) (frTeams rf) (frPlayers rf)

            sf <- fetchScheduleOrFail
            _  <- SyncSchedule.syncSchedule ProviderMLB scheduleScope
                    (fsPayloadSha sf) (fsGames sf)

            let firstGame = case fsGames sf of
                              (g:_) -> g
                              []    -> error "empty schedule fixture"
                gid        = gameId firstGame
                GameId raw = gid
                ext        = T.pack (show raw)

            result <- SyncBox.syncBoxscores ProviderMLB [gid]

            mGameDb <- runTx (Gm.lookupByExternalIdT ProviderMLB ext)
            gameDb <- case mGameDb of
              Just g  -> pure g
              Nothing -> failureE ("first game missing in DB: " <> T.unpack ext)
            battingRows <- runTx (Box.getBattingForGameT gameDb)

            mfl <- runTx (FL.getLastFetchT ProviderMLB "boxscore" ext)
            pure (result, battingRows, mfl, ext)

    SyncBox.boxGamesSeen        bsr `shouldBe` 1
    SyncBox.boxGamesProcessed   bsr `shouldBe` 1
    SyncBox.boxGamesUnchanged   bsr `shouldBe` 0
    SyncBox.boxErrors           bsr `shouldBe` []
    SyncBox.boxBattingUpserted  bsr `shouldBe` pinnedBattersPerGame
    -- All four pinned players are position players whose `"pitching"`
    -- field is the empty object `{}`.  After filtering non-appearances,
    -- this must be zero.  If you see this fail with a non-zero count,
    -- check `pitchingHasAppearance` in `Pelotero.MLB.Convert`.
    SyncBox.boxPitchingUpserted bsr `shouldBe` 0

    length rows `shouldBe` pinnedBattersPerGame

    case mFetch of
      Just fl -> do
        fetchLogResource    fl `shouldBe` "boxscore"
        fetchLogScope       fl `shouldBe` extId
        fetchLogRecordCount fl `shouldBe` fromIntegral pinnedBattersPerGame
      Nothing -> expectationFailure "boxscore fetch log not recorded"

  it "short-circuits when the second sync sees the same bytes" $ \pool -> do
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
            rf <- fetchRostersOrFail
            _  <- SyncPlayers.syncRosters ProviderMLB "2025"
                    (frPayloadSha rf) (frTeams rf) (frPlayers rf)

            sf <- fetchScheduleOrFail
            _  <- SyncSchedule.syncSchedule ProviderMLB scheduleScope
                    (fsPayloadSha sf) (fsGames sf)

            let firstGame = case fsGames sf of
                              (g:_) -> g
                              []    -> error "empty schedule fixture"
                gid = gameId firstGame

            a <- SyncBox.syncBoxscores ProviderMLB [gid]
            b <- SyncBox.syncBoxscores ProviderMLB [gid]
            pure (a, b)

    SyncBox.boxGamesProcessed first  `shouldBe` 1
    SyncBox.boxGamesUnchanged first  `shouldBe` 0
    SyncBox.boxGamesProcessed second `shouldBe` 0
    SyncBox.boxGamesUnchanged second `shouldBe` 1
    SyncBox.boxErrors         second `shouldBe` []

  it "processes every game in the fixture in a single call" $ \pool -> do
    cleanDatabase pool

    (result, gameCount) <-
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
            rf <- fetchRostersOrFail
            _  <- SyncPlayers.syncRosters ProviderMLB "2025"
                    (frPayloadSha rf) (frTeams rf) (frPlayers rf)

            sf <- fetchScheduleOrFail
            _  <- SyncSchedule.syncSchedule ProviderMLB scheduleScope
                    (fsPayloadSha sf) (fsGames sf)

            let gids = map gameId (fsGames sf)
            r <- SyncBox.syncBoxscores ProviderMLB gids
            pure (r, length gids)

    -- Pin both directions: the fixture has 12 curated games, and every one
    -- contributes exactly two pinned-batter rows.
    gameCount                         `shouldBe` 12
    SyncBox.boxGamesSeen        result `shouldBe` gameCount
    SyncBox.boxGamesProcessed   result `shouldBe` gameCount
    SyncBox.boxBattingUpserted  result `shouldBe` pinnedBattersPerGame * gameCount
    SyncBox.boxPitchingUpserted result `shouldBe` 0
    SyncBox.boxErrors           result `shouldBe` []


fetchRostersOrFail
  :: ( E.IOE E.:> es )
  => E.Eff (Pelotero.Effects.MLBClient.MLBClient : es) FetchedRosters
fetchRostersOrFail = do
  rr <- fetchRosters 2025
  case rr of
    Right f  -> pure f
    Left err -> failureE ("roster fixture: " <> err)

fetchScheduleOrFail
  :: ( E.IOE E.:> es )
  => E.Eff (Pelotero.Effects.MLBClient.MLBClient : es) FetchedSchedule
fetchScheduleOrFail = do
  sr <- fetchSchedule scheduleStart scheduleEnd
  case sr of
    Right f  -> pure f
    Left err -> failureE ("schedule fixture: " <> err)

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