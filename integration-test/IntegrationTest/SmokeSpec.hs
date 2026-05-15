{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : IntegrationTest.SmokeSpec
--
-- End-to-end smoke tests. Two cases:
--
-- 1. /Narrow/ — hand-built two-game scenario that pins the snapshot
--    invariant (lineup edits after the snapshot don't move the score).
--    Fast; doesn't touch the sync layer or the MLB fixtures.
--
-- 2. /Fixture-driven/ — full pipeline: sync rosters + schedule +
--    boxscores from the on-disk JSON fixtures, seed a league using
--    known players from those rosters, snapshot every day in the
--    range, score, edit lineup, re-score. Asserts the post-edit
--    score equals the pre-edit score across the whole window.
--
-- Determinism is enforced by: 'runMLBClientFixture' instead of HTTP,
-- 'runClockFixed' instead of @getCurrentTime@, and 'cleanDatabase'
-- between runs. The player IDs used in the fixture-driven test are
-- the same four MLB ids verified by 'IntegrationTest.SyncPlayersSpec';
-- if those fall out of the fixture, both tests fail at the same
-- lookup point.
module IntegrationTest.SmokeSpec (spec) where

import           Data.Maybe                     (catMaybes, listToMaybe)
import qualified Data.Text                      as T
import           Data.Time
                     ( Day
                     , UTCTime (..)
                     , fromGregorian
                     , secondsToDiffTime
                     , showGregorian
                     )
import           Test.Hspec

import           Effectful                      (Eff, IOE, runEff)
import qualified Effectful                      as E
import           Effectful.Error.Static         (Error, runErrorNoCallStack)

import qualified Pelotero.DB.BoxscoreEntry      as Box
import qualified Pelotero.DB.Game               as Game
import qualified Pelotero.DB.LeagueConfig       as LC
import qualified Pelotero.DB.LeagueTeam         as LT
import qualified Pelotero.DB.LineupSlot         as LS
import qualified Pelotero.DB.Player             as P
import qualified Pelotero.DB.Team               as Tm
import           Pelotero.DB.BoxscoreEntry      (BattingRow (..))
import           Pelotero.DB.Game               (LoadedGameRow (..))
import           Pelotero.DB.LeagueConfig       (LeagueConfigRow (..))
import           Pelotero.DB.LeagueTeam         (LeagueTeamRow (..))
import           Pelotero.DB.LineupSlot         (LineupSlotRow (..))
import           Pelotero.DB.Pool               (DBError, Pool)
import           Pelotero.DB.Provider           (ProviderName (..))
import           Pelotero.Domain.Id
                     ( DbGameId
                     , DbPlayerId
                     , DbTeamId
                     , GameId
                     )
import           Pelotero.Domain.Scoring        (Points (..))
import           Pelotero.MLB.Fetch             (FetchedRosters (..), FetchedSchedule (..))
import qualified Pelotero.Provider.ExternalId   as ExtId

import           Pelotero.Effects.BoxscoreEntry  (BoxscoreEntry, runBoxscoreEntryDB)
import           Pelotero.Effects.Clock          (Clock, runClockFixed)
import           Pelotero.Effects.Database       (Database, runDatabasePool, runTx)
import           Pelotero.Effects.FetchLog       (FetchLog, runFetchLogDB)
import qualified Pelotero.Effects.Games          as Games
import           Pelotero.Effects.Games          (Games, runGamesDB)
import           Pelotero.Effects.LeagueConfig   (LeagueConfig, runLeagueConfigDB)
import           Pelotero.Effects.LeagueTeam     (LeagueTeam, runLeagueTeamDB)
import           Pelotero.Effects.LineupSlot     (LineupSlot, runLineupSlotDB)
import           Pelotero.Effects.LineupSnapshot (LineupSnapshot, runLineupSnapshotDB)
import           Pelotero.Effects.Logging        (Logging, runLoggingDiscard)
import qualified Pelotero.Effects.MLBClient      as MLB
import           Pelotero.Effects.MLBClient
                     ( MLBClient
                     , defaultFixture
                     , runMLBClientFixture
                     )
import           Pelotero.Effects.Players        (Players, runPlayersDB)
import           Pelotero.Effects.Teams          (Teams, runTeamsDB)

import qualified Pelotero.Lineup.Snapshot        as Snap
import           Pelotero.Score
                     ( LeagueScore (..)
                     , TeamScore (..)
                     , scoreLeague
                     )
import qualified Pelotero.Sync.Boxscores         as SyncBox
import qualified Pelotero.Sync.Players           as SyncPlayers
import qualified Pelotero.Sync.Schedule          as SyncSchedule

import           IntegrationTest.Fixtures
                     ( mkGameRow
                     , mkLeagueConfigRow
                     , mkPlayerRow
                     , mkTeamRow
                     , singlesOnlyScoring
                     )
import           IntegrationTest.Setup           (cleanDatabase, runEffectsOrFail)

-- ---------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------

spec :: SpecWith Pool
spec = describe "Smoke" $ do

  it "narrow: snapshot pins scoring across a lineup edit (hand-built data)"
     $ \pool -> do
    cleanDatabase pool

    lcid <- runStack pool $ do
      lcid <- runTx (LC.insertLeagueConfigT smokeConfig)
      ltid <- runTx $ LT.insertLeagueTeamT LeagueTeamRow
                { ltId             = Nothing
                , ltLeagueConfigId = lcid
                , ltTeamKey        = "smoke-team-key"
                , ltName           = "smoke-team-name"
                , ltOwner          = "smoke-team-owner"
                }

      pidA <- runTx (P.insertPlayerT (mkPlayerRow "smoke-a"))
      pidB <- runTx (P.insertPlayerT (mkPlayerRow "smoke-b"))
      pidC <- runTx (P.insertPlayerT (mkPlayerRow "smoke-c"))

      atid <- runTx (Tm.insertTeamT (mkTeamRow "Smoke Away" "SMA"))
      htid <- runTx (Tm.insertTeamT (mkTeamRow "Smoke Home" "SMH"))

      gid1 <- runTx $ Game.insertGameT
                (mkGameRow (fromGregorian 2025 4 5) atid htid)
      gid2 <- runTx $ Game.insertGameT
                (mkGameRow (fromGregorian 2025 4 6) atid htid)

      runTx $ Box.upsertBattingT (hits gid1 pidA atid 1)
      runTx $ Box.upsertBattingT (hits gid1 pidB atid 1)
      runTx $ Box.upsertBattingT (hits gid2 pidB atid 2)
      runTx $ Box.upsertBattingT (hits gid2 pidC atid 3)

      runTx $ LS.addSlotT (LineupSlotRow ltid "outfield" pidA)
      runTx $ LS.addSlotT (LineupSlotRow ltid "outfield" pidB)
      _ <- Snap.snapshotLineupsForTeam ltid gid1

      runTx $ LS.removeSlotT ltid pidA
      runTx $ LS.addSlotT (LineupSlotRow ltid "outfield" pidC)
      _ <- Snap.snapshotLineupsForTeam ltid gid2

      pure lcid

    score1 <- runStack pool (scoreLeague lcid)
    case score1 >>= listToMaybe . lscTeams of
      Nothing   -> expectationFailure "scoreLeague returned no team"
      Just team -> tsTotalPoints team `shouldBe` Points 7

    score2 <- runStack pool (scoreLeague lcid)
    score2 `shouldBe` score1

  it "fixture-driven: full pipeline scores deterministically across a lineup edit"
     $ \pool -> do
    cleanDatabase pool

    seeded <- runStack pool $ do
      -- Phase 1: sync everything from fixtures on disk.
      syncRostersOrFail
      syncScheduleOrFail rangeStart rangeEnd
      gameIds <- collectGameIds rangeStart rangeEnd
      _ <- SyncBox.syncBoxscores ProviderMLB gameIds

      -- Phase 2: seed a single-team league pinned to the sync window.
      lcid <- runTx (LC.insertLeagueConfigT pipelineConfig)
      ltid <- runTx $ LT.insertLeagueTeamT LeagueTeamRow
                { ltId             = Nothing
                , ltLeagueConfigId = lcid
                , ltTeamKey        = "pipeline-team-key"
                , ltName           = "Pipeline Team"
                , ltOwner          = "pipeline-owner"
                }

      -- Phase 3: resolve known players from the synced roster.
      (pidStarter1, pidStarter2, pidReplacement) <- resolveLineupPlayers

      -- Phase 4: seed an initial lineup.
      runTx $ LS.addSlotT (LineupSlotRow ltid "outfield" pidStarter1)
      runTx $ LS.addSlotT (LineupSlotRow ltid "outfield" pidStarter2)

      -- Phase 5: snapshot every date in the range. After this, the
      -- snapshots pin the team's lineup as it stood at game time.
      _ <- traverse Snap.snapshotLineupsForDate
             (datesInRange rangeStart rangeEnd)

      pure (lcid, ltid, pidStarter1, pidReplacement)

    let (lcid, ltid, pidStarter1, pidReplacement) = seeded

    -- Phase 6: initial score.
    score1 <- runStack pool (scoreLeague lcid)
    case score1 of
      Nothing -> expectationFailure
                   "scoreLeague returned Nothing for the pipeline league"
      Just ls -> length (lscTeams ls) `shouldBe` 1

    -- Phase 7: edit lineup AFTER snapshots are written.
    runStack pool $ do
      runTx (LS.removeSlotT ltid pidStarter1)
      runTx $ LS.addSlotT (LineupSlotRow ltid "outfield" pidReplacement)

    -- Phase 8: re-score.
    score2 <- runStack pool (scoreLeague lcid)

    -- Snapshot invariant: every game was snapshotted before the edit,
    -- so the post-edit score must match the pre-edit score exactly.
    score2 `shouldBe` score1

-- ---------------------------------------------------------------------
-- Sync phases
-- ---------------------------------------------------------------------

rangeStart, rangeEnd :: Day
rangeStart = fromGregorian 2025 4 1
rangeEnd   = fromGregorian 2025 4 7

datesInRange :: Day -> Day -> [Day]
datesInRange a b = takeWhile (<= b) (iterate succ a)

syncRostersOrFail :: Eff SmokeStack ()
syncRostersOrFail = do
  fr <- MLB.fetchRosters 2025
  case fr of
    Left err ->
      E.liftIO (assertFail ("fixture roster fetch failed: " <> err))
    Right f -> do
      _ <- SyncPlayers.syncRosters
             ProviderMLB
             "2025"
             (frPayloadSha f)
             (frTeams   f)
             (frPlayers f)
      pure ()

syncScheduleOrFail :: Day -> Day -> Eff SmokeStack ()
syncScheduleOrFail from to_ = do
  sr <- MLB.fetchSchedule (showGregorian from) (showGregorian to_)
  case sr of
    Left err ->
      E.liftIO (assertFail ("fixture schedule fetch failed: " <> err))
    Right f -> do
      _ <- SyncSchedule.syncSchedule
             ProviderMLB
             (T.pack (showGregorian from) <> ".." <> T.pack (showGregorian to_))
             (fsPayloadSha f)
             (fsGames f)
      pure ()

collectGameIds :: Day -> Day -> Eff SmokeStack [GameId]
collectGameIds from to_ = do
  games <- Games.getGamesByDateRange from to_
  resolved <- traverse resolve games
  pure (catMaybes resolved)
  where
    resolve g = do
      mExt <- Games.getGameExternalId (lgrId g) ProviderMLB
      pure (mExt >>= ExtId.externalIdToGameId)

-- | Look up three known players from the 2025 fixture. The four MLB
-- ids verified by 'IntegrationTest.SyncPlayersSpec' are stable, so
-- the lookups are deterministic. If any goes missing the test fails
-- here with a clear message rather than later with a confusing
-- @scoreLeague@ outcome.
resolveLineupPlayers
  :: Eff SmokeStack (DbPlayerId, DbPlayerId, DbPlayerId)
resolveLineupPlayers = do
  pAltuve  <- requirePlayer "514888" "Altuve"
  pAlonso  <- requirePlayer "624413" "Alonso"
  pAlvarez <- requirePlayer "670541" "Alvarez"
  pure (pAltuve, pAlonso, pAlvarez)

requirePlayer :: T.Text -> String -> Eff SmokeStack DbPlayerId
requirePlayer extId tag = do
  mPid <- runTx (P.lookupByExternalIdT ProviderMLB extId)
  case mPid of
    Just pid -> pure pid
    Nothing  ->
      E.liftIO (assertFail
        (tag <> " (" <> T.unpack extId <> ") not in synced fixture"))

assertFail :: String -> IO a
assertFail msg = expectationFailure msg >> error "assertFail: unreachable"

-- ---------------------------------------------------------------------
-- Effect stack
-- ---------------------------------------------------------------------

type SmokeStack =
  '[ Players
   , Teams
   , Games
   , BoxscoreEntry
   , FetchLog
   , LineupSlot
   , LineupSnapshot
   , LeagueConfig
   , LeagueTeam
   , MLBClient
   , Database
   , Clock
   , Logging
   , Error DBError
   , IOE
   ]

runStack :: Pool -> Eff SmokeStack a -> IO a
runStack pool =
    runEffectsOrFail
  . runEff
  . runErrorNoCallStack @DBError
  . runLoggingDiscard
  . runClockFixed fixedTime
  . runDatabasePool pool
  . runMLBClientFixture (defaultFixture fixturesDir)
  . runLeagueTeamDB
  . runLeagueConfigDB
  . runLineupSnapshotDB
  . runLineupSlotDB
  . runFetchLogDB
  . runBoxscoreEntryDB
  . runGamesDB
  . runTeamsDB
  . runPlayersDB

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2025 4 15) (secondsToDiffTime 0)

fixturesDir :: FilePath
fixturesDir = "integration-test/fixtures/mlb"

-- ---------------------------------------------------------------------
-- League configs and helpers
-- ---------------------------------------------------------------------

smokeConfig :: LeagueConfigRow
smokeConfig = (mkLeagueConfigRow "smoke")
  { lcCommissioner = "smoke-commish"
  , lcStatus       = "active"
  , lcScoring      = singlesOnlyScoring
  , lcScoringStart = utc (fromGregorian 2025 4 1)
  , lcScoringEnd   = utc (fromGregorian 2025 4 30)
  }
  where
    utc d = UTCTime d (secondsToDiffTime 0)

pipelineConfig :: LeagueConfigRow
pipelineConfig = (mkLeagueConfigRow "pipeline")
  { lcCommissioner = "pipeline-commish"
  , lcStatus       = "active"
  , lcScoring      = singlesOnlyScoring
  , lcScoringStart = utc (fromGregorian 2025 4 1)
  , lcScoringEnd   = utc (fromGregorian 2025 4 30)
  }
  where
    utc d = UTCTime d (secondsToDiffTime 0)

hits :: DbGameId -> DbPlayerId -> DbTeamId -> Int -> BattingRow
hits gid pid tid n = BattingRow
  { battingGameId               = gid
  , battingPlayerId             = pid
  , battingTeamId               = Just tid
  , battingGamesPlayed          = Nothing
  , battingPlateAppearances     = Nothing
  , battingAtBats               = Nothing
  , battingRuns                 = Nothing
  , battingHits                 = Just (fromIntegral n)
  , battingDoubles              = Nothing
  , battingTriples              = Nothing
  , battingHomeRuns             = Nothing
  , battingRbi                  = Nothing
  , battingBaseOnBalls          = Nothing
  , battingIntentionalWalks     = Nothing
  , battingStrikeOuts           = Nothing
  , battingStolenBases          = Nothing
  , battingCaughtStealing       = Nothing
  , battingHitByPitch           = Nothing
  , battingSacBunts             = Nothing
  , battingSacFlies             = Nothing
  , battingGroundIntoDoublePlay = Nothing
  , battingGroundIntoTriplePlay = Nothing
  , battingLeftOnBase           = Nothing
  , battingTotalBases           = Nothing
  , battingFlyOuts              = Nothing
  , battingGroundOuts           = Nothing
  , battingCatchersInterference = Nothing
  , battingPickoffs             = Nothing
  }