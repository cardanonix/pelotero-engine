{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module IntegrationTest.SmokeSpec (spec) where

import qualified Data.Map.Strict          as Map
import           Data.Maybe               (listToMaybe)
import           Data.Text                (Text)
import           Data.Time                (Day, UTCTime(..), fromGregorian, secondsToDiffTime)
import           Test.Hspec

import           Effectful                (Eff, IOE, runEff)
import           Effectful.Error.Static   (Error, runErrorNoCallStack)

import qualified Pelotero.DB.BoxscoreEntry as Box
import qualified Pelotero.DB.Game          as Game
import qualified Pelotero.DB.LeagueConfig  as LC
import qualified Pelotero.DB.LeagueTeam    as LT
import qualified Pelotero.DB.LineupSlot    as LS
import qualified Pelotero.DB.Player        as P
import qualified Pelotero.DB.Team          as Tm
import           Pelotero.DB.BoxscoreEntry (BattingRow(..))
import           Pelotero.DB.Game          (GameRow(..))
import           Pelotero.DB.LeagueConfig  (LeagueConfigRow(..))
import           Pelotero.DB.LeagueTeam    (LeagueTeamRow(..))
import           Pelotero.DB.LineupSlot    (LineupSlotRow(..))
import           Pelotero.DB.Player        (PlayerRow(..))
import           Pelotero.DB.Pool          (DBError, Pool)
import           Pelotero.DB.Team          (TeamRow(..))
import           Pelotero.DB.Provider      (ProviderName(..))
import           Pelotero.Domain.Id
                   ( DbGameId
                   , DbLeagueConfigId
                   , DbPlayerId
                   , DbTeamId
                   )
import           Pelotero.Domain.Roster    (LineupLimits(..), RosterLimits(..))
import           Pelotero.Domain.Scoring
                   ( BattingMultipliers(..)
                   , LeagueScoring(..)
                   , PitchingMultipliers(..)
                   , Points(..)
                   )

import           Pelotero.Effects.BoxscoreEntry  (BoxscoreEntry, runBoxscoreEntryDB)
import           Pelotero.Effects.Database       (Database, runDatabasePool, runTx)
import           Pelotero.Effects.LeagueConfig   (LeagueConfig, runLeagueConfigDB)
import           Pelotero.Effects.LeagueTeam     (LeagueTeam, runLeagueTeamDB)
import           Pelotero.Effects.LineupSlot     (LineupSlot, runLineupSlotDB)
import           Pelotero.Effects.LineupSnapshot (LineupSnapshot, runLineupSnapshotDB)
import           Pelotero.Effects.Logging        (Logging, runLoggingDiscard)
import qualified Pelotero.Lineup.Snapshot        as Snap
import           Pelotero.Score
                   ( LeagueScore(..)
                   , TeamScore(..)
                   , scoreLeague
                   )

import           IntegrationTest.Setup
                   ( cleanDatabase
                   , runEffectsOrFail
                   , withTestPool
                   )

spec :: Spec
spec = around withTestPool $
  describe "End-to-end smoke: lineup snapshots survive mid-period edits" $
    it "scores deterministically and uses the lineup that was active per game"
       $ \pool -> do
      cleanDatabase pool

      -- Phase 1: seed the world.
      --
      -- One fantasy team, three players A/B/C, two MLB games on consecutive days.
      -- Per-game batting stats are crafted so each lineup-game pair gives a
      -- distinct point total: only the snapshot-honoring implementation can
      -- produce the expected grand total of 7.
      --
      --   Lineup at game 1: [A, B]   (snapshotted)
      --   Lineup edited:    A removed, C added
      --   Lineup at game 2: [B, C]   (snapshotted)
      --
      --   Game 1 stats: A=1 hit, B=1 hit, C=(no row)
      --   Game 2 stats: A=(no row), B=2 hits, C=3 hits
      --
      --   Snapshot-honoring score:
      --     Game 1: A(1) + B(1) = 2
      --     Game 2: B(2) + C(3) = 5
      --     Total:  7
      --
      --   "Use current lineup [B,C] for both games" gives 6.
      --   "Use first lineup [A,B]   for both games" gives 4.
      seeded <- runStack pool $ do
        lcid <- runTx (LC.insertLeagueConfigT (mkConfig "smoke"))
        ltid <- runTx (LT.insertLeagueTeamT (mkTeamRow lcid "smoke-team"))

        pidA <- runTx (P.insertPlayerT (mkPlayer "smoke-a"))
        pidB <- runTx (P.insertPlayerT (mkPlayer "smoke-b"))
        pidC <- runTx (P.insertPlayerT (mkPlayer "smoke-c"))

        atid <- runTx (Tm.insertTeamT (mkMlbTeam "Smoke Away" "SMA"))
        htid <- runTx (Tm.insertTeamT (mkMlbTeam "Smoke Home" "SMH"))

        gid1 <- runTx $ Game.insertGameT
          (mkGame (fromGregorian 2025 4 5) atid htid)
        gid2 <- runTx $ Game.insertGameT
          (mkGame (fromGregorian 2025 4 6) atid htid)

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

      let lcid = seeded

      -- Phase 2: score once, assert the exact total only the
      -- snapshot-honoring implementation can produce.
      mScore1 <- runStack pool (scoreLeague lcid)
      case mScore1 >>= listToMaybe . lscTeams of
        Nothing   -> expectationFailure "scoreLeague returned no team"
        Just team -> tsTotalPoints team `shouldBe` Points 7

      -- Phase 3: re-score with no data changes.
      -- Result must be byte-identical (determinism).
      mScore2 <- runStack pool (scoreLeague lcid)
      mScore2 `shouldBe` mScore1

runStack
  :: Pool
  -> Eff
       '[ LeagueConfig
        , LeagueTeam
        , LineupSlot
        , LineupSnapshot
        , BoxscoreEntry
        , Database
        , Logging
        , Error DBError
        , IOE
        ]
       a
  -> IO a
runStack pool =
    runEffectsOrFail
  . runEff
  . runErrorNoCallStack @DBError
  . runLoggingDiscard
  . runDatabasePool pool
  . runBoxscoreEntryDB
  . runLineupSnapshotDB
  . runLineupSlotDB
  . runLeagueTeamDB
  . runLeagueConfigDB

-- Single-hit row builder. Everything else zero so the only point contribution
-- is `bmSingle * (hits - doubles - triples - homeRuns)`.
hits :: DbGameId -> DbPlayerId -> DbTeamId -> Int -> BattingRow
hits gid pid tid n = BattingRow
  { battingGameId = gid, battingPlayerId = pid, battingTeamId = Just tid
  , battingGamesPlayed = Nothing, battingPlateAppearances = Nothing
  , battingAtBats = Nothing, battingRuns = Nothing
  , battingHits = Just (fromIntegral n)
  , battingDoubles = Nothing, battingTriples = Nothing
  , battingHomeRuns = Nothing, battingRbi = Nothing
  , battingBaseOnBalls = Nothing, battingIntentionalWalks = Nothing
  , battingStrikeOuts = Nothing, battingStolenBases = Nothing
  , battingCaughtStealing = Nothing, battingHitByPitch = Nothing
  , battingSacBunts = Nothing, battingSacFlies = Nothing
  , battingGroundIntoDoublePlay = Nothing
  , battingGroundIntoTriplePlay = Nothing
  , battingLeftOnBase = Nothing, battingTotalBases = Nothing
  , battingFlyOuts = Nothing, battingGroundOuts = Nothing
  , battingCatchersInterference = Nothing, battingPickoffs = Nothing
  }

mkGame :: Day -> DbTeamId -> DbTeamId -> GameRow
mkGame d at ht = GameRow
  { gameRowId                 = Nothing
  , gameRowGameDate           = d
  , gameRowAwayTeamId         = at
  , gameRowHomeTeamId         = ht
  , gameRowLastSyncedProvider = Just ProviderMLB
  , gameRowLastSyncedAt       = Nothing
  }

mkMlbTeam :: Text -> Text -> TeamRow
mkMlbTeam name abbr =
  TeamRow Nothing name abbr "Anywhere" (Just ProviderMLB) Nothing

mkPlayer :: Text -> PlayerRow
mkPlayer tag = PlayerRow
  { playerRowId                 = Nothing
  , playerRowFirstName          = tag <> "-first"
  , playerRowLastName           = tag <> "-last"
  , playerRowNameSlug           = tag <> "-slug"
  , playerRowPosition           = Nothing
  , playerRowBatSide            = Nothing
  , playerRowPitchHand          = Nothing
  , playerRowActive             = True
  , playerRowCurrentTeamId      = Nothing
  , playerRowLastSyncedProvider = Just ProviderMLB
  , playerRowLastSyncedAt       = Nothing
  }

mkTeamRow :: DbLeagueConfigId -> Text -> LeagueTeamRow
mkTeamRow lcid tag = LeagueTeamRow
  { ltId             = Nothing
  , ltLeagueConfigId = lcid
  , ltTeamKey        = tag <> "-key"
  , ltName           = tag <> "-name"
  , ltOwner          = tag <> "-owner"
  }

mkConfig :: Text -> LeagueConfigRow
mkConfig tag = LeagueConfigRow
  { lcId            = Nothing
  , lcLeagueId      = tag <> "-league"
  , lcCommissioner  = "smoke-commish"
  , lcStatus        = "active"
  , lcScoring       = singlesOnlyScoring
  , lcRosterLimits  = RosterLimits Map.empty
  , lcLineupLimits  = LineupLimits Map.empty
  , lcDraftAuto     = False
  , lcDraftStrategy = "serpentine"
  , lcDraftAutoAt   = Nothing
  , lcScoringStart  = utc (fromGregorian 2025 4 1)
  , lcScoringEnd    = utc (fromGregorian 2025 4 30)
  }
  where
    utc d = UTCTime d (secondsToDiffTime 0)

singlesOnlyScoring :: LeagueScoring
singlesOnlyScoring = LeagueScoring
  { lsBatting = BattingMultipliers
      { bmSingle = 1, bmDouble = 0, bmTriple = 0, bmHomeRun = 0
      , bmRbi = 0, bmRun = 0, bmBaseOnBalls = 0, bmStolenBase = 0
      , bmHitByPitch = 0, bmStrikeOut = 0, bmCaughtStealing = 0
      }
  , lsPitching = PitchingMultipliers
      { pmWin = 0, pmSave = 0, pmQualityStart = 0, pmInningPitched = 0
      , pmStrikeOut = 0, pmCompleteGame = 0, pmShutout = 0
      , pmBaseOnBalls = 0, pmHitsAllowed = 0, pmEarnedRun = 0
      , pmHitBatsman = 0, pmLoss = 0
      }
  }