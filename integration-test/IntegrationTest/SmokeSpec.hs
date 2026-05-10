-- integration-test/IntegrationTest/SmokeSpec.hs
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module IntegrationTest.SmokeSpec (spec) where

import           Data.Maybe               (listToMaybe)
import           Data.Time                (UTCTime(..), fromGregorian, secondsToDiffTime)
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
import           Pelotero.DB.LeagueConfig  (LeagueConfigRow(..))
import           Pelotero.DB.LeagueTeam    (LeagueTeamRow(..))
import           Pelotero.DB.LineupSlot    (LineupSlotRow(..))
import           Pelotero.DB.Pool          (DBError, Pool)
import           Pelotero.Domain.Id        (DbGameId, DbPlayerId, DbTeamId)
import           Pelotero.Domain.Scoring   (Points(..))

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

import           IntegrationTest.Fixtures
                   ( mkGameRow
                   , mkLeagueConfigRow
                   , mkPlayerRow
                   , mkTeamRow
                   , singlesOnlyScoring
                   )
import           IntegrationTest.Setup
                   ( cleanDatabase
                   , runEffectsOrFail
                   )

spec :: SpecWith Pool
spec = describe "End-to-end smoke: lineup snapshots survive mid-period edits" $
  it "scores deterministically and uses the lineup that was active per game"
     $ \pool -> do
    cleanDatabase pool

    seeded <- runStack pool $ do
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

    let lcid = seeded

    mScore1 <- runStack pool (scoreLeague lcid)
    case mScore1 >>= listToMaybe . lscTeams of
      Nothing   -> expectationFailure "scoreLeague returned no team"
      Just team -> tsTotalPoints team `shouldBe` Points 7

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