{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest.LeagueConfigRepoSpec (spec) where

import qualified Data.Map.Strict          as Map
import           Test.Hspec

import qualified Pelotero.DB.LeagueConfig as LC
import           Pelotero.DB.LeagueConfig (LeagueConfigRow(..))
import           Pelotero.DB.Pool         (Pool)
import           Pelotero.Domain.Roster   (LineupLimits(..), RosterLimits(..), RosterSlot(..))
import           Pelotero.Domain.Scoring
                   ( BattingMultipliers(..)
                   , LeagueScoring(..)
                   , PitchingMultipliers(..)
                   )

import           IntegrationTest.Fixtures (mkLeagueConfigRow)
import           IntegrationTest.Setup    (runRolledBack)

spec :: SpecWith Pool
spec = describe "Pelotero.DB.LeagueConfig" $ do

  it "round-trips an inserted league config" $ \pool -> do
    mGot <- runRolledBack pool $ do
      lcid <- LC.insertLeagueConfigT (mkLeagueConfigRow "lc-rt")
      LC.getByIdT lcid
    case mGot of
      Just got -> do
        LC.llcLeagueId     got `shouldBe` "lc-rt-league"
        LC.llcCommissioner got `shouldBe` "test-commish"
        LC.llcStatus       got `shouldBe` "draft"
      Nothing -> expectationFailure "round-trip read returned Nothing"

  it "preserves JSONB-encoded scoring config across round trip" $ \pool -> do
    mGot <- runRolledBack pool $ do
      let cfg = (mkLeagueConfigRow "lc-jsonb") { lcScoring = customScoring }
      lcid <- LC.insertLeagueConfigT cfg
      LC.getByIdT lcid
    case mGot of
      Just got -> LC.llcScoring got `shouldBe` customScoring
      Nothing  -> expectationFailure "round-trip read returned Nothing"

  it "preserves JSONB-encoded roster and lineup limits" $ \pool -> do
    mGot <- runRolledBack pool $ do
      let cfg = (mkLeagueConfigRow "lc-limits")
            { lcRosterLimits = customRosterLimits
            , lcLineupLimits = customLineupLimits
            }
      lcid <- LC.insertLeagueConfigT cfg
      LC.getByIdT lcid
    case mGot of
      Just got -> do
        LC.llcRosterLimits got `shouldBe` customRosterLimits
        LC.llcLineupLimits got `shouldBe` customLineupLimits
      Nothing -> expectationFailure "round-trip read returned Nothing"

  it "getByLeagueId looks up by the natural league_id" $ \pool -> do
    mGot <- runRolledBack pool $ do
      _ <- LC.insertLeagueConfigT (mkLeagueConfigRow "lc-bylid")
      LC.getByLeagueIdT "lc-bylid-league"
    case mGot of
      Just got -> LC.llcLeagueId got `shouldBe` "lc-bylid-league"
      Nothing  -> expectationFailure "expected to find by league_id"

  it "getByLeagueId returns Nothing for unknown ids" $ \pool -> do
    mGot <- runRolledBack pool $ LC.getByLeagueIdT "nonexistent-league-id"
    mGot `shouldBe` Nothing

  it "updateLeagueConfig overwrites the row" $ \pool -> do
    mGot <- runRolledBack pool $ do
      lcid <- LC.insertLeagueConfigT (mkLeagueConfigRow "lc-upd")
      let updated = (mkLeagueConfigRow "lc-upd")
            { lcCommissioner = "new-commish"
            , lcStatus       = "active"
            }
      LC.updateLeagueConfigT lcid updated
      LC.getByIdT lcid
    case mGot of
      Just got -> do
        LC.llcCommissioner got `shouldBe` "new-commish"
        LC.llcStatus       got `shouldBe` "active"
      Nothing -> expectationFailure "expected to find updated config"

customScoring :: LeagueScoring
customScoring = LeagueScoring
  { lsBatting = BattingMultipliers
      { bmSingle = 1, bmDouble = 2, bmTriple = 3, bmHomeRun = 4
      , bmRbi = 1, bmRun = 1, bmBaseOnBalls = 1, bmStolenBase = 2
      , bmHitByPitch = 1, bmStrikeOut = -1, bmCaughtStealing = -1
      }
  , lsPitching = PitchingMultipliers
      { pmWin = 5, pmSave = 5, pmQualityStart = 4, pmInningPitched = 3
      , pmStrikeOut = 1, pmCompleteGame = 5, pmShutout = 5
      , pmBaseOnBalls = -1, pmHitsAllowed = 0, pmEarnedRun = -1
      , pmHitBatsman = -1, pmLoss = -3
      }
  }

customRosterLimits :: RosterLimits
customRosterLimits = RosterLimits $ Map.fromList
  [ (SlotCatcher, 1), (SlotFirstBase, 1), (SlotSecondBase, 1)
  , (SlotThirdBase, 1), (SlotShortstop, 1), (SlotOutfield, 3)
  , (SlotUtility, 1), (SlotStartingPitcher, 2), (SlotReliefPitcher, 2)
  ]

customLineupLimits :: LineupLimits
customLineupLimits = LineupLimits $ Map.fromList
  [ (SlotCatcher, 1), (SlotFirstBase, 1), (SlotOutfield, 2)
  , (SlotStartingPitcher, 1)
  ]