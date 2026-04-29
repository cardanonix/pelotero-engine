module IntegrationTest.LeagueConfigRepoSpec (spec) where

import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import           Data.Time                (UTCTime(..), fromGregorian, secondsToDiffTime)
import           Test.Hspec

import qualified Pelotero.DB.LeagueConfig as LC
import           Pelotero.DB.LeagueConfig (LeagueConfigRow(..))
import           Pelotero.Domain.Roster   (LineupLimits(..), RosterLimits(..), RosterSlot(..))
import           Pelotero.Domain.Scoring
  ( BattingMultipliers(..), LeagueScoring(..), PitchingMultipliers(..) )

import IntegrationTest.Setup

spec :: Spec
spec = around withTestPool $
  describe "Pelotero.DB.LeagueConfig" $ do

    it "round-trips an inserted league config" $ \pool -> do
      mGot <- runRolledBack pool $ do
        lcid <- LC.insertLeagueConfigT (mkConfig "lc-rt")
        LC.getByIdT lcid
      case mGot of
        Just got -> do
          lcLeagueId got     `shouldBe` "lc-rt-league"
          lcCommissioner got `shouldBe` "test-commish"
          lcStatus got       `shouldBe` "draft"
        Nothing -> expectationFailure "round-trip read returned Nothing"

    it "preserves JSONB-encoded scoring config across round trip" $ \pool -> do
      mGot <- runRolledBack pool $ do
        let cfg = (mkConfig "lc-jsonb")
              { lcScoring = customScoring }
        lcid <- LC.insertLeagueConfigT cfg
        LC.getByIdT lcid
      case mGot of
        Just got -> lcScoring got `shouldBe` customScoring
        Nothing  -> expectationFailure "round-trip read returned Nothing"

    it "preserves JSONB-encoded roster and lineup limits" $ \pool -> do
      mGot <- runRolledBack pool $ do
        let cfg = (mkConfig "lc-limits")
              { lcRosterLimits = customRosterLimits
              , lcLineupLimits = customLineupLimits
              }
        lcid <- LC.insertLeagueConfigT cfg
        LC.getByIdT lcid
      case mGot of
        Just got -> do
          lcRosterLimits got `shouldBe` customRosterLimits
          lcLineupLimits got `shouldBe` customLineupLimits
        Nothing -> expectationFailure "round-trip read returned Nothing"

    it "getByLeagueId looks up by the natural league_id" $ \pool -> do
      mGot <- runRolledBack pool $ do
        _ <- LC.insertLeagueConfigT (mkConfig "lc-bylid")
        LC.getByLeagueIdT "lc-bylid-league"
      case mGot of
        Just got -> lcLeagueId got `shouldBe` "lc-bylid-league"
        Nothing  -> expectationFailure "expected to find by league_id"

    it "getByLeagueId returns Nothing for unknown ids" $ \pool -> do
      mGot <- runRolledBack pool $ LC.getByLeagueIdT "nonexistent-league-id"
      mGot `shouldBe` Nothing

    it "updateLeagueConfig overwrites the row" $ \pool -> do
      mGot <- runRolledBack pool $ do
        lcid <- LC.insertLeagueConfigT (mkConfig "lc-upd")
        let updated = (mkConfig "lc-upd")
              { lcCommissioner = "new-commish"
              , lcStatus       = "active"
              }
        LC.updateLeagueConfigT lcid updated
        LC.getByIdT lcid
      case mGot of
        Just got -> do
          lcCommissioner got `shouldBe` "new-commish"
          lcStatus got       `shouldBe` "active"
        Nothing -> expectationFailure "expected to find updated config"

mkConfig :: Text -> LeagueConfigRow
mkConfig tag = LeagueConfigRow
  { lcId            = Nothing
  , lcLeagueId      = tag <> "-league"
  , lcCommissioner  = "test-commish"
  , lcStatus        = "draft"
  , lcScoring       = zeroScoring
  , lcRosterLimits  = RosterLimits Map.empty
  , lcLineupLimits  = LineupLimits Map.empty
  , lcDraftAuto     = False
  , lcDraftStrategy = "serpentine"
  , lcDraftAutoAt   = Nothing
  , lcScoringStart  = epoch
  , lcScoringEnd    = epoch
  }
  where
    epoch = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

zeroScoring :: LeagueScoring
zeroScoring = LeagueScoring
  { lsBatting = BattingMultipliers
      { bmSingle = 0, bmDouble = 0, bmTriple = 0, bmHomeRun = 0
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