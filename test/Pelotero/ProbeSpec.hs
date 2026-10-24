-- | Probes for suspected defects. Each test asserts the behaviour the
-- spec requires; a failure is evidence for the matching issue.
module Pelotero.ProbeSpec (spec) where

import qualified Data.Map.Strict          as Map
import           Test.Hspec

import           Pelotero.Domain.Draft
import           Pelotero.Domain.Id       (DraftPickNumber (..))
import           Pelotero.Domain.Scoring
import           Pelotero.Domain.Stats


-- Copied from Pelotero.League.Setup.standardScoring.
pit :: PitchingMultipliers
pit = PitchingMultipliers
  { pmWin = 5, pmSave = 5, pmQualityStart = 4, pmInningPitched = 3
  , pmStrikeOut = 1, pmCompleteGame = 5, pmShutout = 5
  , pmBaseOnBalls = -1, pmHitsAllowed = 0, pmEarnedRun = -1
  , pmHitBatsman = -1, pmLoss = -3 }

bat :: BattingMultipliers
bat = BattingMultipliers
  { bmSingle = 1, bmDouble = 2, bmTriple = 3, bmHomeRun = 4
  , bmRbi = 1, bmRun = 1, bmBaseOnBalls = 1, bmStolenBase = 2
  , bmHitByPitch = 1, bmStrikeOut = -1, bmCaughtStealing = -1 }

qsOnly :: PitchingMultipliers
qsOnly = pit
  { pmWin = 0, pmSave = 0, pmInningPitched = 0, pmStrikeOut = 0
  , pmCompleteGame = 0, pmShutout = 0, pmBaseOnBalls = 0, pmHitsAllowed = 0
  , pmEarnedRun = 0, pmHitBatsman = 0, pmLoss = 0, pmQualityStart = 1 }

pickSums :: Ord a => [(a, DraftPickNumber)] -> Map.Map a Int
pickSums xs = Map.fromListWith (+) [ (t, n) | (t, DraftPickNumber n) <- xs ]

spec :: Spec
spec = describe "probes" $ do
  it "P1 no quality start for a pitcher who did not start" $ do
    let reliever = emptyPitching
          { pitGamesStarted = Just 0, pitOuts = Just 18, pitEarnedRuns = Just 0 }
    scorePitching qsOnly reliever `shouldBe` Points 0

  it "P2 inconsistent batting line is rejected, not silently floored" $ do
    let bad = emptyBatting
          { batHits = Just 1, batDoubles = Just 1, batHomeRuns = Just 1 }
    -- 1 hit cannot be both a double and a home run; scoring gives 6 points.
    scoreBatting bat bad `shouldBe` Points 0

  it "P3 a draft shorter than one round still produces picks" $
    generateDraftOrder SerpentineOrder 2 "ABC" `shouldNotBe` []

  it "P4 partial round is not dropped" $
    length (generateDraftOrder SerpentineOrder 7 "ABC") `shouldBe` 7

  it "P5 experimental snake is fair over a full cycle, odd team count" $ do
    let sums = Map.elems (pickSums (generateDraftOrder ExperimentalSnakeOrder 12 "ABC"))
    minimum sums `shouldBe` maximum sums

  it "P6 experimental snake is fair over a full cycle, even team count" $ do
    let sums = Map.elems (pickSums (generateDraftOrder ExperimentalSnakeOrder 16 "ABCD"))
    minimum sums `shouldBe` maximum sums
