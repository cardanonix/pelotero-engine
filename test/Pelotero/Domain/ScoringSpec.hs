module Pelotero.Domain.ScoringSpec (spec) where

import Hedgehog (Gen, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Pelotero.Domain.Scoring
  ( BattingMultipliers(..)
  , PitchingMultipliers(..)
  , Points(..)
  , addPoints
  , scoreBatting
  , scorePitching
  , zeroPoints
  )
import Pelotero.Domain.Stats
  ( BattingStats(..)
  , PitchingStats(..)
  , emptyBatting
  , emptyPitching
  )

spec :: Spec
spec = do
  describe "scoreBatting" $ do
    it "scores zero on an empty stat line" $
      scoreBatting standardBatting emptyBatting `shouldBe` zeroPoints

    it "scores a known line correctly" $ do
      let line = emptyBatting
            { batHits     = Just 1
            , batHomeRuns = Just 1
            , batRbi      = Just 2
            , batRuns     = Just 1
            , batAtBats   = Just 4
            }
      scoreBatting standardBatting line `shouldBe` Points 7

    it "is linear in hit multipliers (property)" $ hedgehog $ do
      line <- forAll genBattingLine
      let doubled = standardBatting
            { bmSingle  = 2 * bmSingle  standardBatting
            , bmDouble  = 2 * bmDouble  standardBatting
            , bmTriple  = 2 * bmTriple  standardBatting
            , bmHomeRun = 2 * bmHomeRun standardBatting
            }
          baseHit = scoreBatting (zeroExceptHits standardBatting) line
          dblHit  = scoreBatting (zeroExceptHits doubled)         line
      unPoints dblHit === 2 * unPoints baseHit

    it "is additive across two stat lines (property)" $ hedgehog $ do
      a <- forAll genBattingLine
      b <- forAll genBattingLine
      let combined = mergeBatting a b
          sumIndiv = addPoints (scoreBatting standardBatting a)
                               (scoreBatting standardBatting b)
      unPoints (scoreBatting standardBatting combined) === unPoints sumIndiv

  describe "scorePitching" $ do
    it "scores zero on an empty stat line" $
      scorePitching standardPitching emptyPitching `shouldBe` zeroPoints

    it "credits a quality start: 18 outs (6.0 IP), 3 ER" $ do
      let line = emptyPitching { pitOuts = Just 18, pitEarnedRuns = Just 3 }
      scorePitching standardPitching line `shouldBe` Points 19

    it "denies a quality start: 17 outs (5.2 IP), 3 ER" $ do
      let line = emptyPitching { pitOuts = Just 17, pitEarnedRuns = Just 3 }
      scorePitching standardPitching line `shouldBe` Points 14

    it "denies a quality start: 18 outs (6.0 IP), 4 ER" $ do
      let line = emptyPitching { pitOuts = Just 18, pitEarnedRuns = Just 4 }
      scorePitching standardPitching line `shouldBe` Points 14


standardBatting :: BattingMultipliers
standardBatting = BattingMultipliers
  { bmSingle         = 1
  , bmDouble         = 2
  , bmTriple         = 3
  , bmHomeRun        = 4
  , bmRbi            = 1
  , bmRun            = 1
  , bmBaseOnBalls    = 1
  , bmStolenBase     = 2
  , bmHitByPitch     = 1
  , bmStrikeOut      = -1
  , bmCaughtStealing = -1
  }

zeroExceptHits :: BattingMultipliers -> BattingMultipliers
zeroExceptHits m = m
  { bmRbi = 0, bmRun = 0, bmBaseOnBalls = 0, bmStolenBase = 0
  , bmHitByPitch = 0, bmStrikeOut = 0, bmCaughtStealing = 0
  }

standardPitching :: PitchingMultipliers
standardPitching = PitchingMultipliers
  { pmWin           = 5
  , pmSave          = 5
  , pmQualityStart  = 4
  , pmInningPitched = 3
  , pmStrikeOut     = 1
  , pmCompleteGame  = 5
  , pmShutout       = 5
  , pmBaseOnBalls   = -1
  , pmHitsAllowed   = 0
  , pmEarnedRun     = -1
  , pmHitBatsman    = -1
  , pmLoss          = -3
  }


genBattingLine :: Gen BattingStats
genBattingLine = do
  hits     <- Gen.int (Range.linear 0 5)
  doubles  <- Gen.int (Range.linear 0 hits)
  triples  <- Gen.int (Range.linear 0 (hits - doubles))
  homeRuns <- Gen.int (Range.linear 0 (hits - doubles - triples))
  let small = Gen.maybe (Gen.int (Range.linear 0 4))
  rbi  <- small
  runs <- small
  bb   <- small
  sb   <- small
  hbp  <- small
  ko   <- small
  cs   <- small
  pure emptyBatting
    { batHits           = Just hits
    , batDoubles        = Just doubles
    , batTriples        = Just triples
    , batHomeRuns       = Just homeRuns
    , batRbi            = rbi
    , batRuns           = runs
    , batBaseOnBalls    = bb
    , batStolenBases    = sb
    , batHitByPitch     = hbp
    , batStrikeOuts     = ko
    , batCaughtStealing = cs
    }

mergeBatting :: BattingStats -> BattingStats -> BattingStats
mergeBatting a b = emptyBatting
  { batHits           = addM batHits
  , batDoubles        = addM batDoubles
  , batTriples        = addM batTriples
  , batHomeRuns       = addM batHomeRuns
  , batRbi            = addM batRbi
  , batRuns           = addM batRuns
  , batBaseOnBalls    = addM batBaseOnBalls
  , batStolenBases    = addM batStolenBases
  , batHitByPitch     = addM batHitByPitch
  , batStrikeOuts     = addM batStrikeOuts
  , batCaughtStealing = addM batCaughtStealing
  }
  where
    addM f = case (f a, f b) of
      (Nothing, Nothing) -> Nothing
      (x, y)             -> Just (maybe 0 id x + maybe 0 id y)