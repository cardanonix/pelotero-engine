module Pelotero.ScoreSpec (spec) where

import           Hedgehog            (Gen, forAll, tripping, (===))
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Hspec          (Spec, describe, it, shouldBe)
import           Test.Hspec.Hedgehog (hedgehog)

import           Pelotero.DB.BoxscoreEntry (BattingRow(..), PitchingRow(..))
import           Pelotero.Domain.Id        (DbGameId(..), DbPlayerId(..))
import           Pelotero.Domain.Scoring
                   ( BattingMultipliers(..)
                   , LeagueScoring(..)
                   , PitchingMultipliers(..)
                   , Points(..)
                   , addPoints
                   , scoreBatting
                   , zeroPoints
                   )
import           Pelotero.Domain.Stats
                   ( BattingStats(..)
                   , PitchingStats(..)
                   , emptyBatting
                   , parseInningsPitched
                   , renderInningsPitched
                   )
import           Pelotero.Score
                   ( PlayerScore(..)
                   , rowToBattingStats
                   , rowToPitchingStats
                   , scorePlayerPure
                   , sumBattingPoints
                   , sumPitchingPoints
                   )

spec :: Spec
spec = do
  describe "Domain.Stats innings round-trip (covered here because Score depends on it)" $
    it "parseInningsPitched . renderInningsPitched == id for outs >= 0" $
      hedgehog $ do
        outs <- forAll (Gen.int (Range.linear 0 999))
        tripping outs renderInningsPitched parseInningsPitched

  describe "rowToBattingStats" $
    it "round-trips against statsToBattingRow" $ hedgehog $ do
      s <- forAll genBattingLine
      rowToBattingStats (statsToBattingRow s) === s

  describe "rowToPitchingStats" $
    it "preserves outs and synthesizes a parseable IP text" $ hedgehog $ do
      outs <- forAll (Gen.int (Range.linear 0 60))
      let row = zeroPitchingRow
            { pitchingInningsPitchedOuts = Just (fromIntegral outs) }
          got = rowToPitchingStats row
      pitOuts got                                     === Just outs
      (parseInningsPitched =<< pitInningsPitched got) === Just outs

  describe "sumBattingPoints" $ do
    it "is zero for an empty list" $
      sumBattingPoints standardBatting [] `shouldBe` zeroPoints

    it "is additive over list concatenation (property)" $ hedgehog $ do
      xs <- forAll (Gen.list (Range.linear 0 5) genBattingRow)
      ys <- forAll (Gen.list (Range.linear 0 5) genBattingRow)
      let combined = sumBattingPoints standardBatting (xs ++ ys)
          parts    = addPoints (sumBattingPoints standardBatting xs)
                               (sumBattingPoints standardBatting ys)
      unPoints combined === unPoints parts

  describe "sumPitchingPoints" $ do
    it "is zero for an empty list" $
      sumPitchingPoints standardPitching [] `shouldBe` zeroPoints

    it "is additive over list concatenation (property)" $ hedgehog $ do
      xs <- forAll (Gen.list (Range.linear 0 5) genPitchingRow)
      ys <- forAll (Gen.list (Range.linear 0 5) genPitchingRow)
      let combined = sumPitchingPoints standardPitching (xs ++ ys)
          parts    = addPoints (sumPitchingPoints standardPitching xs)
                               (sumPitchingPoints standardPitching ys)
      unPoints combined === unPoints parts

  describe "scorePlayerPure" $ do
    it "scores zero for empty inputs" $ do
      let res = scorePlayerPure standardScoring [] [] (DbPlayerId 1)
      psBattingPoints  res `shouldBe` zeroPoints
      psPitchingPoints res `shouldBe` zeroPoints
      psTotalPoints    res `shouldBe` zeroPoints

    it "psTotalPoints = psBattingPoints + psPitchingPoints (property)" $
      hedgehog $ do
        bs <- forAll (Gen.list (Range.linear 0 5) genBattingRow)
        ps <- forAll (Gen.list (Range.linear 0 3) genPitchingRow)
        let res = scorePlayerPure standardScoring bs ps (DbPlayerId 1)
        psTotalPoints res
          === addPoints (psBattingPoints res) (psPitchingPoints res)

    it "scoring through a BattingRow matches scoring the BattingStats directly" $
      hedgehog $ do
        s <- forAll genBattingLine
        let row = statsToBattingRow s
            res = scorePlayerPure standardScoring [row] [] (DbPlayerId 1)
        psBattingPoints res === scoreBatting (lsBatting standardScoring) s

    it "credits a quality start through a PitchingRow: 6.0 IP, 3 ER" $ do
      -- 18 outs * pmInningPitched (3) + pmQualityStart (4) - 3 ER = 19
      let row = zeroPitchingRow
            { pitchingInningsPitchedOuts = Just 18
            , pitchingEarnedRuns         = Just 3
            }
          res = scorePlayerPure standardScoring [] [row] (DbPlayerId 1)
      psPitchingPoints res `shouldBe` Points 19
      psTotalPoints    res `shouldBe` Points 19

--------------------------------------------------------------------------------
-- Fixtures (mirrors Pelotero.Domain.ScoringSpec)

standardScoring :: LeagueScoring
standardScoring = LeagueScoring
  { lsBatting  = standardBatting
  , lsPitching = standardPitching
  }

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

--------------------------------------------------------------------------------
-- Generators and helpers

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

genBattingRow :: Gen BattingRow
genBattingRow = statsToBattingRow <$> genBattingLine

genPitchingRow :: Gen PitchingRow
genPitchingRow = do
  outs   <- Gen.int (Range.linear 0 27)
  ko     <- Gen.maybe (Gen.int (Range.linear 0 12))
  bb     <- Gen.maybe (Gen.int (Range.linear 0 5))
  hits   <- Gen.maybe (Gen.int (Range.linear 0 10))
  er     <- Gen.maybe (Gen.int (Range.linear 0 8))
  hbp    <- Gen.maybe (Gen.int (Range.linear 0 2))
  wins   <- Gen.maybe (Gen.int (Range.linear 0 1))
  losses <- Gen.maybe (Gen.int (Range.linear 0 1))
  saves  <- Gen.maybe (Gen.int (Range.linear 0 1))
  cg     <- Gen.maybe (Gen.int (Range.linear 0 1))
  sho    <- Gen.maybe (Gen.int (Range.linear 0 1))
  pure zeroPitchingRow
    { pitchingInningsPitchedOuts = Just (fromIntegral outs)
    , pitchingStrikeOuts         = fmap fromIntegral ko
    , pitchingBaseOnBalls        = fmap fromIntegral bb
    , pitchingHits               = fmap fromIntegral hits
    , pitchingEarnedRuns         = fmap fromIntegral er
    , pitchingHitBatsmen         = fmap fromIntegral hbp
    , pitchingWins               = fmap fromIntegral wins
    , pitchingLosses             = fmap fromIntegral losses
    , pitchingSaves              = fmap fromIntegral saves
    , pitchingCompleteGames      = fmap fromIntegral cg
    , pitchingShutouts           = fmap fromIntegral sho
    }

statsToBattingRow :: BattingStats -> BattingRow
statsToBattingRow s = BattingRow
  { battingGameId               = DbGameId 0
  , battingPlayerId             = DbPlayerId 0
  , battingTeamId               = Nothing
  , battingGamesPlayed          = i (batGamesPlayed s)
  , battingPlateAppearances     = i (batPlateAppearances s)
  , battingAtBats               = i (batAtBats s)
  , battingRuns                 = i (batRuns s)
  , battingHits                 = i (batHits s)
  , battingDoubles              = i (batDoubles s)
  , battingTriples              = i (batTriples s)
  , battingHomeRuns             = i (batHomeRuns s)
  , battingRbi                  = i (batRbi s)
  , battingBaseOnBalls          = i (batBaseOnBalls s)
  , battingIntentionalWalks     = i (batIntentionalWalks s)
  , battingStrikeOuts           = i (batStrikeOuts s)
  , battingStolenBases          = i (batStolenBases s)
  , battingCaughtStealing       = i (batCaughtStealing s)
  , battingHitByPitch           = i (batHitByPitch s)
  , battingSacBunts             = i (batSacBunts s)
  , battingSacFlies             = i (batSacFlies s)
  , battingGroundIntoDoublePlay = i (batGroundIntoDoublePlay s)
  , battingGroundIntoTriplePlay = i (batGroundIntoTriplePlay s)
  , battingLeftOnBase           = i (batLeftOnBase s)
  , battingTotalBases           = i (batTotalBases s)
  , battingFlyOuts              = i (batFlyOuts s)
  , battingGroundOuts           = i (batGroundOuts s)
  , battingCatchersInterference = i (batCatchersInterference s)
  , battingPickoffs             = i (batPickoffs s)
  }
  where
    i = fmap fromIntegral

zeroPitchingRow :: PitchingRow
zeroPitchingRow = PitchingRow
  { pitchingGameId                 = DbGameId 0
  , pitchingPlayerId               = DbPlayerId 0
  , pitchingTeamId                 = Nothing
  , pitchingGamesPlayed            = Nothing
  , pitchingGamesStarted           = Nothing
  , pitchingGamesFinished          = Nothing
  , pitchingCompleteGames          = Nothing
  , pitchingShutouts               = Nothing
  , pitchingWins                   = Nothing
  , pitchingLosses                 = Nothing
  , pitchingSaves                  = Nothing
  , pitchingSaveOpportunities      = Nothing
  , pitchingHolds                  = Nothing
  , pitchingBlownSaves             = Nothing
  , pitchingInningsPitchedOuts     = Nothing
  , pitchingBattersFaced           = Nothing
  , pitchingNumberOfPitches        = Nothing
  , pitchingStrikes                = Nothing
  , pitchingBalls                  = Nothing
  , pitchingHits                   = Nothing
  , pitchingDoubles                = Nothing
  , pitchingTriples                = Nothing
  , pitchingHomeRuns               = Nothing
  , pitchingRuns                   = Nothing
  , pitchingEarnedRuns             = Nothing
  , pitchingStrikeOuts             = Nothing
  , pitchingBaseOnBalls            = Nothing
  , pitchingIntentionalWalks       = Nothing
  , pitchingHitBatsmen             = Nothing
  , pitchingWildPitches            = Nothing
  , pitchingBalks                  = Nothing
  , pitchingPickoffs               = Nothing
  , pitchingFlyOuts                = Nothing
  , pitchingGroundOuts             = Nothing
  , pitchingAirOuts                = Nothing
  , pitchingInheritedRunners       = Nothing
  , pitchingInheritedRunnersScored = Nothing
  , pitchingStolenBases            = Nothing
  , pitchingCaughtStealing         = Nothing
  , pitchingAtBats                 = Nothing
  , pitchingRbi                    = Nothing
  , pitchingSacBunts               = Nothing
  , pitchingSacFlies               = Nothing
  , pitchingCatchersInterference   = Nothing
  , pitchingPassedBall             = Nothing
  }