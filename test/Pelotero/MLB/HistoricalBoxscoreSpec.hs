{-# LANGUAGE OverloadedStrings #-}


module Pelotero.MLB.HistoricalBoxscoreSpec (spec) where

import           Data.Aeson                  (FromJSON, eitherDecodeStrict)
import qualified Data.ByteString             as BS

import           Test.Hspec
                     ( Expectation
                     , Spec
                     , describe
                     , expectationFailure
                     , it
                     , shouldBe
                     )

import           Pelotero.Domain.Id          (GameId (..), PlayerId (..))
import           Pelotero.Domain.Scoring
                     ( PitchingMultipliers (..)
                     , Points (..)
                     , scorePitching
                     )
import           Pelotero.Domain.Stats       (PitchingStats (..))
import           Pelotero.MLB.Convert        (BoxscoreEntry (..), convertBoxscore)
import           Pelotero.MLB.Wire.Boxscore  ()

spec :: Spec
spec = describe "real MLB boxscores through convertBoxscore and scorePitching" $ do










  describe "Paul Skenes vs COL, gamePk 823385, 5/12/2026" $ do
    let path = "test/fixtures/boxscore-823385.json"
        gid  = GameId   823385
        pid  = PlayerId 694973

    it "convert emits no warnings on this boxscore" $ do
      wbs <- decodeFixture path
      fst (convertBoxscore gid wbs) `shouldBe` []

    it "every score-relevant field matches the published box score" $
      withPitching path gid pid $ \ps -> do
        pitOuts        ps `shouldBe` Just 24
        pitStrikeOuts  ps `shouldBe` Just 10
        pitBaseOnBalls ps `shouldBe` Just  0
        pitEarnedRuns  ps `shouldBe` Just  0
        pitHitBatsmen  ps `shouldBe` Just  1
        pitWins        ps `shouldBe` Just  1
        pitLosses      ps `shouldBe` Just  0
        pitSaves       ps `shouldBe` Just  0
        pitCompleteGames ps `shouldBe` Just 0
        pitShutouts    ps `shouldBe` Just  0
        pitHits        ps `shouldBe` Just  2
        pitRuns        ps `shouldBe` Just  0
        pitHomeRuns    ps `shouldBe` Just  0

    it "scoring Skenes' converted line yields 42 points" $
      withPitching path gid pid $ \ps ->
        scorePitching standardPitching ps `shouldBe` Points 42










  describe "Zack Wheeler vs BOS, gamePk 824764, 5/12/2026" $ do
    let path = "test/fixtures/boxscore-824764.json"
        gid  = GameId   824764
        pid  = PlayerId 554430

    it "convert emits no warnings on this boxscore" $ do
      wbs <- decodeFixture path
      fst (convertBoxscore gid wbs) `shouldBe` []

    it "every score-relevant field matches the published box score" $
      withPitching path gid pid $ \ps -> do
        pitOuts        ps `shouldBe` Just 22
        pitStrikeOuts  ps `shouldBe` Just  4
        pitBaseOnBalls ps `shouldBe` Just  0
        pitEarnedRuns  ps `shouldBe` Just  1
        pitHitBatsmen  ps `shouldBe` Just  1
        pitWins        ps `shouldBe` Just  1
        pitLosses      ps `shouldBe` Just  0
        pitSaves       ps `shouldBe` Just  0
        pitCompleteGames ps `shouldBe` Just 0
        pitShutouts    ps `shouldBe` Just  0
        pitHits        ps `shouldBe` Just  6
        pitRuns        ps `shouldBe` Just  1
        pitHomeRuns    ps `shouldBe` Just  0

    it "scoring Wheeler's converted line yields 33 points" $
      withPitching path gid pid $ \ps ->
        scorePitching standardPitching ps `shouldBe` Points 33




decodeFixture :: FromJSON a => FilePath -> IO a
decodeFixture path = do
  bs <- BS.readFile path
  case eitherDecodeStrict bs of
    Right v  -> pure v
    Left err -> error ("fixture " <> path <> ": " <> err)




withPitching
  :: FilePath
  -> GameId
  -> PlayerId
  -> (PitchingStats -> Expectation)
  -> Expectation
withPitching path gid pid assertion = do
  wbs <- decodeFixture path
  let (_, entries) = convertBoxscore gid wbs
      matching     = [ e | e <- entries, boxPlayerId e == pid ]
  case matching of
    [entry] -> case boxPitching entry of
      Just ps -> assertion ps
      Nothing ->
        expectationFailure
          ("no pitching stats on entry for player " <> show pid)
    other ->
      expectationFailure
        ( "expected exactly one entry for player " <> show pid
       <> "; got " <> show (length other)
        )




standardPitching :: PitchingMultipliers
standardPitching = PitchingMultipliers
  { pmWin           =  5
  , pmSave          =  5
  , pmQualityStart  =  4
  , pmInningPitched =  3
  , pmStrikeOut     =  1
  , pmCompleteGame  =  5
  , pmShutout       =  5
  , pmBaseOnBalls   = -1
  , pmHitsAllowed   =  0
  , pmEarnedRun     = -1
  , pmHitBatsman    = -1
  , pmLoss          = -3
  }