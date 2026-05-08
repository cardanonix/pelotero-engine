module Pelotero.Domain.Scoring
  (
    Points(..)
  , zeroPoints
  , addPoints
  , subPoints
  , scalePoints
  , sumPoints

  , BattingMultipliers(..)
  , PitchingMultipliers(..)
  , LeagueScoring(..)

  , scoreBatting
  , scorePitching
  ) where

import Data.Maybe (fromMaybe)

import Pelotero.Domain.Stats
  ( BattingStats(..)
  , PitchingStats(..)
  )

newtype Points = Points { unPoints :: Rational }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

zeroPoints :: Points
zeroPoints = Points 0

addPoints :: Points -> Points -> Points
addPoints (Points a) (Points b) = Points (a + b)

subPoints :: Points -> Points -> Points
subPoints (Points a) (Points b) = Points (a - b)

scalePoints :: Rational -> Points -> Points
scalePoints r (Points p) = Points (r * p)

sumPoints :: Foldable t => t Points -> Points
sumPoints = foldl' addPoints zeroPoints

data BattingMultipliers = BattingMultipliers
  { bmSingle         :: !Rational
  , bmDouble         :: !Rational
  , bmTriple         :: !Rational
  , bmHomeRun        :: !Rational
  , bmRbi            :: !Rational
  , bmRun            :: !Rational
  , bmBaseOnBalls    :: !Rational
  , bmStolenBase     :: !Rational
  , bmHitByPitch     :: !Rational
  , bmStrikeOut      :: !Rational
  , bmCaughtStealing :: !Rational
  }
  deriving stock (Show, Eq)

data PitchingMultipliers = PitchingMultipliers
  { pmWin           :: !Rational
  , pmSave          :: !Rational
  , pmQualityStart  :: !Rational
  , pmInningPitched :: !Rational
  , pmStrikeOut     :: !Rational
  , pmCompleteGame  :: !Rational
  , pmShutout       :: !Rational
  , pmBaseOnBalls   :: !Rational
  , pmHitsAllowed   :: !Rational
  , pmEarnedRun     :: !Rational
  , pmHitBatsman    :: !Rational
  , pmLoss          :: !Rational
  }
  deriving stock (Show, Eq)

data LeagueScoring = LeagueScoring
  { lsBatting  :: !BattingMultipliers
  , lsPitching :: !PitchingMultipliers
  }
  deriving stock (Show, Eq)

scoreBatting :: BattingMultipliers -> BattingStats -> Points
scoreBatting m s = sumPoints
  [ scalePoints (bmSingle m)         (rationalPts singles)
  , scalePoints (bmDouble m)         (rationalPts (zeroIfNothing batDoubles))
  , scalePoints (bmTriple m)         (rationalPts (zeroIfNothing batTriples))
  , scalePoints (bmHomeRun m)        (rationalPts (zeroIfNothing batHomeRuns))
  , scalePoints (bmRbi m)            (rationalPts (zeroIfNothing batRbi))
  , scalePoints (bmRun m)            (rationalPts (zeroIfNothing batRuns))
  , scalePoints (bmBaseOnBalls m)    (rationalPts (zeroIfNothing batBaseOnBalls))
  , scalePoints (bmStolenBase m)     (rationalPts (zeroIfNothing batStolenBases))
  , scalePoints (bmHitByPitch m)     (rationalPts (zeroIfNothing batHitByPitch))
  , scalePoints (bmStrikeOut m)      (rationalPts (zeroIfNothing batStrikeOuts))
  , scalePoints (bmCaughtStealing m) (rationalPts (zeroIfNothing batCaughtStealing))
  ]
  where

    singles = max 0 $
      zeroIfNothing batHits
        - zeroIfNothing batDoubles
        - zeroIfNothing batTriples
        - zeroIfNothing batHomeRuns

    zeroIfNothing :: (BattingStats -> Maybe Int) -> Int
    zeroIfNothing f = fromMaybe 0 (f s)

scorePitching :: PitchingMultipliers -> PitchingStats -> Points
scorePitching m s = sumPoints
  [ scalePoints (pmWin m)          (rationalPts (i (pitWins s)))
  , scalePoints (pmSave m)         (rationalPts (i (pitSaves s)))
  , scalePoints (pmQualityStart m) (rationalPts qs)
  , Points (pmInningPitched m * toRational outs / 3)
  , scalePoints (pmStrikeOut m)    (rationalPts (i (pitStrikeOuts s)))
  , scalePoints (pmCompleteGame m) (rationalPts (i (pitCompleteGames s)))
  , scalePoints (pmShutout m)      (rationalPts (i (pitShutouts s)))
  , scalePoints (pmBaseOnBalls m)  (rationalPts (i (pitBaseOnBalls s)))
  , scalePoints (pmHitsAllowed m)  (rationalPts (i (pitHits s)))
  , scalePoints (pmEarnedRun m)    (rationalPts (i (pitEarnedRuns s)))
  , scalePoints (pmHitBatsman m)   (rationalPts (i (pitHitBatsmen s)))
  , scalePoints (pmLoss m)         (rationalPts (i (pitLosses s)))
  ]
  where
    i    = maybe 0 id
    outs = i (pitOuts s)
    er   = i (pitEarnedRuns s)
    qs   = if outs >= 18 && er <= 3 then 1 else 0

rationalPts :: Int -> Points
rationalPts = Points . toRational