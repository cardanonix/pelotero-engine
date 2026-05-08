-- | Fantasy points scoring. Pure functions over 'BattingStats' /
-- 'PitchingStats' from "Pelotero.Domain.Stats". The wire layer never appears
-- here — that's deliberate so scoring is testable in complete isolation.
--
-- 'Points' is exact rational. Sum-over-games semantics is associative; you
-- can re-aggregate a season from individual games without rounding drift,
-- which matters when a player's season total visibly disagrees with the sum
-- of their game lines.
module Pelotero.Domain.Scoring
  ( -- * Points
    Points(..)
  , zeroPoints
  , addPoints
  , subPoints
  , scalePoints
  , sumPoints
    -- * Multipliers
  , BattingMultipliers(..)
  , PitchingMultipliers(..)
  , LeagueScoring(..)
    -- * Scoring
  , scoreBatting
  , scorePitching
  ) where

import Data.Maybe (fromMaybe)

import Pelotero.Domain.Stats
  ( BattingStats(..)
  , PitchingStats(..)
  )
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))

--------------------------------------------------------------------------------
-- Points

-- | Fantasy points. Exact rational, not 'Double': sums across many games
-- accumulate; floating drift makes the season total disagree with the
-- per-game sum, which is the kind of bug players notice.
newtype Points = Points { unPoints :: Rational }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

zeroPoints :: Points
zeroPoints = Points 0

addPoints :: Points -> Points -> Points
addPoints (Points a) (Points b) = Points (a + b)

subPoints :: Points -> Points -> Points
subPoints (Points a) (Points b) = Points (a - b)

-- | Multiply a points value by a rational multiplier.
scalePoints :: Rational -> Points -> Points
scalePoints r (Points p) = Points (r * p)

sumPoints :: Foldable t => t Points -> Points
sumPoints = foldl' addPoints zeroPoints

--------------------------------------------------------------------------------
-- Multipliers

-- | League-configured batting multipliers. The shape is dictated by every
-- fantasy league we've seen — fields here are the things leagues actually
-- score on. Adding a new statistic is a structural change on purpose: it
-- forces explicit decisions about every league's score function.
--
-- Singles aren't a stat in the boxscore; we derive them as
-- @hits - (doubles + triples + homeRuns)@ at scoring time.
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

-- | League-configured pitching multipliers. \"Quality start\" is a synthetic
-- statistic — derived inside 'scorePitching' from IP and ER.
data PitchingMultipliers = PitchingMultipliers
  { pmWin           :: !Rational
  , pmSave          :: !Rational
  , pmQualityStart  :: !Rational
  , pmInningPitched :: !Rational  -- per inning (one inning = 3 outs)
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

-- | A league's complete scoring configuration: batting plus pitching
-- multipliers. Wired together at the league-config layer.
data LeagueScoring = LeagueScoring
  { lsBatting  :: !BattingMultipliers
  , lsPitching :: !PitchingMultipliers
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Scoring

-- | Score a batting line. Returns 'zeroPoints' for an empty stat line, which
-- is the right answer for "did not bat" — e.g. a starting pitcher in the AL.
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
    -- Singles are derived: hits minus extra-base hits.
    singles = max 0 $
      zeroIfNothing batHits
        - zeroIfNothing batDoubles
        - zeroIfNothing batTriples
        - zeroIfNothing batHomeRuns

    zeroIfNothing :: (BattingStats -> Maybe Int) -> Int
    zeroIfNothing f = fromMaybe 0 (f s)

-- | Score a pitching line. Inning-pitched scoring is exact: the multiplier
-- is per-inning, but innings can be fractional ("6.2" = 20 outs = 20\/3
-- innings). We compute the multiplier times the rational outs/3 to preserve
-- exactness end-to-end.
--
-- Quality start is the only derived stat: 6.0+ IP and 3 or fewer earned runs.
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

-- After the existing type definitions:

instance ToJSON BattingMultipliers where
  toJSON BattingMultipliers{..} = object
    [ "single"          .= bmSingle
    , "double"          .= bmDouble
    , "triple"          .= bmTriple
    , "homeRun"         .= bmHomeRun
    , "rbi"             .= bmRbi
    , "run"             .= bmRun
    , "baseOnBalls"     .= bmBaseOnBalls
    , "stolenBase"      .= bmStolenBase
    , "hitByPitch"      .= bmHitByPitch
    , "strikeOut"       .= bmStrikeOut
    , "caughtStealing"  .= bmCaughtStealing
    ]

instance FromJSON BattingMultipliers where
  parseJSON = withObject "BattingMultipliers" $ \o -> BattingMultipliers
    <$> o .: "single"
    <*> o .: "double"
    <*> o .: "triple"
    <*> o .: "homeRun"
    <*> o .: "rbi"
    <*> o .: "run"
    <*> o .: "baseOnBalls"
    <*> o .: "stolenBase"
    <*> o .: "hitByPitch"
    <*> o .: "strikeOut"
    <*> o .: "caughtStealing"

instance ToJSON PitchingMultipliers where
  toJSON PitchingMultipliers{..} = object
    [ "win"           .= pmWin
    , "save"          .= pmSave
    , "qualityStart"  .= pmQualityStart
    , "inningPitched" .= pmInningPitched
    , "strikeOut"     .= pmStrikeOut
    , "completeGame"  .= pmCompleteGame
    , "shutout"       .= pmShutout
    , "baseOnBalls"   .= pmBaseOnBalls
    , "hitsAllowed"   .= pmHitsAllowed
    , "earnedRun"     .= pmEarnedRun
    , "hitBatsman"    .= pmHitBatsman
    , "loss"          .= pmLoss
    ]

instance FromJSON PitchingMultipliers where
  parseJSON = withObject "PitchingMultipliers" $ \o -> PitchingMultipliers
    <$> o .: "win"
    <*> o .: "save"
    <*> o .: "qualityStart"
    <*> o .: "inningPitched"
    <*> o .: "strikeOut"
    <*> o .: "completeGame"
    <*> o .: "shutout"
    <*> o .: "baseOnBalls"
    <*> o .: "hitsAllowed"
    <*> o .: "earnedRun"
    <*> o .: "hitBatsman"
    <*> o .: "loss"

instance ToJSON LeagueScoring where
  toJSON LeagueScoring{..} = object
    [ "batting"  .= lsBatting
    , "pitching" .= lsPitching
    ]

instance FromJSON LeagueScoring where
  parseJSON = withObject "LeagueScoring" $ \o -> LeagueScoring
    <$> o .: "batting"
    <*> o .: "pitching"