{-# LANGUAGE OverloadedStrings #-}

module Pelotero.Domain.Stats
  ( BattingStats (..)
  , PitchingStats (..)
  , emptyBatting
  , emptyPitching
  , parseInningsPitched
  , renderInningsPitched
  ) where

import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.Read  as TR

data BattingStats = BattingStats
  { batGamesPlayed          :: Maybe Int
  , batPlateAppearances     :: Maybe Int
  , batAtBats               :: Maybe Int
  , batRuns                 :: Maybe Int
  , batHits                 :: Maybe Int
  , batDoubles              :: Maybe Int
  , batTriples              :: Maybe Int
  , batHomeRuns             :: Maybe Int
  , batRbi                  :: Maybe Int
  , batBaseOnBalls          :: Maybe Int
  , batIntentionalWalks     :: Maybe Int
  , batStrikeOuts           :: Maybe Int
  , batStolenBases          :: Maybe Int
  , batCaughtStealing       :: Maybe Int
  , batHitByPitch           :: Maybe Int
  , batSacBunts             :: Maybe Int
  , batSacFlies             :: Maybe Int
  , batGroundIntoDoublePlay :: Maybe Int
  , batGroundIntoTriplePlay :: Maybe Int
  , batLeftOnBase           :: Maybe Int
  , batTotalBases           :: Maybe Int
  , batFlyOuts              :: Maybe Int
  , batGroundOuts           :: Maybe Int
  , batCatchersInterference :: Maybe Int
  , batPickoffs             :: Maybe Int
  }
  deriving stock (Show, Eq)

-- | Per-pitcher per-game stats. 'pitOuts' is the canonical source of
-- truth for innings pitched; the wire-format Text representation is
-- parsed and discarded at convert time. Phase B.1 dropped the prior
-- 'pitInningsPitched :: Maybe Text' redundancy.
data PitchingStats = PitchingStats
  { pitGamesPlayed             :: Maybe Int
  , pitGamesStarted            :: Maybe Int
  , pitGamesFinished           :: Maybe Int
  , pitCompleteGames           :: Maybe Int
  , pitShutouts                :: Maybe Int
  , pitWins                    :: Maybe Int
  , pitLosses                  :: Maybe Int
  , pitSaves                   :: Maybe Int
  , pitSaveOpportunities       :: Maybe Int
  , pitHolds                   :: Maybe Int
  , pitBlownSaves              :: Maybe Int
  , pitOuts                    :: Maybe Int
  , pitBattersFaced            :: Maybe Int
  , pitNumberOfPitches         :: Maybe Int
  , pitStrikes                 :: Maybe Int
  , pitBalls                   :: Maybe Int
  , pitHits                    :: Maybe Int
  , pitDoubles                 :: Maybe Int
  , pitTriples                 :: Maybe Int
  , pitHomeRuns                :: Maybe Int
  , pitRuns                    :: Maybe Int
  , pitEarnedRuns              :: Maybe Int
  , pitStrikeOuts              :: Maybe Int
  , pitBaseOnBalls             :: Maybe Int
  , pitIntentionalWalks        :: Maybe Int
  , pitHitBatsmen              :: Maybe Int
  , pitWildPitches             :: Maybe Int
  , pitBalks                   :: Maybe Int
  , pitPickoffs                :: Maybe Int
  , pitFlyOuts                 :: Maybe Int
  , pitGroundOuts              :: Maybe Int
  , pitAirOuts                 :: Maybe Int
  , pitInheritedRunners        :: Maybe Int
  , pitInheritedRunnersScored  :: Maybe Int
  , pitStolenBases             :: Maybe Int
  , pitCaughtStealing          :: Maybe Int
  , pitAtBats                  :: Maybe Int
  , pitRbi                     :: Maybe Int
  , pitSacBunts                :: Maybe Int
  , pitSacFlies                :: Maybe Int
  , pitCatchersInterference    :: Maybe Int
  , pitPassedBall              :: Maybe Int
  }
  deriving stock (Show, Eq)

emptyBatting :: BattingStats
emptyBatting = BattingStats
  { batGamesPlayed          = Nothing
  , batPlateAppearances     = Nothing
  , batAtBats               = Nothing
  , batRuns                 = Nothing
  , batHits                 = Nothing
  , batDoubles              = Nothing
  , batTriples              = Nothing
  , batHomeRuns             = Nothing
  , batRbi                  = Nothing
  , batBaseOnBalls          = Nothing
  , batIntentionalWalks     = Nothing
  , batStrikeOuts           = Nothing
  , batStolenBases          = Nothing
  , batCaughtStealing       = Nothing
  , batHitByPitch           = Nothing
  , batSacBunts             = Nothing
  , batSacFlies             = Nothing
  , batGroundIntoDoublePlay = Nothing
  , batGroundIntoTriplePlay = Nothing
  , batLeftOnBase           = Nothing
  , batTotalBases           = Nothing
  , batFlyOuts              = Nothing
  , batGroundOuts           = Nothing
  , batCatchersInterference = Nothing
  , batPickoffs             = Nothing
  }

emptyPitching :: PitchingStats
emptyPitching = PitchingStats
  { pitGamesPlayed             = Nothing
  , pitGamesStarted            = Nothing
  , pitGamesFinished           = Nothing
  , pitCompleteGames           = Nothing
  , pitShutouts                = Nothing
  , pitWins                    = Nothing
  , pitLosses                  = Nothing
  , pitSaves                   = Nothing
  , pitSaveOpportunities       = Nothing
  , pitHolds                   = Nothing
  , pitBlownSaves              = Nothing
  , pitOuts                    = Nothing
  , pitBattersFaced            = Nothing
  , pitNumberOfPitches         = Nothing
  , pitStrikes                 = Nothing
  , pitBalls                   = Nothing
  , pitHits                    = Nothing
  , pitDoubles                 = Nothing
  , pitTriples                 = Nothing
  , pitHomeRuns                = Nothing
  , pitRuns                    = Nothing
  , pitEarnedRuns              = Nothing
  , pitStrikeOuts              = Nothing
  , pitBaseOnBalls             = Nothing
  , pitIntentionalWalks        = Nothing
  , pitHitBatsmen              = Nothing
  , pitWildPitches             = Nothing
  , pitBalks                   = Nothing
  , pitPickoffs                = Nothing
  , pitFlyOuts                 = Nothing
  , pitGroundOuts              = Nothing
  , pitAirOuts                 = Nothing
  , pitInheritedRunners        = Nothing
  , pitInheritedRunnersScored  = Nothing
  , pitStolenBases             = Nothing
  , pitCaughtStealing          = Nothing
  , pitAtBats                  = Nothing
  , pitRbi                     = Nothing
  , pitSacBunts                = Nothing
  , pitSacFlies                = Nothing
  , pitCatchersInterference    = Nothing
  , pitPassedBall              = Nothing
  }

-- | Parse the MLB wire IP convention into outs.
-- "6.2" -> 20 (six innings + two outs), "6" -> 18, "6.0" -> 18.
-- The fractional component must be 0, 1, or 2; anything else fails.
parseInningsPitched :: Text -> Maybe Int
parseInningsPitched t = case T.splitOn "." t of
  [whole]       -> outsFrom whole 0
  [whole, frac] -> do
    f <- parseNonNegInt frac
    if f <= 2 then outsFrom whole f else Nothing
  _ -> Nothing
  where
    outsFrom whole f = do
      w <- parseNonNegInt whole
      Just (w * 3 + f)

parseNonNegInt :: Text -> Maybe Int
parseNonNegInt s = case TR.decimal s of
  Right (n, rest) | T.null rest && n >= 0 -> Just n
  _                                       -> Nothing

-- | Inverse of 'parseInningsPitched' for human display. 20 -> "6.2".
-- Negative input rounds up to "0.0".
renderInningsPitched :: Int -> Text
renderInningsPitched n
  | n < 0     = "0.0"
  | otherwise = T.pack (show innings) <> "." <> T.pack (show outs)
  where
    (innings, outs) = n `divMod` 3