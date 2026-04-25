-- | Per-game player statistics. 'Maybe Int' fields distinguish "stat absent"
-- (e.g. a pitcher's batting line on a day they didn't bat) from "stat present
-- and zero" (a hitter who went 0-for-3).
module Pelotero.Domain.Stats
  ( BattingStats(..)
  , PitchingStats(..)
  , emptyBatting
  , emptyPitching
    -- * Innings pitched
  , parseInningsPitched
  , renderInningsPitched
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

-- | Per-game batting line. Fields are 'Maybe' to preserve "did this player
-- bat at all?" information from the upstream feed.
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

-- | A batting line with every field absent. Useful as a parser default.
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

-- | Per-game pitching line. 'pitInningsPitched' is text because MLB reports
-- it as a fractional string (\"6.2\" = six and two-thirds innings) which is
-- *not* a decimal — converting blindly to Double silently corrupts data.
-- Use 'parseInningsPitched' to obtain an exact out-count.
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
  , pitInningsPitched          :: Maybe Text  -- e.g. "6.2"
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
  , pitInningsPitched          = Nothing
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

--------------------------------------------------------------------------------
-- Innings pitched

-- | Parse MLB's "innings pitched" string format. The number after the dot is
-- /outs/, not a decimal: \"6.2\" means 6 innings + 2 outs = 20 outs total.
-- Reading it as 'Double' (as the legacy code did) is a real bug — 6.2 as a
-- 'Double' is 6.2, but 6.2 IP as outs is 20\/3 ≈ 6.667.
--
-- The result is in /outs/ rather than innings so that arithmetic on totals
-- stays exact. Use 'renderInningsPitched' to go the other way for display.
parseInningsPitched :: Text -> Maybe Int
parseInningsPitched raw =
  let t = T.strip raw
  in case T.splitOn "." t of
       [whole]       -> (* 3) <$> readNonNeg whole
       [whole, frac] -> do
         inns <- readNonNeg whole
         outs <- readNonNeg frac
         if outs > 2 then Nothing else Just (inns * 3 + outs)
       _ -> Nothing
  where
    readNonNeg s = case TR.decimal s of
      Right (n :: Int, rest) | T.null rest, n >= 0 -> Just n
      _                                            -> Nothing

-- | Render an out-count back to MLB IP notation (\"6.2\" for 20 outs).
-- Negative inputs render as @"0.0"@ rather than producing garbage.
renderInningsPitched :: Int -> Text
renderInningsPitched outs
  | outs < 0  = "0.0"
  | otherwise =
      let (i, r) = outs `divMod` 3
      in T.pack (show i) <> "." <> T.pack (show r)