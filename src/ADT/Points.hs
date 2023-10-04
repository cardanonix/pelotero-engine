{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}


module Points where

import Control.Monad (filterM)
import Data.Aeson
    ( Result(Success),
      Value,
      encode,
      fromJSON,
      ToJSON(..),
      Value(..),
      object,
      (.=),
      FromJSON(..),
      Result(Success),
      Value,
      decode,
      eitherDecodeStrict,
      fromJSON,
      withObject,
      (.!=),
      (.:),
      (.:?) )

import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (writeFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Aeson.Key as K
import GHC.Arr (array)

import qualified Input as I
import qualified Middle as M
import qualified Config as C
import qualified Roster as R

-- codify the dinstinction between batting and pitching with boolean logic 
data StatType = Batting | Pitching deriving (Show, Eq)

data PlayerResults 
    = BattingResults (Maybe I.BattingStats) 
    | PitchingResults (Maybe I.PitchingStats)
    | NoStats
    deriving (Show, Eq)

-- data type to hold final point totals for a single team, attributing them to each active player
data Results = Results
  { cC  :: (Text, Double)
  , b1C :: (Text, Double)
  , b2C  :: (Text, Double)
  , b3C  :: (Text, Double)
  , ssC  :: (Text, Double)
  , ofC  :: [(Text, Double)]
  , uC   :: (Text, Double)
  , spC  :: [(Text, Double)]
  , rpC  :: [(Text, Double)]
  } deriving (Show, Eq)

-- data type for storing the top level unsummed points for that player for a given day which may contain many games as a batter or pitching or both
data GmPoints = GmPoints
  {   gmb_batting     :: [Maybe BattingGmPoints]
    , gmb_pitching    :: [Maybe PitchingGmPoints]
  } deriving (Show, Eq)

-- we want to be more granular with our calculations for each game's stats by sending the calculated values for each game to this type
data BattingGmPoints = BattingGmPoints
  { gmb_gameId          :: Text
  , gmb_total_points    :: Double
  , gmb_single          :: Double
  , gmb_double          :: Double
  , gmb_triple          :: Double
  , gmb_homerun         :: Double
  , gmb_rbi             :: Double
  , gmb_run             :: Double
  , gmb_base_on_balls   :: Double
  , gmb_stolen_base     :: Double
  , gmb_hit_by_pitch    :: Double
  , gmb_strikeout       :: Double
  , gmb_caught_stealing :: Double
  } deriving (Show, Eq)

-- we want to be more granular with our calculations for each game's stats by sending the calculated values for each game to this type
data PitchingGmPoints = PitchingGmPoints
  { gmp_gameId          :: Text
  , gmp_total_points    :: Double 
  , gmp_win             :: Double
  , gmp_save            :: Double
  , gmp_quality_start   :: Double
  , gmp_inning_pitched  :: Double
  , gmp_strikeout       :: Double
  , gmp_complete_game   :: Double
  , gmp_shutout         :: Double
  , gmp_base_on_balls   :: Double
  , gmp_hits_allowed    :: Double
  , gmp_earned_runs     :: Double
  , gmp_hit_batsman     :: Double
  , gmp_loss            :: Double
  } deriving (Show, Eq)

