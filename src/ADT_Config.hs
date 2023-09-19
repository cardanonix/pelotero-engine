{-# LANGUAGE OverloadedStrings, RecordWildCards, GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE InstanceSigs #-}

module ADT_Config where

import Control.Monad (filterM)
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Result (Success), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..), withScientific, withText)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text ( Text, Text )
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Scientific (toBoundedInteger)

-- ## League Configuration ADT ## -- 
data Configuration = Configuration
  { status           :: Text
  , leagueID         :: Text
  , point_parameters :: PointParameters
  , draft_parameters :: DraftParameters
  , commissioner     :: Text
  , lgMembers        :: [Text]
  } deriving (Show, Eq)

data PointParameters = PointParameters
  { lg_style          :: Text
  , start_UTC         :: Text
  , end_UTC           :: Text
  , lg_battingMults   :: BattingMults
  , lg_pitchingMults  :: PitchingMults
  , valid_roster      :: LgRoster
  } deriving (Show, Eq)

data BattingMults = BattingMults
  { lgb_single          :: Double
  , lgb_double          :: Double
  , lgb_triple          :: Double
  , lgb_homerun         :: Double
  , lgb_rbi             :: Double
  , lgb_run             :: Double
  , lgb_base_on_balls   :: Double
  , lgb_stolen_base     :: Double
  , lgb_hit_by_pitch    :: Double
  , lgb_strikeout       :: Double
  , lgb_caught_stealing :: Double
  } deriving (Show, Eq)

data PitchingMults = PitchingMults
  { lgp_win             :: Double
  , lgp_save            :: Double
  , lgp_quality_start   :: Double
  , lgp_inning_pitched  :: Double
  , lgp_strikeout       :: Double
  , lgp_complete_game   :: Double
  , lgp_shutout         :: Double
  , lgp_base_on_balls   :: Double
  , lgp_hits_allowed    :: Double
  , lgp_earned_runs     :: Double
  , lgp_hit_batsman     :: Double
  , lgp_loss            :: Double
  } deriving (Show, Eq)

data LgRoster = LgRoster
  { lg_catcher    :: Int
  , lg_first      :: Int
  , lg_second     :: Int
  , lg_third      :: Int
  , lg_shortstop  :: Int
  , lg_outfield   :: Int
  , lg_utility    :: Int
  , lg_s_pitcher  :: Int
  , lg_r_pitcher  :: Int
  , lg_max_size   :: Int
  } deriving (Show, Eq)


data DraftRoster = DraftRoster
  { dr_catcher    :: Int
  , dr_first      :: Int
  , dr_second     :: Int
  , dr_third      :: Int
  , dr_shortstop  :: Int
  , dr_outfield   :: Int
  , dr_utility    :: Int
  , dr_s_pitcher  :: Int
  , dr_r_pitcher  :: Int
  } deriving (Show, Eq)

data DraftParameters = DraftParameters
  { autoDraft     :: Bool
  , autoDraft_UTC :: Text
  , draft_limits  :: DraftRoster
  } deriving (Show, Eq)

-- FromJSON Instances
instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \v ->
    Configuration <$> v .: "status"
                  <*> v .: "leagueID"
                  <*> v .: "point_parameters"
                  <*> v .: "draft_parameters"
                  <*> v .: "commissioner"
                  <*> v .: "lgMembers"

instance FromJSON PointParameters where
  parseJSON = withObject "PointParameters" $ \v ->
    PointParameters <$> v .: "style"
                    <*> v .: "start_UTC"
                    <*> v .: "end_UTC"
                    <*> v .: "batting"
                    <*> v .: "pitching"
                    <*> v .: "valid_roster"

parseDouble :: Value -> Parser Double
parseDouble = withText "double" $ \t ->
    case reads (T.unpack t) :: [(Double, String)] of
      [(d, "")] -> return d
      _         -> fail "Could not parse double from string"

instance FromJSON BattingMults where
    parseJSON = withObject "BattingMults" $ \v -> do
        lgb_single          <- v .: "single" >>= parseDouble
        lgb_double          <- v .: "double" >>= parseDouble
        lgb_triple          <- v .: "triple" >>= parseDouble
        lgb_homerun         <- v .: "homerun" >>= parseDouble
        lgb_rbi             <- v .: "rbi" >>= parseDouble
        lgb_run             <- v .: "run" >>= parseDouble
        lgb_base_on_balls   <- v .: "base_on_balls" >>= parseDouble
        lgb_stolen_base     <- v .: "stolen_base" >>= parseDouble
        lgb_hit_by_pitch    <- v .: "hit_by_pitch" >>= parseDouble
        lgb_strikeout       <- v .: "strikeout" >>= parseDouble
        lgb_caught_stealing <- v .: "caught_stealing" >>= parseDouble

        return BattingMults { .. }

instance FromJSON PitchingMults where
    parseJSON = withObject "PitchingMults" $ \v -> do
        lgp_win             <- v .: "win" >>= parseDouble
        lgp_save            <- v .: "save" >>= parseDouble
        lgp_quality_start   <- v .: "quality_start" >>= parseDouble
        lgp_inning_pitched  <- v .: "inning_pitched" >>= parseDouble
        lgp_strikeout       <- v .: "strikeout" >>= parseDouble
        lgp_complete_game   <- v .: "complete_game" >>= parseDouble
        lgp_shutout         <- v .: "shutout" >>= parseDouble
        lgp_base_on_balls   <- v .: "base_on_balls" >>= parseDouble
        lgp_hits_allowed    <- v .: "hits_allowed" >>= parseDouble
        lgp_earned_runs     <- v .: "earned_runs" >>= parseDouble
        lgp_hit_batsman     <- v .: "hit_batsman" >>= parseDouble
        lgp_loss            <- v .: "loss" >>= parseDouble

        return PitchingMults { .. }

instance FromJSON LgRoster where
  parseJSON = withObject "LgRoster" $ \v ->
    LgRoster <$> v .: "catcher"
           <*> v .: "first"
           <*> v .: "second"
           <*> v .: "third"
           <*> v .: "shortstop"
           <*> v .: "outfield"
           <*> v .: "utility"
           <*> v .: "s_pitcher"
           <*> v .: "r_pitcher"
           <*> v .: "max_size"

instance FromJSON DraftParameters where
  parseJSON :: Value -> Parser DraftParameters
  parseJSON = withObject "DraftParameters" $ \v ->
    DraftParameters <$> v .: "autoDraft"
                    <*> v .: "autoDraft_UTC"
                    <*> v .: "draft_limits"

instance FromJSON DraftRoster where
  parseJSON :: Value -> Parser DraftRoster
  parseJSON = withObject "DraftRoster" $ \v ->
    DraftRoster <$> v .: "catcher"
           <*> v .: "first"
           <*> v .: "second"
           <*> v .: "third"
           <*> v .: "shortstop"
           <*> v .: "outfield"
           <*> v .: "utility"
           <*> v .: "s_pitcher"
           <*> v .: "r_pitcher"