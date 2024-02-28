{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Config where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), Result (Success), Value, decode, eitherDecodeStrict, fromJSON, withObject, ToJSON(..), object, (.=), (.!=), (.:), (.:?))
import Data.Aeson ()



import Data.Aeson.Types (Parser, Result (..), withScientific, withText)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

-- ## League Configuration ADT ## --
data Configuration = Configuration
    { status :: Text
    , leagueID :: Text
    , point_parameters :: PointParameters
    , draft_parameters :: DraftParameters
    , commissioner :: Text
    , lgMembers :: [Text]
    }
    deriving (Show, Eq)

data PointParameters = PointParameters
    { lg_style :: Text
    , start_UTC :: Text
    , end_UTC :: Text
    , lg_battingMults :: BattingMults
    , lg_pitchingMults :: PitchingMults
    , valid_roster :: LgRoster
    }
    deriving (Show, Eq)

data BattingMults = BattingMults
    { lgb_single :: Double
    , lgb_double :: Double
    , lgb_triple :: Double
    , lgb_homerun :: Double
    , lgb_rbi :: Double
    , lgb_run :: Double
    , lgb_base_on_balls :: Double
    , lgb_stolen_base :: Double
    , lgb_hit_by_pitch :: Double
    , lgb_strikeout :: Double
    , lgb_caught_stealing :: Double
    }
    deriving (Show, Eq)

data PitchingMults = PitchingMults
    { lgp_win :: Double
    , lgp_save :: Double
    , lgp_quality_start :: Double
    , lgp_inning_pitched :: Double
    , lgp_strikeout :: Double
    , lgp_complete_game :: Double
    , lgp_shutout :: Double
    , lgp_base_on_balls :: Double
    , lgp_hits_allowed :: Double
    , lgp_earned_runs :: Double
    , lgp_hit_batsman :: Double
    , lgp_loss :: Double
    }
    deriving (Show, Eq)

data LgRoster = LgRoster
    { lg_catcher :: Int
    , lg_first :: Int
    , lg_second :: Int
    , lg_third :: Int
    , lg_shortstop :: Int
    , lg_outfield :: Int
    , lg_utility :: Int
    , lg_s_pitcher :: Int
    , lg_r_pitcher :: Int
    , lg_max_size :: Int
    }
    deriving (Show, Eq)

data DraftParameters = DraftParameters
    { autoDraft :: Bool
    , autoDraft_UTC :: Text
    , draft_limits :: DraftRoster
    }
    deriving (Show, Eq)

data DraftRoster = DraftRoster
    { dr_catcher :: Int
    , dr_first :: Int
    , dr_second :: Int
    , dr_third :: Int
    , dr_shortstop :: Int
    , dr_outfield :: Int
    , dr_utility :: Int
    , dr_s_pitcher :: Int
    , dr_r_pitcher :: Int
    }
    deriving (Show, Eq)

-- FromJSON Instances
instance FromJSON Configuration where
    parseJSON = withObject "Configuration" $ \v ->
        Configuration
            <$> v .: "status"
            <*> v .: "leagueID"
            <*> v .: "point_parameters"
            <*> v .: "draft_parameters"
            <*> v .: "commissioner"
            <*> v .: "lgMembers"

instance FromJSON PointParameters where
    parseJSON = withObject "PointParameters" $ \v ->
        PointParameters
            <$> v .: "style"
            <*> v .: "start_UTC"
            <*> v .: "end_UTC"
            <*> v .: "batting"
            <*> v .: "pitching"
            <*> v .: "valid_roster"

parseDouble :: Value -> Parser Double
parseDouble = withText "double" $ \t ->
    case reads (T.unpack t) :: [(Double, String)] of
        [(d, "")] -> return d
        _ -> fail "Could not parse double from string"

instance FromJSON BattingMults where
    parseJSON = withObject "BattingMults" $ \v -> do
        lgb_single <- v .: "single" >>= parseDouble
        lgb_double <- v .: "double" >>= parseDouble
        lgb_triple <- v .: "triple" >>= parseDouble
        lgb_homerun <- v .: "homerun" >>= parseDouble
        lgb_rbi <- v .: "rbi" >>= parseDouble
        lgb_run <- v .: "run" >>= parseDouble
        lgb_base_on_balls <- v .: "base_on_balls" >>= parseDouble
        lgb_stolen_base <- v .: "stolen_base" >>= parseDouble
        lgb_hit_by_pitch <- v .: "hit_by_pitch" >>= parseDouble
        lgb_strikeout <- v .: "strikeout" >>= parseDouble
        lgb_caught_stealing <- v .: "caught_stealing" >>= parseDouble

        return BattingMults{..}

instance FromJSON PitchingMults where
    parseJSON = withObject "PitchingMults" $ \v -> do
        lgp_win <- v .: "win" >>= parseDouble
        lgp_save <- v .: "save" >>= parseDouble
        lgp_quality_start <- v .: "quality_start" >>= parseDouble
        lgp_inning_pitched <- v .: "inning_pitched" >>= parseDouble
        lgp_strikeout <- v .: "strikeout" >>= parseDouble
        lgp_complete_game <- v .: "complete_game" >>= parseDouble
        lgp_shutout <- v .: "shutout" >>= parseDouble
        lgp_base_on_balls <- v .: "base_on_balls" >>= parseDouble
        lgp_hits_allowed <- v .: "hits_allowed" >>= parseDouble
        lgp_earned_runs <- v .: "earned_runs" >>= parseDouble
        lgp_hit_batsman <- v .: "hit_batsman" >>= parseDouble
        lgp_loss <- v .: "loss" >>= parseDouble

        return PitchingMults{..}

instance FromJSON LgRoster where
    parseJSON = withObject "LgRoster" $ \v ->
        LgRoster
            <$> v .: "catcher"
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
        DraftParameters
            <$> v .: "autoDraft"
            <*> v .: "autoDraft_UTC"
            <*> v .: "draft_limits"

instance FromJSON DraftRoster where
    parseJSON :: Value -> Parser DraftRoster
    parseJSON = withObject "DraftRoster" $ \v ->
        DraftRoster
            <$> v .: "catcher"
            <*> v .: "first"
            <*> v .: "second"
            <*> v .: "third"
            <*> v .: "shortstop"
            <*> v .: "outfield"
            <*> v .: "utility"
            <*> v .: "s_pitcher"
            <*> v .: "r_pitcher"

-- ToJSON Instances
instance ToJSON Configuration where
    toJSON :: Configuration -> Value
    toJSON Configuration{..} = object
        [ "status" .= status
        , "leagueID" .= leagueID
        , "point_parameters" .= point_parameters
        , "draft_parameters" .= draft_parameters
        , "commissioner" .= commissioner
        , "lgMembers" .= lgMembers
        ]

instance ToJSON PointParameters where
    toJSON :: PointParameters -> Value
    toJSON PointParameters{..} = object
        [ "style" .= lg_style
        , "start_UTC" .= start_UTC
        , "end_UTC" .= end_UTC
        , "batting" .= lg_battingMults
        , "pitching" .= lg_pitchingMults
        , "valid_roster" .= valid_roster
        ]

instance ToJSON BattingMults where
    toJSON :: BattingMults -> Value
    toJSON BattingMults{..} = object
        [ "single" .= lgb_single
        , "double" .= lgb_double
        , "triple" .= lgb_triple
        , "homerun" .= lgb_homerun
        , "rbi" .= lgb_rbi
        , "run" .= lgb_run
        , "base_on_balls" .= lgb_base_on_balls
        , "stolen_base" .= lgb_stolen_base
        , "hit_by_pitch" .= lgb_hit_by_pitch
        , "strikeout" .= lgb_strikeout
        , "caught_stealing" .= lgb_caught_stealing
        ]

instance ToJSON PitchingMults where
    toJSON :: PitchingMults -> Value
    toJSON PitchingMults{..} = object
        [ "win" .= lgp_win
        , "save" .= lgp_save
        , "quality_start" .= lgp_quality_start
        , "inning_pitched" .= lgp_inning_pitched
        , "strikeout" .= lgp_strikeout
        , "complete_game" .= lgp_complete_game
        , "shutout" .= lgp_shutout
        , "base_on_balls" .= lgp_base_on_balls
        , "hits_allowed" .= lgp_hits_allowed
        , "earned_runs" .= lgp_earned_runs
        , "hit_batsman" .= lgp_hit_batsman
        , "loss" .= lgp_loss
        ]

instance ToJSON LgRoster where
    toJSON :: LgRoster -> Value
    toJSON LgRoster{..} = object
        [ "catcher" .= lg_catcher
        , "first" .= lg_first
        , "second" .= lg_second
        , "third" .= lg_third
        , "shortstop" .= lg_shortstop
        , "outfield" .= lg_outfield
        , "utility" .= lg_utility
        , "s_pitcher" .= lg_s_pitcher
        , "r_pitcher" .= lg_r_pitcher
        , "max_size" .= lg_max_size
        ]

instance ToJSON DraftParameters where
    toJSON :: DraftParameters -> Value
    toJSON DraftParameters{..} = object
        [ "autoDraft" .= autoDraft
        , "autoDraft_UTC" .= autoDraft_UTC
        , "draft_limits" .= draft_limits
        ]

instance ToJSON DraftRoster where
    toJSON :: DraftRoster -> Value
    toJSON DraftRoster{..} = object
        [ "catcher" .= dr_catcher
        , "first" .= dr_first
        , "second" .= dr_second
        , "third" .= dr_third
        , "shortstop" .= dr_shortstop
        , "outfield" .= dr_outfield
        , "utility" .= dr_utility
        , "s_pitcher" .= dr_s_pitcher
        , "r_pitcher" .= dr_r_pitcher
        ]
