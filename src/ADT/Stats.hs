{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Stats where

import Control.Monad (filterM)
import Data.Aeson
    ( FromJSON(..),
      Result(Success),
      Value,
      decode,
      eitherDecodeStrict,
      fromJSON,
      withObject,
      (.!=),
      (.:),
      (.:?),
      FromJSON,
      Value,
      (.:),
      withObject,
      FromJSON,
      Value,
      (.:),
      withObject,
      FromJSON,
      Value,
      (.:),
      withObject )


import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V

import qualified Middle as M

-- new types to prevent conflation of gameids and playerids
newtype GameCode = GameCode {game_code :: Text}
  deriving (Show, Eq)

newtype PlayerCode = PlayerCode {player_code :: Text}
  deriving (Show, Eq)

data BattingStats where
  BattingStats :: {  bat_gamesPlayed :: Maybe Int,
                     bat_flyOuts :: Maybe Int,
                     bat_groundOuts :: Maybe Int,
                     bat_runs :: Maybe Int,
                     bat_doubles :: Maybe Int,
                     bat_triples :: Maybe Int,
                     bat_homeRuns :: Maybe Int,
                     bat_strikeOuts :: Maybe Int,
                     bat_baseOnBalls :: Maybe Int,
                     bat_intentionalWalks :: Maybe Int,
                     bat_hits :: Maybe Int,
                     bat_hitByPitch :: Maybe Int,
                     bat_atBats :: Maybe Int,
                     bat_caughtStealing :: Maybe Int,
                     bat_stolenBases :: Maybe Int,
                     bat_groundIntoDoublePlay :: Maybe Int,
                     bat_groundIntoTriplePlay :: Maybe Int,
                     bat_plateAppearances :: Maybe Int,
                     bat_totalBases :: Maybe Int,
                     bat_rbi :: Maybe Int,
                     bat_leftOnBase :: Maybe Int,
                     bat_sacBunts :: Maybe Int,
                     bat_sacFlies :: Maybe Int,
                     bat_catchersInterference :: Maybe Int,
                     bat_pickoffs :: Maybe Int
                     }
                    -> BattingStats
  deriving (Show, Eq)

data PitchingStats where
  PitchingStats :: {  pit_gamesPlayed :: Maybe Int,
                      pit_gamesStarted :: Maybe Int,
                      pit_flyOuts :: Maybe Int,
                      pit_groundOuts :: Maybe Int,
                      pit_airOuts :: Maybe Int,
                      pit_runs :: Maybe Int,
                      pit_doubles :: Maybe Int,
                      pit_triples :: Maybe Int,
                      pit_homeRuns :: Maybe Int,
                      pit_strikeOuts :: Maybe Int,
                      pit_baseOnBalls :: Maybe Int,
                      pit_intentionalWalks :: Maybe Int,
                      pit_hits :: Maybe Int,
                      pit_hitByPitch :: Maybe Int,
                      pit_atBats :: Maybe Int,
                      pit_caughtStealing :: Maybe Int,
                      pit_stolenBases :: Maybe Int,
                      pit_numberOfPitches :: Maybe Int,
                      pit_inningsPitched :: Maybe Text,
                      pit_wins :: Maybe Int,
                      pit_losses :: Maybe Int,
                      pit_saves :: Maybe Int,
                      pit_saveOpportunities :: Maybe Int,
                      pit_holds :: Maybe Int,
                      pit_blownSaves :: Maybe Int,
                      pit_earnedRuns :: Maybe Int,
                      pit_battersFaced :: Maybe Int,
                      pit_outs :: Maybe Int,
                      pit_gamesPitched :: Maybe Int,
                      pit_completeGames :: Maybe Int,
                      pit_shutouts :: Maybe Int,
                      pit_pitchesThrown :: Maybe Int,
                      pit_balls :: Maybe Int,
                      pit_strikes :: Maybe Int,
                      pit_hitBatsmen :: Maybe Int,
                      pit_balks :: Maybe Int,
                      pit_wildPitches :: Maybe Int,
                      pit_pickoffs :: Maybe Int,
                      pit_rbi :: Maybe Int,
                      pit_gamesFinished :: Maybe Int,
                      pit_inheritedRunners :: Maybe Int,
                      pit_inheritedRunnersScored :: Maybe Int,
                      pit_catchersInterference :: Maybe Int,
                      pit_sacBunts :: Maybe Int,
                      pit_sacFlies :: Maybe Int,
                      pit_passedBall :: Maybe Int
                      }
                     -> PitchingStats
  deriving (Show, Eq)

instance FromJSON M.JsonPlayerData where
    parseJSON :: Value -> Parser M.JsonPlayerData
    parseJSON = withObject "JsonPlayerData" $ \v ->
        M.JsonPlayerData <$> v .: "player_id"
                         <*> v .: "fullName"
                         <*> v .: "stats"

instance FromJSON M.JsonStatsData where
    parseJSON :: Value -> Parser M.JsonStatsData
    parseJSON = withObject "JsonStatsData" $ \v ->
        M.JsonStatsData <$> v .: "parentTeamId"
                        <*> v .: "allPositions"
                        <*> v .: "status"
                        <*> v .: "batting"
                        <*> v .: "pitching"

instance FromJSON PitchingStats where
    parseJSON :: Value -> Parser PitchingStats
    parseJSON = withObject "PitchingStats" $ \v ->
        PitchingStats <$> v .:? "pit_gamesPlayed"
                        <*> v .:? "pit_gamesStarted"
                        <*> v .:? "pit_flyOuts"
                        <*> v .:? "pit_groundOuts"
                        <*> v .:? "pit_airOuts"
                        <*> v .:? "pit_runs"
                        <*> v .:? "pit_doubles"
                        <*> v .:? "pit_triples"
                        <*> v .:? "pit_homeRuns"
                        <*> v .:? "pit_strikeOuts"
                        <*> v .:? "pit_baseOnBalls"
                        <*> v .:? "pit_intentionalWalks"
                        <*> v .:? "pit_hits"
                        <*> v .:? "pit_hitByPitch"
                        <*> v .:? "pit_atBats"
                        <*> v .:? "pit_caughtStealing"
                        <*> v .:? "pit_stolenBases"
                        <*> v .:? "pit_numberOfPitches"
                        <*> v .:? "pit_inningsPitched"
                        <*> v .:? "pit_wins"
                        <*> v .:? "pit_losses"
                        <*> v .:? "pit_saves"
                        <*> v .:? "pit_saveOpportunities"
                        <*> v .:? "pit_holds"
                        <*> v .:? "pit_blownSaves"
                        <*> v .:? "pit_earnedRuns"
                        <*> v .:? "pit_battersFaced"
                        <*> v .:? "pit_outs"
                        <*> v .:? "pit_gamesPitched"
                        <*> v .:? "pit_completeGames"
                        <*> v .:? "pit_shutouts"
                        <*> v .:? "pit_pitchesThrown"
                        <*> v .:? "pit_balls"
                        <*> v .:? "pit_strikes"
                        <*> v .:? "pit_hitBatsmen"
                        <*> v .:? "pit_balks"
                        <*> v .:? "pit_wildPitches"
                        <*> v .:? "pit_pickoffs"
                        <*> v .:? "pit_rbi"
                        <*> v .:? "pit_gamesFinished"
                        <*> v .:? "pit_inheritedRunners"
                        <*> v .:? "pit_inheritedRunnersScored"
                        <*> v .:? "pit_catchersInterference"
                        <*> v .:? "pit_sacBunts"
                        <*> v .:? "pit_sacFlies"
                        <*> v .:? "pit_passedBall"

instance FromJSON BattingStats where
    parseJSON = withObject "BattingStats" $ \v ->
        BattingStats  <$> v .:? "bat_gamesPlayed"
                        <*> v .:? "bat_flyOuts"
                        <*> v .:? "bat_groundOuts"
                        <*> v .:? "bat_runs"
                        <*> v .:? "bat_doubles"
                        <*> v .:? "bat_triples"
                        <*> v .:? "bat_homeRuns"
                        <*> v .:? "bat_strikeOuts"
                        <*> v .:? "bat_baseOnBalls"
                        <*> v .:? "bat_intentionalWalks"
                        <*> v .:? "bat_hits"
                        <*> v .:? "bat_hitByPitch"
                        <*> v .:? "bat_atBats"
                        <*> v .:? "bat_caughtStealing"
                        <*> v .:? "bat_stolenBases"
                        <*> v .:? "bat_groundIntoDoublePlay"
                        <*> v .:? "bat_groundIntoTriplePlay"
                        <*> v .:? "bat_plateAppearances"
                        <*> v .:? "bat_totalBases"
                        <*> v .:? "bat_rbi"
                        <*> v .:? "bat_leftOnBase"
                        <*> v .:? "bat_sacBunts"
                        <*> v .:? "bat_sacFlies"
                        <*> v .:? "bat_catchersInterference"
                        <*> v .:? "bat_pickoffs"
