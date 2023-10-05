{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}

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

import Data.Aeson (FromJSON, Value, (.:), withObject)

-- new types to prevent conflation of gameids and playerids
newtype GameCode = GameCode {game_code :: Text}
  deriving (Show, Eq)

newtype PlayerCode = PlayerCode {player_code :: Text}
  deriving (Show, Eq)

instance FromJSON M.JsonPlayerData where
    parseJSON = withObject "JsonPlayerData" $ \v ->
        M.JsonPlayerData <$> v .: "player_id"
                         <*> v .: "fullName"
                         <*> v .: "stats"

instance FromJSON M.JsonStatsData where
    parseJSON = withObject "JsonStatsData" $ \v ->
        M.JsonStatsData <$> v .: "parentTeamId"
                        <*> v .: "allPositions"
                        <*> v .: "status"
                        <*> v .: "batting"
                        <*> v .: "pitching"

instance FromJSON I.Position where
    parseJSON = withObject "Position" $ \v ->
        I.Position <$> v .: "pos_code"

instance FromJSON I.PitchingStats where
    parseJSON = withObject "PitchingStats" $ \v ->
        I.PitchingStats <$> v .:? "pit_gamesPlayed"
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


instance FromJSON I.BattingStats where
    parseJSON = withObject "BattingStats" $ \v ->
        I.BattingStats  <$> v .:? "bat_gamesPlayed"
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
