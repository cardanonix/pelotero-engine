{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module ADT_DayStats where

import Control.Monad (filterM)
import Data.Aeson
    ( Result(Success),
      Value,
      encode,
      fromJSON,
      ToJSON(..),
      Value(..),
      object,
      (.=) )
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (writeFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Aeson.Key as K
import GHC.Arr (array)

import qualified ADT_Input as I

instance FromJSON JsonPlayerData where
    parseJSON :: Value -> Parser JsonPlayerData
    parseJSON (Object v) = JsonPlayerData
        <$> v .: "player_id"
        <*> v .: "fullName"
        <*> v .: "stats"
    parseJSON _ = fail "Expected an object for JsonPlayerData"

instance FromJSON JsonStatsData where
    parseJSON :: Value -> Parser JsonStatsData
    parseJSON (Object v) = JsonStatsData
        <$> v .: "parentTeamId"
        <*> v .: "allPositions"
        <*> v .: "status"
        <*> v .:? "batting"
        <*> v .:? "pitching"
    parseJSON _ = fail "Expected an object for JsonStatsData"

instance FromJSON I.Position where
    parseJSON :: Value -> Parser I.Position
    parseJSON (Number n) = 
        let positionString = Text.pack $ show $ round n 
        in pure $ I.Position positionString
    parseJSON _ = fail "Expected a number for I.Position"

instance FromJSON I.PitchingStats where
    parseJSON :: Value -> Parser I.PitchingStats
    parseJSON (Object v) = I.PitchingStats
        <$> v .:? "pit_gamesPlayed"
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
    parseJSON _ = fail "Expected an object for I.PitchingStats"

instance FromJSON I.BattingStats where
    parseJSON :: Value -> Parser I.BattingStats
    parseJSON (Object v) = I.BattingStats
        <$> v .:? "bat_gamesPlayed"
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
    parseJSON _ = fail "Expected an object for I.BattingStats"