{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}

module Leaderboard where

import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), Result (Success), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V

-- A Mofified Version of the Input fromJson stuff.
data SeasonData where
  SeasonData :: {teams :: Teams} -> SeasonData
  deriving (Show, Eq)

instance FromJSON SeasonData where
  parseJSON :: Value -> Parser SeasonData
  parseJSON = withObject "SeasonData" $ \v ->
    SeasonData <$> v .: "teams"

data Teams where
  Teams :: {away :: TeamData, home :: TeamData} -> Teams
  deriving (Show, Eq)

instance FromJSON Teams where
  parseJSON :: Value -> Parser Teams
  parseJSON = withObject "Teams" $ \v ->
    Teams
      <$> v
      .: "away"
      <*> v
      .: "home"

data TeamData where
  TeamData :: {players :: M.Map Text Player} -> TeamData
  deriving (Show, Eq)

instance FromJSON TeamData where
  parseJSON :: Value -> Parser TeamData
  parseJSON = withObject "TeamData" $ \v -> do
    playersMap <- v .: "players"
    validPlayers <- traverse parseJSON playersMap
    pure TeamData {players = validPlayers}

type Players = [(Text, Player)]

data Player where
  Player :: {  person :: Person,
               status :: Status,
               seasonStats :: PlayerSeasonStats
               }
              -> Player
  deriving (Show, Eq)

instance FromJSON Player where
  parseJSON :: Value -> Parser Player
  parseJSON = withObject "Player" $ \v -> do
    person <- v .: "person"
    status <- v .: "status"
    seasonStats <- v .: "seasonStats"
    return $ Player person status seasonStats

data Person where
  Person :: {personId :: Int, fullName :: Text} -> Person
  deriving (Show, Eq)

instance FromJSON Person where
  parseJSON :: Value -> Parser Person
  parseJSON = withObject "Person" $ \v ->
    Person
      <$> v
      .: "id"
      <*> v
      .: "fullName"

data Status where
  Status :: {status_code :: Text} -> Status
  deriving (Show, Eq)

instance FromJSON Status where
  parseJSON :: Value -> Parser Status
  parseJSON = withObject "Status" $ \v ->
    Status
      <$> v
      .: "code"

data PlayerSeasonStats where
  PlayerSeasonStats :: {  
                    batting :: Maybe BattingTotals,
                    pitching :: Maybe PitchingTotals}
                   -> PlayerSeasonStats
  deriving (Show, Eq)

instance FromJSON PlayerSeasonStats where
  parseJSON :: Value -> Parser PlayerSeasonStats
  parseJSON = withObject "PlayerSeasonStats" $ \v ->
    PlayerSeasonStats
      <$> v .:? "batting"
      <*> v .:? "pitching"

data BattingTotals where
  BattingTotals :: {  
                    bat_T_gamesPlayed :: Maybe Int,
                    bat_T_flyOuts :: Maybe Int,
                    bat_T_groundOuts :: Maybe Int,
                    bat_T_runs :: Maybe Int,
                    bat_T_doubles :: Maybe Int,
                    bat_T_triples :: Maybe Int,
                    bat_T_homeRuns :: Maybe Int,
                    bat_T_strikeOuts :: Maybe Int,
                    bat_T_baseOnBalls :: Maybe Int,
                    bat_T_intentionalWalks :: Maybe Int,
                    bat_T_hits :: Maybe Int,
                    bat_T_hitByPitch :: Maybe Int,
                    bat_T_avg :: Maybe Text,
                    bat_T_atBats :: Maybe Int,
                    bat_T_obp :: Maybe Text,
                    bat_T_slg :: Maybe Text,
                    bat_T_ops :: Maybe Text,
                    bat_T_caughtStealing :: Maybe Int,
                    bat_T_stolenBases :: Maybe Int,
                    bat_T_stolenBasePercentage :: Maybe Text,
                    bat_T_groundIntoDoublePlay :: Maybe Int,
                    bat_T_groundIntoTriplePlay :: Maybe Int,
                    bat_T_plateAppearances :: Maybe Int,
                    bat_T_totalBases :: Maybe Int,
                    bat_T_rbi :: Maybe Int,
                    bat_T_leftOnBase :: Maybe Int,
                    bat_T_sacBunts :: Maybe Int,
                    bat_T_sacFlies :: Maybe Int,
                    bat_T_babip :: Maybe Text,
                    bat_T_catchersInterference :: Maybe Int,
                    bat_T_pickoffs :: Maybe Int,
                    bat_T_atBatsPerHomeRun :: Maybe Text
                    }
                    -> BattingTotals
  deriving (Show, Eq)

-- Pitching Season Stuff (seasonStats.pitching inside of boxscore)
data PitchingTotals where
  PitchingTotals :: {  
                    pit_T_gamesPlayed :: Maybe Int,
                    pit_T_gamesStarted :: Maybe Int,
                    pit_T_groundOuts :: Maybe Int,
                    pit_T_airOuts :: Maybe Int,
                    pit_T_runs :: Maybe Int,
                    pit_T_doubles :: Maybe Int,
                    pit_T_triples :: Maybe Int,
                    pit_T_homeRuns :: Maybe Int,
                    pit_T_strikeOuts :: Maybe Int,
                    pit_T_baseOnBalls :: Maybe Int,
                    pit_T_intentionalWalks :: Maybe Int,
                    pit_T_hits :: Maybe Int,
                    pit_T_hitByPitch :: Maybe Int,
                    pit_T_atBats :: Maybe Int,
                    pit_T_obp :: Maybe Text,
                    pit_T_caughtStealing :: Maybe Int,
                    pit_T_stolenBases :: Maybe Int,
                    pit_T_stolenBasePercentage :: Maybe Text,
                    pit_T_numberOfPitches :: Maybe Int,
                    pit_T_era :: Maybe Text,
                    pit_T_inningsPitched :: Maybe Text,
                    pit_T_wins :: Maybe Int,
                    pit_T_losses :: Maybe Int,
                    pit_T_saves :: Maybe Int,
                    pit_T_saveOpportunities :: Maybe Int,
                    pit_T_holds :: Maybe Int,
                    pit_T_blownSaves :: Maybe Int,
                    pit_T_earnedRuns :: Maybe Int,
                    pit_T_whip :: Maybe Text,
                    pit_T_battersFaced :: Maybe Int,
                    pit_T_outs :: Maybe Int,
                    pit_T_gamesPitched :: Maybe Int,
                    pit_T_completeGames :: Maybe Int,
                    pit_T_shutouts :: Maybe Int,
                    pit_T_pitchesThrown :: Maybe Int,
                    pit_T_balls :: Maybe Int,
                    pit_T_strikes :: Maybe Int,
                    pit_T_strikePercentage :: Maybe Text,
                    pit_T_hitBatsmen :: Maybe Int,
                    pit_T_balks :: Maybe Int,
                    pit_T_wildPitches :: Maybe Int,
                    pit_T_pickoffs :: Maybe Int,
                    pit_T_groundOutsToAirouts :: Maybe Text,
                    pit_T_rbi :: Maybe Int,
                    pit_T_winPercentage :: Maybe Text,
                    pit_T_pitchesPerInning :: Maybe Text,
                    pit_T_gamesFinished :: Maybe Int,
                    pit_T_strikeoutWalkRatio :: Maybe Text,
                    pit_T_strikeoutsPer9Inn :: Maybe Text,
                    pit_T_walksPer9Inn :: Maybe Text,
                    pit_T_hitsPer9Inn :: Maybe Text,
                    pit_T_runsScoredPer9 :: Maybe Text,
                    pit_T_homeRunsPer9 :: Maybe Text,
                    pit_T_inheritedRunners :: Maybe Int,
                    pit_T_inheritedRunnersScored :: Maybe Int,
                    pit_T_catchersInterference :: Maybe Int,
                    pit_T_sacBunts :: Maybe Int,
                    pit_T_sacFlies :: Maybe Int,
                    pit_T_passedBall :: Maybe Int
                    }
                    -> PitchingTotals
  deriving (Show, Eq)

instance FromJSON PitchingTotals where
  parseJSON :: Value -> Parser PitchingTotals
  parseJSON = withObject "PitchingTotals" $ \v ->
    PitchingTotals
      <$> v .:? "gamesPlayed"
      <*> v .:? "gamesStarted"
      <*> v .:? "groundOuts"
      <*> v .:? "airOuts"
      <*> v .:? "runs"
      <*> v .:? "doubles"
      <*> v .:? "triples"
      <*> v .:? "homeRuns"
      <*> v .:? "strikeOuts"
      <*> v .:? "baseOnBalls"
      <*> v .:? "intentionalWalks"
      <*> v .:? "hits"
      <*> v .:? "hitByPitch"
      <*> v .:? "atBats"
      <*> v .:? "obp"
      <*> v .:? "caughtStealing"
      <*> v .:? "stolenBases"
      <*> v .:? "stolenBasePercentage"
      <*> v .:? "numberOfPitches"
      <*> v .:? "era"
      <*> v .:? "inningsPitched"
      <*> v .:? "wins"
      <*> v .:? "losses"
      <*> v .:? "saves"
      <*> v .:? "saveOpportunities"
      <*> v .:? "holds"
      <*> v .:? "blownSaves"
      <*> v .:? "earnedRuns"
      <*> v .:? "whip"
      <*> v .:? "battersFaced"
      <*> v .:? "outs"
      <*> v .:? "gamesPitched"
      <*> v .:? "completeGames"
      <*> v .:? "shutouts"
      <*> v .:? "pitchesThrown"
      <*> v .:? "balls"
      <*> v .:? "strikes"
      <*> v .:? "strikePercentage"
      <*> v .:? "hitBatsmen"
      <*> v .:? "balks"
      <*> v .:? "wildPitches"
      <*> v .:? "pickoffs"
      <*> v .:? "groundOutsToAirouts"
      <*> v .:? "rbi"
      <*> v .:? "winPercentage"
      <*> v .:? "pitchesPerInning"
      <*> v .:? "gamesFinished"
      <*> v .:? "strikeoutWalkRatio"
      <*> v .:? "strikeoutsPer9Inn"
      <*> v .:? "walksPer9Inn"
      <*> v .:? "hitsPer9Inn"
      <*> v .:? "runsScoredPer9"
      <*> v .:? "homeRunsPer9"
      <*> v .:? "inheritedRunners"
      <*> v .:? "inheritedRunnersScored"
      <*> v .:? "catchersInterference"
      <*> v .:? "sacBunts"
      <*> v .:? "sacFlies"
      <*> v .:? "passedBall"

instance FromJSON BattingTotals where
  parseJSON :: Value -> Parser BattingTotals
  parseJSON = withObject "BattingTotals" $ \v ->
    BattingTotals
      <$> v .:? "gamesPlayed"
      <*> v .:? "flyOuts"
      <*> v .:? "groundOuts"
      <*> v .:? "runs"
      <*> v .:? "doubles"
      <*> v .:? "triples"
      <*> v .:? "homeRuns"
      <*> v .:? "strikeOuts"
      <*> v .:? "baseOnBalls"
      <*> v .:? "intentionalWalks"
      <*> v .:? "hits"
      <*> v .:? "hitByPitch"
      <*> v .:? "avg"
      <*> v .:? "atBats"
      <*> v .:? "obp"
      <*> v .:? "slg"
      <*> v .:? "ops"
      <*> v .:? "caughtStealing"
      <*> v .:? "stolenBases"
      <*> v .:? "stolenBasePercentage"
      <*> v .:? "groundIntoDoublePlay"
      <*> v .:? "groundIntoTriplePlay"
      <*> v .:? "plateAppearances"
      <*> v .:? "totalBases"
      <*> v .:? "rbi"
      <*> v .:? "leftOnBase"
      <*> v .:? "sacBunts"
      <*> v .:? "sacFlies"
      <*> v .:? "babip"
      <*> v .:? "catchersInterference"
      <*> v .:? "pickoffs"
      <*> v .:? "atBatsPerHomeRun"
