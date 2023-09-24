{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}


module DayStats where

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

-- extract a list of players from the json using the playerids as the keys for players to be extracted
-- extractPlayerStats :: Text -> M.JsonStatsData ->  I.PlayerStats
-- need to ponder how to properly structure this: 
-- a list of players with a list of game stats need to be decomposed but also be accesible individually 

-- currently needs a lot of work
--  I may create a new temporary type for testing the calculation functions because testing this thoroughly revolves around the rest
-- of the project

-- create a function that takes a String Position string (to decide whether to pull the pitching or batting stats for that player) and uses that to search through the json for the player's stats and returns an array of each of the games stats as batter or pitcher

queryPlayerId :: Text -> Text -> M.JsonPlayerData -> Either (Maybe I.BattingStats) (Maybe I.PitchingStats)
queryPlayerId playerId position playerData
    | playerId /= playerId' = Left Nothing
    | position == "batter"  = Left $ batting statsData
    | position == "pitcher" = Right $ pitching statsData
    | otherwise             = error "Invalid position"
    where
        playerId' = playerId playerData
        statsData = stats playerData


calculatePoints :: C.PointParameters -> R.LgManager -> M.JsonStatsData -> Double
calculatePoints params team stats =
    -- get the player's stats from the day (assuming a function named getPlayerStats exists)
    let playerStats = getPlayerStats team stats

        -- calculate batting points
        bat_points = case batting playerStats of
            Nothing -> 0
            Just b  -> calcBattingPoints (lg_battingMults params) b

        -- calculate pitching points
        pit_points = case pitching playerStats of
            Nothing -> 0
            Just p  -> calcPitchingPoints (lg_pitchingMults params) p

        -- calculate total points
        total_points = bat_points + pit_points
    in total_points

{- -- context for JsonStatsData from Middle in calculatePoints function:
data JsonPlayerData where
  JsonPlayerData :: {playerId :: Text,
                       fullName :: Text,
                       stats :: M.Map Text JsonStatsData}
                      -> JsonPlayerData
  deriving (Show, Eq)
data JsonStatsData where
  JsonStatsData :: {parentTeamId :: Int,
                      allPositions :: [I.Position],
                      statusCode :: Text,
                      batting :: Maybe I.BattingStats,
                      pitching :: Maybe I.PitchingStats}
                     -> JsonStatsData
  deriving (Show, Eq)
context for 
data LgManager = LgManager
  { status         :: Text
  , commissioner   :: Text
  , teamId         :: Text
  , leagueID       :: Text
  , current_lineup :: CurrentLineup
  , roster         :: Roster
  } deriving (Show, Eq) 
 -}
{- -- context for CurrentLineup from Config in calculatePoints function:
data CurrentLineup = CurrentLineup
  { cC  :: Text
  , b1C :: Text
  , b2C  :: Text
  , b3C  :: Text
  , ssC  :: Text
  , ofC  :: [Text]
  , uC   :: Text
  , spC  :: [Text]
  , rpC  :: [Text]
  } deriving (Show, Eq)
data Roster = Roster
  { cR  :: [Text]
  , b1R :: [Text]
  , b2R :: [Text]
  , b3R :: [Text]
  , ssR :: [Text]
  , ofR :: [Text]
  , uR  :: [Text]
  , spR :: [Text]
  , rpR :: [Text]
  } deriving (Show, Eq)
 -}

calcBattingPoints :: C.BattingMults -> I.BattingStats -> Double
calcBattingPoints C.BattingMults{..} I.BattingStats{..} =
    let s = ((fromMaybe 0 I.bat_hits - (fromMaybe 0 I.bat_triples + fromMaybe 0 I.bat_doubles + fromMaybe 0 I.bat_homeRuns)) * C.lgb_single)
        d = fromMaybe 0 I.bat_doubles * C.lgb_double
        t = fromMaybe 0 I.bat_triples * C.lgb_triple
        h = fromMaybe 0 I.bat_homeRuns * C.lgb_homerun
        rbi = fromMaybe 0 I.bat_rbi * C.lgb_rbi
        r = fromMaybe 0 I.bat_runs * C.lgb_run
        bob = fromMaybe 0 I.bat_baseOnBalls * C.lgb_base_on_balls
        sb = fromMaybe 0 I.bat_stolenBases * C.lgb_stolen_base
        hbp = fromMaybe 0 I.bat_hitByPitch * C.lgb_hit_by_pitch
        ko =  fromMaybe 0 I.bat_strikeOuts * C.lgb_strikeout
        cs = fromMaybe 0 I.bat_caughtStealing * C.lgb_caught_stealing
    in s + d + t + h + rbi + r + bob + sb + hbp - ko - cs

-- takes league point parameters and pitching stats from a single game and returns a double
calcPitchingPoints :: C.PitchingMults -> I.PitchingStats -> Double
calcPitchingPoints C.PitchingMults{..} I.PitchingStats{..} =
    let w = fromMaybe 0 I.pit_wins * C.lgp_win
        s = fromMaybe 0 I.pit_saves * C.lgp_save
        inningsPitched = fromMaybe "0" I.pit_inningsPitched
        parsedInnings = readMaybe (Text.unpack inningsPitched) :: Maybe Double
        actualInnings = fromMaybe 0 parsedInnings
        qs = if actualInnings >= 6 && fromMaybe 0 I.pit_earnedRuns <= 3
             then C.lgp_quality_start
             else 0
        ip = actualInnings * C.lgp_inning_pitched
        ko = fromMaybe 0 I.pit_strikeOuts * C.lgp_strikeout
        cg = fromMaybe 0 I.pit_completeGames * C.lgp_complete_game
        sho = fromMaybe 0 I.pit_shutouts * C.lgp_shutout
        bob = fromMaybe 0 I.pit_baseOnBalls * C.lgp_base_on_balls
        ha = fromMaybe 0 I.pit_hits * C.lgp_hits_allowed
        er = fromMaybe 0 I.pit_earnedRuns * C.lgp_earned_runs
        hbm = fromMaybe 0 I.pit_hitBatsmen * C.lgp_hit_batsman
        l = fromMaybe 0 I.pit_losses * C.lgp_loss
    in w + s + qs + ip + ko + cg + sho - bob - ha - er - hbm - l


-- new data type to hold point totals for a single team, attributing them to each player
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

instance FromJSON M.JsonPlayerData where
    parseJSON :: Value -> Parser M.JsonPlayerData
    parseJSON (Object v) = JsonPlayerData
        <$> v .: "player_id"
        <*> v .: "fullName"
        <*> v .: "stats"
    parseJSON _ = fail "Expected an object for JsonPlayerData"

instance FromJSON M.JsonStatsData where
    parseJSON :: Value -> Parser M.JsonStatsData
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