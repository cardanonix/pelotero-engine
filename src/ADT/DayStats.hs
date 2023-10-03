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


-- function that takes a String Position string (to decide whether to pull the pitching or batting stats for that player) and uses that to search through the json for the player's stats and returns an array of each of the games stats as batter or pitcher
queryPlayerId :: Text -> StatType -> M.JsonPlayerData -> Either (Maybe I.BattingStats) (Maybe I.PitchingStats)
queryPlayerId playerId statType playerData
    | playerId /= playerId' = Left Nothing
    | statType == Batting   = Left $ batting statsData
    | statType == Pitching  = Right $ pitching statsData
    where
        playerId' = playerId playerData
        statsData = stats playerData

calculatePointsForPlayer :: C.Configuration -> Text -> StatType -> M.JsonPlayerData -> Double
calculatePointsForPlayer params playerId statType stats =
    let playerStatsEither = queryPlayerId playerId statType stats
        bat_points = case playerStatsEither of
            Left (Just b)  -> calcBattingPoints (C.lg_battingMults params) b
            _              -> 0
        pit_points = case playerStatsEither of
            Right (Just p) -> calcPitchingPoints (C.lg_pitchingMults params) p
            _              -> 0
    in bat_points + pit_points

calculatePoints :: C.Configuration -> R.LgManager -> M.JsonPlayerData -> Double
calculatePoints params team stats =
    let playerStats = getPlayerStats team stats
        bat_points = case batting playerStats of
            Nothing -> 0
            Just b  -> calcBattingPoints (C.lg_battingMults params) b
        pit_points = case pitching playerStats of
            Nothing -> 0
            Just p  -> calcPitchingPoints (C.lg_pitchingMults params) p
    in bat_points + pit_points
 
calcBattingPoints :: C.BattingMults -> I.BattingStats -> Double
calcBattingPoints C.BattingMults{..} I.BattingStats{..} =
    let s = fromIntegral (fromMaybe 0 I.bat_hits - (fromMaybe 0 I.bat_triples + fromMaybe 0 I.bat_doubles + fromMaybe 0 I.bat_homeRuns)) * lgb_single
        d = fromIntegral (fromMaybe 0 I.bat_doubles) * lgb_double
        t = fromIntegral (fromMaybe 0 I.bat_triples) * lgb_triple
        h = fromIntegral (fromMaybe 0 I.bat_homeRuns) * lgb_homerun
        rbi = fromIntegral (fromMaybe 0 I.bat_rbi) * lgb_rbi
        r = fromIntegral (fromMaybe 0 I.bat_runs) * lgb_run
        bob = fromIntegral (fromMaybe 0 I.bat_baseOnBalls) * lgb_base_on_balls
        sb = fromIntegral (fromMaybe 0 I.bat_stolenBases) * lgb_stolen_base
        hbp = fromIntegral (fromMaybe 0 I.bat_hitByPitch) * lgb_hit_by_pitch
        ko = fromIntegral (fromMaybe 0 I.bat_strikeOuts) * lgb_strikeout
        cs = fromIntegral (fromMaybe 0 I.bat_caughtStealing) * lgb_caught_stealing
    in s + d + t + h + rbi + r + bob + sb + hbp - ko - cs

calcPitchingPoints :: C.PitchingMults -> I.PitchingStats -> Double
calcPitchingPoints C.PitchingMults{..} I.PitchingStats{..} =
    let w = fromIntegral (fromMaybe 0 I.pit_wins) * lgp_win
        s = fromIntegral (fromMaybe 0 I.pit_saves) * lgp_save
        inningsPitched = fromMaybe "0" I.pit_inningsPitched
        parsedInnings = readMaybe (Text.unpack inningsPitched) :: Maybe Double
        actualInnings = fromMaybe 0.0 parsedInnings
        qs = if actualInnings >= 6 && fromIntegral (fromMaybe 0 I.pit_earnedRuns) <= 3
             then lgp_quality_start
             else 0
        ip = actualInnings * lgp_inning_pitched
        ko = fromIntegral (fromMaybe 0 I.pit_strikeOuts) * lgp_strikeout
        cg = fromIntegral (fromMaybe 0 I.pit_completeGames) * lgp_complete_game
        sho = fromIntegral (fromMaybe 0 I.pit_shutouts) * lgp_shutout
        bob = fromIntegral (fromMaybe 0 I.pit_baseOnBalls) * lgp_base_on_balls
        ha = fromIntegral (fromMaybe 0 I.pit_hits) * lgp_hits_allowed
        er = fromIntegral (fromMaybe 0 I.pit_earnedRuns) * lgp_earned_runs
        hbm = fromIntegral (fromMaybe 0 I.pit_hitBatsmen) * lgp_hit_batsman
        l = fromIntegral (fromMaybe 0 I.pit_losses) * lgp_loss
    in w + s + qs + ip + ko + cg + sho - bob - ha - er - hbm - l

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

-- -- new data type to hold point totals for a single team, attributing them to each player
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

-- data type to codify the dinstinction between batting and pitching with boolean logic 
data StatType = Batting | Pitching deriving (Show, Eq)

-- Convert StatType to a string representation
statTypeToString :: StatType -> String
statTypeToString Batting = "batting"
statTypeToString Pitching = "pitching"

{- instance FromJSON M.JsonPlayerData where
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
 -}