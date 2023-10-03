{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}


module PointCalc where

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
import qualified Points as P

import Validators


calculateAllPoints :: C.Configuration -> R.LgManager -> [M.JsonPlayerData] -> [(Text, Either Text Double)]
calculateAllPoints config lgManager allPlayerStats =
    map (\playerData -> let playerId = M.playerId playerData 
                        in (playerId, calculatePointsForGivenPlayer config playerId lgManager playerData)) 
        allPlayerStats


calculatePointsForGivenPlayer :: C.Configuration -> Text -> R.LgManager -> M.JsonPlayerData -> Either Text Double
calculatePointsForGivenPlayer config playerId lgManager playerStats = do
    playerType <- batterOrPitcher playerId lgManager
    position   <- findPlayerPosition playerId lgManager
    
    -- Now that we have player type and position, we can decide how to calculate points
    return $ calculatePoints config position playerType playerStats


-- function that takes a String Position string (to decide whether to pull the pitching or batting stats for that player) and uses that to search through the json for the player's stats and returns an array of each of the games stats as batter or pitcher
queryPlayerId :: Text -> StatType -> M.JsonPlayerData -> PlayerResults
queryPlayerId playerIdQuery statType playerData
    | playerIdQuery /= playerID = NoStats
    | statType == Batting   = BattingResults $ M.batting statsData
    | statType == Pitching  = PitchingResults $ M.pitching statsData
    where
        playerID = M.playerId playerData
        statsData = M.stats playerData

calculatePointsForPlayer :: C.Configuration -> Text -> StatType -> M.JsonPlayerData -> Double
calculatePointsForPlayer params playerId statType stats =
    case queryPlayerId playerId statType stats of
        BattingResults (Just b)  -> calcBattingPoints (C.lg_battingMults params) b
        PitchingResults (Just p) -> calcPitchingPoints (C.lg_pitchingMults params) p
        _                      -> 0

calculatePoints :: C.Configuration -> R.LgManager -> M.JsonPlayerData -> Double
calculatePoints params team stats =
    let playerStats = stats -- I assume stats already holds the relevant data, since getPlayerStats is not defined
        bat_points = case M.batting playerStats of
            Nothing -> 0
            Just b  -> calcBattingPoints (C.lg_battingMults params) b
        pit_points = case M.pitching playerStats of
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
