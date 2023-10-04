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
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
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

-- this function needs to be refactored to handle the new shape of results
calculateAllPoints :: C.Configuration -> R.LgManager -> [M.JsonPlayerData] -> [(Text, Either Text [P.GmPoints])]
calculateAllPoints config lgManager = map (\playerData ->
    let playerId = M.playerId playerData
    in (playerId, calculatePointsForGivenPlayer config playerId lgManager playerData))

calculatePointsForGivenPlayer :: C.Configuration -> Text -> R.LgManager -> M.JsonPlayerData -> Either Text [P.GmPoints]
calculatePointsForGivenPlayer config playerId lgManager playerData = do
    position <- findPlayerPosition playerId lgManager
    let playerAllStats = M.elems $ M.stats playerData
    return $ map (\stats ->
            let batPoints = case M.batting stats of
                    Nothing -> P.GmPoints [] []
                    Just b  -> calcBattingPoints (C.lg_battingMults $ C.point_parameters config) b
                pitPoints = case M.pitching stats of
                    Nothing -> P.GmPoints [] []
                    Just p  -> calcPitchingPoints (C.lg_pitchingMults $ C.point_parameters config) p
            in mergeGmPoints batPoints pitPoints
         ) playerAllStats

queryPlayerId :: Text -> P.StatType -> M.JsonPlayerData -> P.PlayerResults
queryPlayerId playerIdQuery statType playerData
    | playerIdQuery /= playerID = P.NoStats
    | statType == P.Batting   = P.BattingResults (listToMaybe $ map M.batting allStats)
    | statType == P.Pitching  = P.PitchingResults (listToMaybe $ map M.pitching allStats)
    where
        playerID = M.playerId playerData
        allStats = M.elems $ M.stats playerData

mergeGmPoints :: P.GmPoints -> P.GmPoints -> P.GmPoints
mergeGmPoints (P.GmPoints b1 p1) (P.GmPoints b2 p2) = P.GmPoints (b1 ++ b2) (p1 ++ p2)

calculatePointsForPlayer :: C.Configuration -> Text -> P.StatType -> M.JsonPlayerData -> P.GmPoints
calculatePointsForPlayer config playerId statType stats =
    let params = C.point_parameters config in
    case queryPlayerId playerId statType stats of
        P.BattingResults (Just b)  -> calcBattingPoints (C.lg_battingMults params) b
        P.PitchingResults (Just p) -> calcPitchingPoints (C.lg_pitchingMults params) p
        _                          -> 0

calcBattingPoints :: C.BattingMults -> I.BattingStats -> P.GmPoints
calcBattingPoints mults stats@I.BattingStats{..} =
    let s = fromIntegral $ (fromMaybe 0 (I.bat_hits stats) - (fromMaybe 0 (I.bat_triples stats) + fromMaybe 0 (I.bat_doubles stats) + fromMaybe 0 (I.bat_homeRuns stats))) * C.lgb_single
        d   = fromIntegral $ fromMaybe 0 (I.bat_doubles stats) * fromIntegral C.lgb_double mults
        t   = fromIntegral $ fromMaybe 0 (I.bat_triples stats) * fromIntegral C.lgb_triple
        h   = fromIntegral $ fromMaybe 0 (I.bat_homeRuns stats) * fromIntegral C.lgb_homerun
        rbi = fromIntegral $ fromMaybe 0 (I.bat_rbi stats) * fromIntegral C.lgb_rbi
        r   = fromIntegral $ fromMaybe 0 (I.bat_runs stats) * fromIntegral C.lgb_run
        bob = fromIntegral $ fromMaybe 0 (I.bat_baseOnBalls stats) * fromIntegral C.lgb_base_on_balls
        sb  = fromIntegral $ fromMaybe 0 (I.bat_stolenBases stats) * fromIntegral C.lgb_stolen_base
        hbp = fromIntegral $ fromMaybe 0 (I.bat_hitByPitch stats) * fromIntegral C.lgb_hit_by_pitch
        ko  = fromIntegral $ fromMaybe 0 (I.bat_strikeOuts stats) * fromIntegral C.lgb_strikeout
        cs  = fromIntegral $ fromMaybe 0 (I.bat_caughtStealing stats) * fromIntegral C.lgb_caught_stealing
    in P.GmPoints
        [Just P.BattingGmPoints
          { gmb_gameId          = "GameID"  -- TODO: Get the actual gameId
          , gmb_total_points    = s + d + t + h + rbi + r + bob + sb + hbp - ko - cs
          , gmb_single          = s
          , gmb_double          = d
          , gmb_triple          = t
          , gmb_homerun         = h
          , gmb_rbi             = rbi
          , gmb_run             = r
          , gmb_base_on_balls   = bob
          , gmb_stolen_base     = sb
          , gmb_hit_by_pitch    = hbp
          , gmb_strikeout       = ko
          , gmb_caught_stealing = cs
          }]
        []

calcPitchingPoints :: C.PitchingMults -> I.PitchingStats -> P.GmPoints
calcPitchingPoints mults stats@I.PitchingStats{..} =
    let w = fromIntegral $ fromMaybe 0 (I.pit_wins stats) * fromIntegral C.lgp_win mults
        s = fromIntegral $ fromMaybe 0 (I.pit_saves stats) * fromIntegral C.lgp_save mults
        inningsPitched = fromMaybe "0" (I.pit_inningsPitched stats)
        parsedInnings = readMaybe (Text.unpack inningsPitched) :: Maybe Double
        actualInnings = fromMaybe 0.0 parsedInnings
        qs = if actualInnings >= 6 && fromIntegral (fromMaybe 0 (I.pit_earnedRuns stats)) <= 3
             then fromIntegral C.lgp_quality_start mults
             else 0
        ip = actualInnings * C.lgp_inning_pitched mults
        ko = fromIntegral $ fromMaybe 0 (I.pit_strikeOuts stats) * fromIntegral C.lgp_strikeout mults
        cg = fromIntegral $ fromMaybe 0 (I.pit_completeGames stats) * fromIntegral C.lgp_complete_game mults
        sho = fromIntegral $ fromMaybe 0 (I.pit_shutouts stats) * fromIntegral C.lgp_shutout mults
        bob = fromIntegral $ fromMaybe 0 (I.pit_baseOnBalls stats) * fromIntegral C.lgp_base_on_balls mults
        ha = fromIntegral $ fromMaybe 0 (I.pit_hits stats) * fromIntegral C.lgp_hits_allowed mults
        er = fromIntegral $ fromMaybe 0 (I.pit_earnedRuns stats) * fromIntegral C.lgp_earned_runs mults
        hbm = fromIntegral $ fromMaybe 0 (I.pit_hitBatsmen stats) * fromIntegral C.lgp_hit_batsman mults
        l = fromIntegral $ fromMaybe 0 (I.pit_losses stats) * fromIntegral C.lgp_loss mults
    in P.GmPoints
        []
        [Just P.PitchingGmPoints
          { gmp_gameId          = "GameID"  -- TODO: Get the actual gameId
          , gmp_total_points    = w + s + qs + ip + ko + cg + sho - bob - ha - er - hbm - l
          , gmp_win             = w
          , gmp_save            = s
          , gmp_quality_start   = qs
          , gmp_inning_pitched  = ip
          , gmp_strikeout       = ko
          , gmp_complete_game   = cg
          , gmp_shutout         = sho
          , gmp_base_on_balls   = bob
          , gmp_hits_allowed    = ha
          , gmp_earned_runs     = er
          , gmp_hit_batsman     = hbm
          , gmp_loss            = l
          }]

