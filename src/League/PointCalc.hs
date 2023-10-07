{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module PointCalc where

import Control.Monad (filterM)
import Data.Aeson (
    FromJSON (..),
    Result (Success),
    ToJSON (..),
    Value (..),
    decode,
    eitherDecodeStrict,
    encode,
    fromJSON,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
 )

import qualified Data.Aeson.Key as K
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (writeFile)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as MS
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import GHC.Arr (array)
import Text.Read (readMaybe)

import qualified Config as C
import qualified Input as I
import qualified Middle as M
import qualified Points as P
import qualified Roster as R

import Validators

mergeGmPoints :: P.GmPoints -> P.GmPoints -> P.GmPoints
mergeGmPoints (P.GmPoints b1 p1) (P.GmPoints b2 p2) = P.GmPoints (b1 ++ b2) (p1 ++ p2)

calculatePointsForPlayer :: C.Configuration -> Text -> R.LgManager -> M.JsonPlayerData -> Either Text P.GmPoints
calculatePointsForPlayer config playerId lgManager stats = do
    playerType <- batterOrPitcher playerId lgManager
    let maybeStatsData = MS.lookup playerId (M.stats stats)
    case playerType of
        P.Batting -> case maybeStatsData >>= M.batting of
            Just battingStats -> Right $ calculateBattingPoints battingStats
            Nothing -> Left "Batting stats not found"
        P.Pitching -> case maybeStatsData >>= M.pitching of
            Just pitchingStats -> Right $ calculatePitchingPoints pitchingStats
            Nothing -> Left "Pitching stats not found"
  where
    calculateBattingPoints = calcBattingPoints (C.lg_battingMults $ C.point_parameters config)
    calculatePitchingPoints = calcPitchingPoints (C.lg_pitchingMults $ C.point_parameters config)

calcBattingPoints :: C.BattingMults -> I.BattingStats -> P.GmPoints
calcBattingPoints mults stats@I.BattingStats{..} =
    let s =
            fromIntegral
                ( fromMaybe 0 (I.bat_hits stats)
                    - ( fromMaybe 0 (I.bat_triples stats)
                            + fromMaybe 0 (I.bat_doubles stats)
                            + fromMaybe 0 (I.bat_homeRuns stats)
                      )
                )
                * C.lgb_single mults
        d = fromIntegral (fromMaybe 0 (I.bat_doubles stats)) * C.lgb_double mults
        t = fromIntegral (fromMaybe 0 (I.bat_triples stats)) * C.lgb_triple mults
        h = fromIntegral (fromMaybe 0 (I.bat_homeRuns stats)) * C.lgb_homerun mults
        rbi = fromIntegral (fromMaybe 0 (I.bat_rbi stats)) * C.lgb_rbi mults
        r = fromIntegral (fromMaybe 0 (I.bat_runs stats)) * C.lgb_run mults
        bob = fromIntegral (fromMaybe 0 (I.bat_baseOnBalls stats)) * C.lgb_base_on_balls mults
        sb = fromIntegral (fromMaybe 0 (I.bat_stolenBases stats)) * C.lgb_stolen_base mults
        hbp = fromIntegral (fromMaybe 0 (I.bat_hitByPitch stats)) * C.lgb_hit_by_pitch mults
        ko = fromIntegral (fromMaybe 0 (I.bat_strikeOuts stats)) * C.lgb_strikeout mults
        cs = fromIntegral (fromMaybe 0 (I.bat_caughtStealing stats)) * C.lgb_caught_stealing mults
     in P.GmPoints
            [ Just
                P.BattingGmPoints
                    { gmb_gameId = "GameID" -- TODO: Get the actual gameId
                    , gmb_total_points = s + d + t + h + rbi + r + bob + sb + hbp - ko - cs
                    , gmb_single = s
                    , gmb_double = d
                    , gmb_triple = t
                    , gmb_homerun = h
                    , gmb_rbi = rbi
                    , gmb_run = r
                    , gmb_base_on_balls = bob
                    , gmb_stolen_base = sb
                    , gmb_hit_by_pitch = hbp
                    , gmb_strikeout = ko
                    , gmb_caught_stealing = cs
                    }
            ]
            []

calcPitchingPoints :: C.PitchingMults -> I.PitchingStats -> P.GmPoints
calcPitchingPoints mults stats@I.PitchingStats{..} =
    let w = fromIntegral (fromMaybe 0 (I.pit_wins stats)) * C.lgp_win mults
        s = fromIntegral (fromMaybe 0 (I.pit_saves stats)) * C.lgp_save mults
        inningsPitched = fromMaybe "0" (I.pit_inningsPitched stats)
        parsedInnings = readMaybe (Text.unpack inningsPitched) :: Maybe Double
        actualInnings = fromMaybe 0.0 parsedInnings
        qs =
            if actualInnings >= 6 && fromIntegral (fromMaybe 0 (I.pit_earnedRuns stats)) <= 3
                then C.lgp_quality_start mults
                else 0
        ip = actualInnings * C.lgp_inning_pitched mults
        ko = fromIntegral (fromMaybe 0 (I.pit_strikeOuts stats)) * C.lgp_strikeout mults
        cg = fromIntegral (fromMaybe 0 (I.pit_completeGames stats)) * C.lgp_complete_game mults
        sho = fromIntegral (fromMaybe 0 (I.pit_shutouts stats)) * C.lgp_shutout mults
        bob = fromIntegral (fromMaybe 0 (I.pit_baseOnBalls stats)) * C.lgp_base_on_balls mults
        ha = fromIntegral (fromMaybe 0 (I.pit_hits stats)) * C.lgp_hits_allowed mults
        er = fromIntegral (fromMaybe 0 (I.pit_earnedRuns stats)) * C.lgp_earned_runs mults
        hbm = fromIntegral (fromMaybe 0 (I.pit_hitBatsmen stats)) * C.lgp_hit_batsman mults
        l = fromIntegral (fromMaybe 0 (I.pit_losses stats)) * C.lgp_loss mults
     in P.GmPoints
            []
            [ Just
                P.PitchingGmPoints
                    { gmp_gameId = "GameID" -- TODO: Get the actual gameId
                    , gmp_total_points = w + s + qs + ip + ko + cg + sho - bob - ha - er - hbm - l
                    , gmp_win = w
                    , gmp_save = s
                    , gmp_quality_start = qs
                    , gmp_inning_pitched = ip
                    , gmp_strikeout = ko
                    , gmp_complete_game = cg
                    , gmp_shutout = sho
                    , gmp_base_on_balls = bob
                    , gmp_hits_allowed = ha
                    , gmp_earned_runs = er
                    , gmp_hit_batsman = hbm
                    , gmp_loss = l
                    }
            ]

{-
-- broken functions for tying it all together
-- this one is only broken because it uses our old style of querying the stats and getting batter or pitcher
calculateAllPoints :: C.Configuration -> R.LgManager -> [M.JsonPlayerData] -> [(Text, Either Text [P.GmPoints])]
calculateAllPoints config lgManager = map (\playerData ->
    let playerId = M.playerId playerData
    in (playerId, calculatePointsForGivenPlayer config playerId lgManager playerData))

calculatePointsForGivenPlayer :: C.Configuration -> Text -> R.LgManager -> M.JsonPlayerData -> Either Text [P.GmPoints]
calculatePointsForGivenPlayer config playerId lgManager playerData = do
    playerType <- batterOrPitcher playerId lgManager
    let playerAllStats = MS.elems $ M.stats playerData
    return $ case playerType of
        P.Batting -> map (\stats -> fromMaybe (P.GmPoints [] []) (M.batting stats) >>= calcBattingPoints (C.lg_battingMults $ C.point_parameters config)) playerAllStats
        P.Pitching -> map (\stats -> fromMaybe (P.GmPoints [] []) (M.pitching stats) >>= calcPitchingPoints (C.lg_pitchingMults $ C.point_parameters config)) playerAllStats
                        -- errors for the above code: • Couldn't match expected type ‘P.GmPoints’
{-               with actual type ‘m0 b0’
• In the expression:
    fromMaybe (P.GmPoints [] []) (M.batting stats)
      >>=
        calcBattingPoints (C.lg_battingMults $ C.point_parameters config)
  In the first argument of ‘map’, namely
    ‘(\ stats
        -> fromMaybe (P.GmPoints [] []) (M.batting stats)
             >>=
               calcBattingPoints (C.lg_battingMults $ C.point_parameters config))’
  In the expression:
    map
      (\ stats
         -> fromMaybe (P.GmPoints [] []) (M.batting stats)
              >>=
                calcBattingPoints (C.lg_battingMults $ C.point_parameters config))
      playerAllStatstypecheck(-Wdeferred-type-errors)
• Couldn't match expected type ‘P.GmPoints’
              with actual type ‘m0 b0’
• In the expression:
    fromMaybe (P.GmPoints [] []) (M.batting stats)
      >>=
        calcBattingPoints (C.lg_battingMults $ C.point_parameters config)
  In the first argument of ‘map’, namely
    ‘(\ stats
        -> fromMaybe (P.GmPoints [] []) (M.batting stats)
             >>=
               calcBattingPoints (C.lg_battingMults $ C.point_parameters config))’
  In the expression:
    map
      (\ stats
         -> fromMaybe (P.GmPoints [] []) (M.batting stats)
              >>=
                calcBattingPoints (C.lg_battingMults $ C.point_parameters config))
      playerAllStatstypecheck(-Wdeferred-type-errors)
• Couldn't match expected type ‘P.GmPoints’
              with actual type ‘m0 b0’
• In the expression:
    fromMaybe (P.GmPoints [] []) (M.batting stats)
      >>=
        calcBattingPoints (C.lg_battingMults $ C.point_parameters config)
  In the first argument of ‘map’, namely
    ‘(\ stats
        -> fromMaybe (P.GmPoints [] []) (M.batting stats)
             >>=
               calcBattingPoints (C.lg_battingMults $ C.point_parameters config))’
  In the expression:
    map
      (\ stats
         -> fromMaybe (P.GmPoints [] []) (M.batting stats)
              >>=
                calcBattingPoints (C.lg_battingMults $ C.point_parameters config)) -}

queryPlayerId :: Text -> P.StatType -> M.JsonPlayerData -> P.PlayerResults
queryPlayerId playerIdQuery statType playerData
    | playerIdQuery /= playerID = P.NoStats
    | statType == P.Batting   = P.BattingResults (listToMaybe $ map M.batting allStats)
    | statType == P.Pitching  = P.PitchingResults (listToMaybe $ map M.pitching allStats)
    where
        playerID = M.playerId playerData
        allStats = MS.elems $ M.stats playerData
-- errors for the above function at row 4 column 69 and row5 column 70:
{- • Couldn't match type ‘Maybe I.PitchingStats’
                 with ‘I.PitchingStats’
  Expected: M.JsonStatsData -> I.PitchingStats
    Actual: M.JsonStatsData -> Maybe I.PitchingStats
• In the first argument of ‘map’, namely ‘M.pitching’
  In the second argument of ‘($)’, namely ‘map M.pitching allStats’
  In the first argument of ‘P.PitchingResults’, namely
    ‘(listToMaybe $ map M.pitching allStats)’typecheck(-Wdeferred-type-errors)
• Couldn't match type ‘Maybe I.PitchingStats’
                 with ‘I.PitchingStats’
  Expected: M.JsonStatsData -> I.PitchingStats
    Actual: M.JsonStatsData -> Maybe I.PitchingStats
• In the first argument of ‘map’, namely ‘M.pitching’
  In the second argument of ‘($)’, namely ‘map M.pitching allStats’
  In the first argument of ‘P.PitchingResults’, namely
    ‘(listToMaybe $ map M.pitching allStats)’typecheck(-Wdeferred-type-errors)
• Couldn't match type ‘Maybe I.PitchingStats’
                 with ‘I.PitchingStats’
  Expected: M.JsonStatsData -> I.PitchingStats
    Actual: M.JsonStatsData -> Maybe I.PitchingStats
• In the first argument of ‘map’, namely ‘M.pitching’
  In the second argument of ‘($)’, namely ‘map M.pitching allStats’
  In the first argument of ‘P.PitchingResults’, namely
    ‘(listToMaybe $ map M.pitching allStats)’typecheck( -}
 -}
