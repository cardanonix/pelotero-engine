{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module MiddleADT where

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
import qualified InputADT as I
import GHC.Arr (array)

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

instance ToJSON JsonPlayerData where
    toJSON :: JsonPlayerData -> Value
    toJSON (JsonPlayerData pid fname sts) =
        object ["fullName" .= fname, "player_id" .= pid, "stats" .= sts]

instance ToJSON JsonStatsData where
    toJSON :: JsonStatsData -> Value
    toJSON (JsonStatsData pId allPos statCode bat pitch) =
        object ["parentTeamId" .= pId, "allPositions" .= allPos, "status" .= statCode, "batting" .= bat, "pitching" .= pitch]

-- instance ToJSON I.Position where
--     toJSON :: I.Position -> Value
--     toJSON (I.Position allPositions) = 
--         let positionValue = read (Text.unpack allPositions) :: Int 
--         in Array $ V.fromList [Number (fromIntegral positionValue)]

instance ToJSON I.Position where
    toJSON :: I.Position -> Value
    toJSON (I.Position allPositions) = 
        let positionValue = read (Text.unpack allPositions) :: Int 
        in Number (fromIntegral positionValue)

instance ToJSON I.PitchingStats where
    toJSON :: I.PitchingStats -> Value
    toJSON pitStats = object $ catMaybes
        [ fmap ("pit_gamesPlayed" .=) (I.pit_gamesPlayed pitStats)
        , fmap ("pit_gamesStarted" .=) (I.pit_gamesStarted pitStats)
        , fmap ("pit_flyOuts" .=) (I.pit_flyOuts pitStats)
        , fmap ("pit_groundOuts" .=) (I.pit_groundOuts pitStats)
        , fmap ("pit_airOuts" .=) (I.pit_airOuts pitStats)
        , fmap ("pit_runs" .=) (I.pit_runs pitStats)
        , fmap ("pit_doubles" .=) (I.pit_doubles pitStats)
        , fmap ("pit_triples" .=) (I.pit_triples pitStats)
        , fmap ("pit_homeRuns" .=) (I.pit_homeRuns pitStats)
        , fmap ("pit_strikeOuts" .=) (I.pit_strikeOuts pitStats)
        , fmap ("pit_baseOnBalls" .=) (I.pit_baseOnBalls pitStats)
        , fmap ("pit_intentionalWalks" .=) (I.pit_intentionalWalks pitStats)
        , fmap ("pit_hits" .=) (I.pit_hits pitStats)
        , fmap ("pit_hitByPitch" .=) (I.pit_hitByPitch pitStats)
        , fmap ("pit_atBats" .=) (I.pit_atBats pitStats)
        , fmap ("pit_caughtStealing" .=) (I.pit_caughtStealing pitStats)
        , fmap ("pit_stolenBases" .=) (I.pit_stolenBases pitStats)
        , fmap ("pit_numberOfPitches" .=) (I.pit_numberOfPitches pitStats)
        , fmap ("pit_inningsPitched" .=) (I.pit_inningsPitched pitStats)
        , fmap ("pit_wins" .=) (I.pit_wins pitStats)
        , fmap ("pit_losses" .=) (I.pit_losses pitStats)
        , fmap ("pit_saves" .=) (I.pit_saves pitStats)
        , fmap ("pit_saveOpportunities" .=) (I.pit_saveOpportunities pitStats)
        , fmap ("pit_holds" .=) (I.pit_holds pitStats)
        , fmap ("pit_blownSaves" .=) (I.pit_blownSaves pitStats)
        , fmap ("pit_earnedRuns" .=) (I.pit_earnedRuns pitStats)
        , fmap ("pit_battersFaced" .=) (I.pit_battersFaced pitStats)
        , fmap ("pit_outs" .=) (I.pit_outs pitStats)
        , fmap ("pit_gamesPitched" .=) (I.pit_gamesPitched pitStats)
        , fmap ("pit_completeGames" .=) (I.pit_completeGames pitStats)
        , fmap ("pit_shutouts" .=) (I.pit_shutouts pitStats)
        , fmap ("pit_pitchesThrown" .=) (I.pit_pitchesThrown pitStats)
        , fmap ("pit_balls" .=) (I.pit_balls pitStats)
        , fmap ("pit_strikes" .=) (I.pit_strikes pitStats)
        , fmap ("pit_hitBatsmen" .=) (I.pit_hitBatsmen pitStats)
        , fmap ("pit_balks" .=) (I.pit_balks pitStats)
        , fmap ("pit_wildPitches" .=) (I.pit_wildPitches pitStats)
        , fmap ("pit_pickoffs" .=) (I.pit_pickoffs pitStats)
        , fmap ("pit_rbi" .=) (I.pit_rbi pitStats)
        , fmap ("pit_gamesFinished" .=) (I.pit_gamesFinished pitStats)
        , fmap ("pit_inheritedRunners" .=) (I.pit_inheritedRunners pitStats)
        , fmap ("pit_inheritedRunnersScored" .=) (I.pit_inheritedRunnersScored pitStats)
        , fmap ("pit_catchersInterference" .=) (I.pit_catchersInterference pitStats)
        , fmap ("pit_sacBunts" .=) (I.pit_sacBunts pitStats)
        , fmap ("pit_sacFlies" .=) (I.pit_sacFlies pitStats)
        , fmap ("pit_passedBall" .=) (I.pit_passedBall pitStats)
        ]

instance ToJSON I.BattingStats where
    toJSON :: I.BattingStats -> Value
    toJSON batStats = object $ catMaybes
        [ fmap ("bat_gamesPlayed" .=) (I.bat_gamesPlayed batStats)
        , fmap ("bat_flyOuts" .=) (I.bat_flyOuts batStats)
        , fmap ("bat_groundOuts" .=) (I.bat_groundOuts batStats)
        , fmap ("bat_runs" .=) (I.bat_runs batStats)
        , fmap ("bat_doubles" .=) (I.bat_doubles batStats)
        , fmap ("bat_triples" .=) (I.bat_triples batStats)
        , fmap ("bat_homeRuns" .=) (I.bat_homeRuns batStats)
        , fmap ("bat_strikeOuts" .=) (I.bat_strikeOuts batStats)
        , fmap ("bat_baseOnBalls" .=) (I.bat_baseOnBalls batStats)
        , fmap ("bat_intentionalWalks" .=) (I.bat_intentionalWalks batStats)
        , fmap ("bat_hits" .=) (I.bat_hits batStats)
        , fmap ("bat_hitByPitch" .=) (I.bat_hitByPitch batStats)
        , fmap ("bat_atBats" .=) (I.bat_atBats batStats)
        , fmap ("bat_caughtStealing" .=) (I.bat_caughtStealing batStats)
        , fmap ("bat_stolenBases" .=) (I.bat_stolenBases batStats)
        , fmap ("bat_groundIntoDoublePlay" .=) (I.bat_groundIntoDoublePlay batStats)
        , fmap ("bat_groundIntoTriplePlay" .=) (I.bat_groundIntoTriplePlay batStats)
        , fmap ("bat_plateAppearances" .=) (I.bat_plateAppearances batStats)
        , fmap ("bat_totalBases" .=) (I.bat_totalBases batStats)
        , fmap ("bat_rbi" .=) (I.bat_rbi batStats)
        , fmap ("bat_leftOnBase" .=) (I.bat_leftOnBase batStats)
        , fmap ("bat_sacBunts" .=) (I.bat_sacBunts batStats)
        , fmap ("bat_sacFlies" .=) (I.bat_sacFlies batStats)
        , fmap ("bat_catchersInterference" .=) (I.bat_catchersInterference batStats)
        , fmap ("bat_pickoffs" .=) (I.bat_pickoffs batStats)
        ]
