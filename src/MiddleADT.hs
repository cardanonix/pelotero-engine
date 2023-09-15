{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}

module MiddleADT where

import Control.Monad (filterM)
import Data.Aeson
    ( FromJSON(..),
      Result(Success),
      Value,
      decode,
      encode,
      eitherDecodeStrict,
      fromJSON,
      withObject,
      (.!=),
      (.:),
      (.:?),
      ToJSON(..),
      Value(..),
      object,
      (.=) )
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile, writeFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Aeson.Key as K
import qualified InputADT as IN

data JsonPlayerData = JsonPlayerData
    { playerId :: Text
    , fullName :: Text
    , stats :: M.Map Text JsonStatsData
    } deriving (Show, Eq)

data JsonStatsData = JsonStatsData
    { parentTeamId :: Int
    , allPositions :: [IN.Position]
    , statusCode :: Text
    , batting :: Maybe IN.BattingStats
    , pitching :: Maybe IN.PitchingStats
    } deriving (Show, Eq)

-- crucial player stat-mutation stuff
playerToJsonPlayerData :: IN.Player -> JsonPlayerData
playerToJsonPlayerData p =
    JsonPlayerData
        { playerId = Text.pack $ show $ IN.personId (IN.person p)
        , fullName = IN.fullName (IN.person p)
        , stats = M.singleton (maybe "" (Text.pack . show) (IN.gameid p)) (playerToJsonStatsData p)
        }

playerToJsonStatsData :: IN.Player -> JsonStatsData
playerToJsonStatsData p =
    JsonStatsData
        { parentTeamId = IN.parentTeamId p
        , allPositions = fromMaybe [] (IN.allPositions p)
        , statusCode = IN.status_code (IN.status p)
        , batting = IN.batting (IN.stats p)
        , pitching = IN.pitching (IN.stats p)
        }

instance ToJSON JsonPlayerData where
    toJSON (JsonPlayerData pid fname sts) =
        object ["player_id" .= pid, "fullName" .= fname, "stats" .= sts]

instance ToJSON JsonStatsData where
    toJSON (JsonStatsData pId allPos statCode bat pitch) =
        object ["parentTeamId" .= pId, "allPositions" .= allPos, "status" .= statCode, "batting" .= bat, "pitching" .= pitch]

instance ToJSON IN.PitchingStats where
    toJSON pitStats = object $ catMaybes
        [ fmap ("pit_gamesPlayed" .=) (IN.pit_gamesPlayed pitStats)
        , fmap ("pit_gamesStarted" .=) (IN.pit_gamesStarted pitStats)
        , fmap ("pit_flyOuts" .=) (IN.pit_flyOuts pitStats)
        , fmap ("pit_groundOuts" .=) (IN.pit_groundOuts pitStats)
        , fmap ("pit_airOuts" .=) (IN.pit_airOuts pitStats)
        , fmap ("pit_runs" .=) (IN.pit_runs pitStats)
        , fmap ("pit_doubles" .=) (IN.pit_doubles pitStats)
        , fmap ("pit_triples" .=) (IN.pit_triples pitStats)
        , fmap ("pit_homeRuns" .=) (IN.pit_homeRuns pitStats)
        , fmap ("pit_strikeOuts" .=) (IN.pit_strikeOuts pitStats)
        , fmap ("pit_baseOnBalls" .=) (IN.pit_baseOnBalls pitStats)
        , fmap ("pit_intentionalWalks" .=) (IN.pit_intentionalWalks pitStats)
        , fmap ("pit_hits" .=) (IN.pit_hits pitStats)
        , fmap ("pit_hitByPitch" .=) (IN.pit_hitByPitch pitStats)
        , fmap ("pit_atBats" .=) (IN.pit_atBats pitStats)
        , fmap ("pit_caughtStealing" .=) (IN.pit_caughtStealing pitStats)
        , fmap ("pit_stolenBases" .=) (IN.pit_stolenBases pitStats)
        , fmap ("pit_numberOfPitches" .=) (IN.pit_numberOfPitches pitStats)
        , fmap ("pit_inningsPitched" .=) (IN.pit_inningsPitched pitStats)
        , fmap ("pit_wins" .=) (IN.pit_wins pitStats)
        , fmap ("pit_losses" .=) (IN.pit_losses pitStats)
        , fmap ("pit_saves" .=) (IN.pit_saves pitStats)
        , fmap ("pit_saveOpportunities" .=) (IN.pit_saveOpportunities pitStats)
        , fmap ("pit_holds" .=) (IN.pit_holds pitStats)
        , fmap ("pit_blownSaves" .=) (IN.pit_blownSaves pitStats)
        , fmap ("pit_earnedRuns" .=) (IN.pit_earnedRuns pitStats)
        , fmap ("pit_battersFaced" .=) (IN.pit_battersFaced pitStats)
        , fmap ("pit_outs" .=) (IN.pit_outs pitStats)
        , fmap ("pit_gamesPitched" .=) (IN.pit_gamesPitched pitStats)
        , fmap ("pit_completeGames" .=) (IN.pit_completeGames pitStats)
        , fmap ("pit_shutouts" .=) (IN.pit_shutouts pitStats)
        , fmap ("pit_pitchesThrown" .=) (IN.pit_pitchesThrown pitStats)
        , fmap ("pit_balls" .=) (IN.pit_balls pitStats)
        , fmap ("pit_strikes" .=) (IN.pit_strikes pitStats)
        , fmap ("pit_hitBatsmen" .=) (IN.pit_hitBatsmen pitStats)
        , fmap ("pit_balks" .=) (IN.pit_balks pitStats)
        , fmap ("pit_wildPitches" .=) (IN.pit_wildPitches pitStats)
        , fmap ("pit_pickoffs" .=) (IN.pit_pickoffs pitStats)
        , fmap ("pit_rbi" .=) (IN.pit_rbi pitStats)
        , fmap ("pit_gamesFinished" .=) (IN.pit_gamesFinished pitStats)
        , fmap ("pit_inheritedRunners" .=) (IN.pit_inheritedRunners pitStats)
        , fmap ("pit_inheritedRunnersScored" .=) (IN.pit_inheritedRunnersScored pitStats)
        , fmap ("pit_catchersInterference" .=) (IN.pit_catchersInterference pitStats)
        , fmap ("pit_sacBunts" .=) (IN.pit_sacBunts pitStats)
        , fmap ("pit_sacFlies" .=) (IN.pit_sacFlies pitStats)
        , fmap ("pit_passedBall" .=) (IN.pit_passedBall pitStats)
        ]

instance ToJSON IN.BattingStats where
    toJSON batStats = object $ catMaybes
        [ fmap ("bat_gamesPlayed" .=) (IN.bat_gamesPlayed batStats)
        , fmap ("bat_flyOuts" .=) (IN.bat_flyOuts batStats)
        , fmap ("bat_groundOuts" .=) (IN.bat_groundOuts batStats)
        , fmap ("bat_runs" .=) (IN.bat_runs batStats)
        , fmap ("bat_doubles" .=) (IN.bat_doubles batStats)
        , fmap ("bat_triples" .=) (IN.bat_triples batStats)
        , fmap ("bat_homeRuns" .=) (IN.bat_homeRuns batStats)
        , fmap ("bat_strikeOuts" .=) (IN.bat_strikeOuts batStats)
        , fmap ("bat_baseOnBalls" .=) (IN.bat_baseOnBalls batStats)
        , fmap ("bat_intentionalWalks" .=) (IN.bat_intentionalWalks batStats)
        , fmap ("bat_hits" .=) (IN.bat_hits batStats)
        , fmap ("bat_hitByPitch" .=) (IN.bat_hitByPitch batStats)
        , fmap ("bat_atBats" .=) (IN.bat_atBats batStats)
        , fmap ("bat_caughtStealing" .=) (IN.bat_caughtStealing batStats)
        , fmap ("bat_stolenBases" .=) (IN.bat_stolenBases batStats)
        , fmap ("bat_groundIntoDoublePlay" .=) (IN.bat_groundIntoDoublePlay batStats)
        , fmap ("bat_groundIntoTriplePlay" .=) (IN.bat_groundIntoTriplePlay batStats)
        , fmap ("bat_plateAppearances" .=) (IN.bat_plateAppearances batStats)
        , fmap ("bat_totalBases" .=) (IN.bat_totalBases batStats)
        , fmap ("bat_rbi" .=) (IN.bat_rbi batStats)
        , fmap ("bat_leftOnBase" .=) (IN.bat_leftOnBase batStats)
        , fmap ("bat_sacBunts" .=) (IN.bat_sacBunts batStats)
        , fmap ("bat_sacFlies" .=) (IN.bat_sacFlies batStats)
        , fmap ("bat_catchersInterference" .=) (IN.bat_catchersInterference batStats)
        , fmap ("bat_pickoffs" .=) (IN.bat_pickoffs batStats)
        ]


convertPlayerToJson :: IN.Player -> ByteString
convertPlayerToJson = BL.toStrict . encode . playerToJsonPlayerData

writePlayerToJsonFile :: FilePath -> IN.Player -> IO ()
writePlayerToJsonFile path player = B.writeFile path (convertPlayerToJson player)