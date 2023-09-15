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
        [ fmap ("pit_gamesPlayed" .=) (pit_gamesPlayed pitStats)
        , fmap ("pit_gamesStarted" .=) (pit_gamesStarted pitStats)
        , fmap ("pit_flyOuts" .=) (pit_flyOuts pitStats)
        , fmap ("pit_groundOuts" .=) (pit_groundOuts pitStats)
        , fmap ("pit_airOuts" .=) (pit_airOuts pitStats)
        , fmap ("pit_runs" .=) (pit_runs pitStats)
        , fmap ("pit_doubles" .=) (pit_doubles pitStats)
        , fmap ("pit_triples" .=) (pit_triples pitStats)
        , fmap ("pit_homeRuns" .=) (pit_homeRuns pitStats)
        , fmap ("pit_strikeOuts" .=) (pit_strikeOuts pitStats)
        , fmap ("pit_baseOnBalls" .=) (pit_baseOnBalls pitStats)
        , fmap ("pit_intentionalWalks" .=) (pit_intentionalWalks pitStats)
        , fmap ("pit_hits" .=) (pit_hits pitStats)
        , fmap ("pit_hitByPitch" .=) (pit_hitByPitch pitStats)
        , fmap ("pit_atBats" .=) (pit_atBats pitStats)
        , fmap ("pit_caughtStealing" .=) (pit_caughtStealing pitStats)
        , fmap ("pit_stolenBases" .=) (pit_stolenBases pitStats)
        , fmap ("pit_numberOfPitches" .=) (pit_numberOfPitches pitStats)
        , fmap ("pit_inningsPitched" .=) (pit_inningsPitched pitStats)
        , fmap ("pit_wins" .=) (pit_wins pitStats)
        , fmap ("pit_losses" .=) (pit_losses pitStats)
        , fmap ("pit_saves" .=) (pit_saves pitStats)
        , fmap ("pit_saveOpportunities" .=) (pit_saveOpportunities pitStats)
        , fmap ("pit_holds" .=) (pit_holds pitStats)
        , fmap ("pit_blownSaves" .=) (pit_blownSaves pitStats)
        , fmap ("pit_earnedRuns" .=) (pit_earnedRuns pitStats)
        , fmap ("pit_battersFaced" .=) (pit_battersFaced pitStats)
        , fmap ("pit_outs" .=) (pit_outs pitStats)
        , fmap ("pit_gamesPitched" .=) (pit_gamesPitched pitStats)
        , fmap ("pit_completeGames" .=) (pit_completeGames pitStats)
        , fmap ("pit_shutouts" .=) (pit_shutouts pitStats)
        , fmap ("pit_pitchesThrown" .=) (pit_pitchesThrown pitStats)
        , fmap ("pit_balls" .=) (pit_balls pitStats)
        , fmap ("pit_strikes" .=) (pit_strikes pitStats)
        , fmap ("pit_hitBatsmen" .=) (pit_hitBatsmen pitStats)
        , fmap ("pit_balks" .=) (pit_balks pitStats)
        , fmap ("pit_wildPitches" .=) (pit_wildPitches pitStats)
        , fmap ("pit_pickoffs" .=) (pit_pickoffs pitStats)
        , fmap ("pit_rbi" .=) (pit_rbi pitStats)
        , fmap ("pit_gamesFinished" .=) (pit_gamesFinished pitStats)
        , fmap ("pit_inheritedRunners" .=) (pit_inheritedRunners pitStats)
        , fmap ("pit_inheritedRunnersScored" .=) (pit_inheritedRunnersScored pitStats)
        , fmap ("pit_catchersInterference" .=) (pit_catchersInterference pitStats)
        , fmap ("pit_sacBunts" .=) (pit_sacBunts pitStats)
        , fmap ("pit_sacFlies" .=) (pit_sacFlies pitStats)
        , fmap ("pit_passedBall" .=) (pit_passedBall pitStats)
        ]

instance ToJSON IN.BattingStats where
    toJSON batStats = object $ catMaybes
        [ fmap ("bat_gamesPlayed" .=) (bat_gamesPlayed batStats)
        , fmap ("bat_flyOuts" .=) (bat_flyOuts batStats)
        , fmap ("bat_groundOuts" .=) (bat_groundOuts batStats)
        , fmap ("bat_runs" .=) (bat_runs batStats)
        , fmap ("bat_doubles" .=) (bat_doubles batStats)
        , fmap ("bat_triples" .=) (bat_triples batStats)
        , fmap ("bat_homeRuns" .=) (bat_homeRuns batStats)
        , fmap ("bat_strikeOuts" .=) (bat_strikeOuts batStats)
        , fmap ("bat_baseOnBalls" .=) (bat_baseOnBalls batStats)
        , fmap ("bat_intentionalWalks" .=) (bat_intentionalWalks batStats)
        , fmap ("bat_hits" .=) (bat_hits batStats)
        , fmap ("bat_hitByPitch" .=) (bat_hitByPitch batStats)
        , fmap ("bat_atBats" .=) (bat_atBats batStats)
        , fmap ("bat_caughtStealing" .=) (bat_caughtStealing batStats)
        , fmap ("bat_stolenBases" .=) (bat_stolenBases batStats)
        , fmap ("bat_groundIntoDoublePlay" .=) (bat_groundIntoDoublePlay batStats)
        , fmap ("bat_groundIntoTriplePlay" .=) (bat_groundIntoTriplePlay batStats)
        , fmap ("bat_plateAppearances" .=) (bat_plateAppearances batStats)
        , fmap ("bat_totalBases" .=) (bat_totalBases batStats)
        , fmap ("bat_rbi" .=) (bat_rbi batStats)
        , fmap ("bat_leftOnBase" .=) (bat_leftOnBase batStats)
        , fmap ("bat_sacBunts" .=) (bat_sacBunts batStats)
        , fmap ("bat_sacFlies" .=) (bat_sacFlies batStats)
        , fmap ("bat_catchersInterference" .=) (bat_catchersInterference batStats)
        , fmap ("bat_pickoffs" .=) (bat_pickoffs batStats)
        ]


convertPlayerToJson :: IN.Player -> ByteString
convertPlayerToJson = BL.toStrict . encode . playerToJsonPlayerData

writePlayerToJsonFile :: FilePath -> IN.Player -> IO ()
writePlayerToJsonFile path player = B.writeFile path (convertPlayerToJson player)