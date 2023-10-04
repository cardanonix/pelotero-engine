{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}

module Stats where

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

import qualified Middle as M

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

instance FromJSON JsonPlayerData where
    fromJSON :: JsonPlayerData -> Value
    fromJSON (JsonPlayerData pid fname sts) =
        object ["fullName" .= fname, "player_id" .= pid, "stats" .= sts]

instance FromJSON JsonStatsData where
    fromJSON :: JsonStatsData -> Value
    fromJSON (JsonStatsData pId allPos statCode bat pitch) =
        object ["parentTeamId" .= pId, "allPositions" .= allPos, "status" .= statCode, "batting" .= bat, "pitching" .= pitch]

instance FromJSON I.Position where
    toJSON :: I.Position -> Value
    toJSON (I.Position allPositions) = 
        let positionValue = read (T.unpack allPositions) :: Int 
        in Number (fromIntegral positionValue)

instance FromJSON I.PitchingStats where
    fromJSON :: I.PitchingStats -> Value
    fromJSON pitStats = object $ catMaybes
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

instance FromJSON I.BattingStats where
    fromJSON :: I.BattingStats -> Value
    fromJSON batStats = object $ catMaybes
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


auto recommendation:
``
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( JSONStats (..)
    , JSONStatsValue (..)
    , Stat (..)
    , Pitching (..)
    , Status (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

-- a newtype to make gameId's more formalized inside of functions
newtype StatGameId = StatGameId { gameId :: Text } deriving (Show Eq)

type JSONStats = HashMap Text JSONStatsValue

data JSONStatsPlayerValue = JSONStatsPlayerValue
    { fullNameJSONStatsValue :: Text
    , playerIDJSONStatsValue :: Text
    , statsJSONStatsValue :: HashMap StatGameId JSONStatsValue
    } deriving (Show)

data JSONStatsValue = JSONStatsValue
    { allPositionsStat :: Maybe ([Int])
    , parentTeamIDStat :: Maybe Int
    , battingStat :: Maybe I.Batting
    , pitchingStat :: Maybe I.Pitching
    , statusStat :: Maybe I.Status
    } deriving (Show)


decodeTopLevel :: ByteString -> Maybe JSONStats
decodeTopLevel = decode

instance FromJSON JSONStatsPlayerValue where
    parseJSON (Object v) = JSONStatsPlayerValue
        <$> v .: "fullName"
        <*> v .: "player_id"
        <*> v .: "stats"

instance FromJSON JSONStatsValue where
    parseJSON (Object v) = JSONStatsValue
        <$> v .: "allPositions"
        <*> v .: "batting"
        <*> v .: "parentTeamId"
        <*> v .: "pitching"
        <*> v .: "status"

instance FromJSON I.BattingStats where
    fromJSON :: I.BattingStats -> Value
    parseJSON (Object v) = Pitching
        <$> v .:? "pit_airOuts"
        <*> v .:? "pit_atBats"
        <*> v .:? "pit_balks"
        <*> v .:? "pit_balls"
        <*> v .:? "pit_baseOnBalls"
        <*> v .:? "pit_battersFaced"
        <*> v .:? "pit_blownSaves"
        <*> v .:? "pit_catchersInterference"
        <*> v .:? "pit_caughtStealing"
        <*> v .:? "pit_completeGames"
        <*> v .:? "pit_doubles"
        <*> v .:? "pit_earnedRuns"
        <*> v .:? "pit_gamesFinished"
        <*> v .:? "pit_gamesPitched"
        <*> v .:? "pit_gamesPlayed"
        <*> v .:? "pit_gamesStarted"
        <*> v .:? "pit_groundOuts"
        <*> v .:? "pit_hitBatsmen"
        <*> v .:? "pit_hitByPitch"
        <*> v .:? "pit_hits"
        <*> v .:? "pit_holds"
        <*> v .:? "pit_homeRuns"
        <*> v .:? "pit_inheritedRunners"
        <*> v .:? "pit_inheritedRunnersScored"
        <*> v .:? "pit_inningsPitched"
        <*> v .:? "pit_intentionalWalks"
        <*> v .:? "pit_losses"
        <*> v .:? "pit_numberOfPitches"
        <*> v .:? "pit_outs"
        <*> v .:? "pit_passedBall"
        <*> v .:? "pit_pickoffs"
        <*> v .:? "pit_pitchesThrown"
        <*> v .:? "pit_rbi"
        <*> v .:? "pit_runs"
        <*> v .:? "pit_sacBunts"
        <*> v .:? "pit_sacFlies"
        <*> v .:? "pit_saveOpportunities"
        <*> v .:? "pit_saves"
        <*> v .:? "pit_shutouts"
        <*> v .:? "pit_stolenBases"
        <*> v .:? "pit_strikeOuts"
        <*> v .:? "pit_strikes"
        <*> v .:? "pit_triples"
        <*> v .:? "pit_wildPitches"
        <*> v .:? "pit_wins"
        <*> v .:? "pit_flyOuts"

instance FromJSON I.BattingStats where
    fromJSON :: I.BattingStats -> Value
    fromJSON batStats = object $ catMaybes
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

instance FromJSON Status where
    parseJSON = withText "Status" parseText
        where
            parseText "A" = return AStatus
