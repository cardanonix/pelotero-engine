{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}

module OutputADT where

import Control.Monad (filterM)
import Data.Aeson
    ( FromJSON(..),
      Result(Success),
      Value,
      decode,
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
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Aeson.Key as K
import InputADT (BattingStats, GameData (..), PitchingStats, Player (..), PlayerStats (..), Position (..), TeamData (..), Teams (..), fullName, parentTeamId, person, personId, pitching, status, status_code, stats, allPositions, batting)
import qualified Data.Map.Strict as M

-- Convert function
convertGameDataToOutputData :: GameData -> OutputData
convertGameDataToOutputData gameData = 
    let
        -- Extract player data from TeamData and create PlayerData
        extractPlayerData :: TeamData -> [PlayerData]
        extractPlayerData teamData =
            [ PlayerData 
                { pd_player_id = personId $ person player
                , pd_fullName = fullName $ person player
                , pd_stats = M.fromList [(parentTeamId player, convertPlayerStats player (stats player))]
                }
            | player <- M.elems $ players teamData]

        -- Convert PlayerStats to PlayerStatsOutput
-- Convert PlayerStats to PlayerStatsOutput
        convertPlayerStats :: Player -> PlayerStats -> PlayerStatsOutput
        convertPlayerStats player playerStats = 
            PlayerStatsOutput
            { pso_parentTeamId = parentTeamId player
            , pso_allPositions = allPositions player
            , pso_status = status_code $ status player
            , pso_batting = batting playerStats
            , pso_pitching = pitching playerStats
            }

        allPlayerData = concat [extractPlayerData (away $ teams gameData), extractPlayerData (home $ teams gameData)]
        playerMap = M.fromList [(pd_player_id pd, pd) | pd <- allPlayerData]

    in 
    OutputData
    { od_players = playerMap
    , od_games = M.empty  -- dummy value, needs to be filled in properly
    , od_checksum = "c0d122700c4f7b97b39d485c179556db02d8ca4113fec5a18ba1cde0b6be28e2"  -- TODO: calculate checksum
    , od_date = "2023_08_22"  -- dummy value, needs to be filled in properly
    }

data PlayerData = PlayerData
  { pd_player_id  :: Int
  , pd_fullName   :: Text
  , pd_stats      :: M.Map Int PlayerStatsOutput
  } deriving (Show, Eq)

data PlayerStatsOutput = PlayerStatsOutput
  { pso_parentTeamId :: Int
  , pso_allPositions :: Maybe [Position]
  , pso_status       :: Text
  , pso_batting      :: Maybe BattingStats
  , pso_pitching     :: Maybe PitchingStats
  } deriving (Show, Eq)

data OutputData = OutputData
  { od_players   :: M.Map Int PlayerData
  , od_games     :: M.Map Int Text
  , od_checksum  :: Text
  , od_date      :: Text
  } deriving (Show, Eq)

instance ToJSON OutputData where
  toJSON od = object
    [ "players"   .= od_players od
    , "games"     .= od_games od
    , "checksum"  .= od_checksum od
    , "date"      .= od_date od
    ]

instance ToJSON PlayerData where
  toJSON pd = object
    [ "player_id" .= pd_player_id pd
    , "fullName"  .= pd_fullName pd
    , "stats"     .= pd_stats pd
    ]

instance ToJSON PlayerStatsOutput where
  toJSON pso = object $ catMaybes
    [ Just ("parentTeamId" .= pso_parentTeamId pso)
    , fmap ("allPositions" .=) (pso_allPositions pso)
    , Just ("status" .= pso_status pso)
    , fmap ("batting" .=) (pso_batting pso)
    , fmap ("pitching" .=) (pso_pitching pso)
    ]