{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}

module Scraper
    ( fetchGameScheduleForDate
    , scheduleUrl
    , hasGamesForDate 
    , extractGameIds 
    , gameStatusUrl 
    , boxScoreUrl 
    , fetchAndDecode 
    , fetchGameStatus
    , fetchFinishedBxScore 
    , processGameIds 
    , printProcessedGameData
    , processAndPrintGames 
    , convertGameDataToOutputData
    , GameSchedule
    , DateEntry
    , GameID
    , LiveGameStatus
    , LiveGameWrapper
    ) where

import Network.HTTP.Simple
    ( parseRequest_,
      getResponseBody,
      httpBS,
      httpBS,
      parseRequest_,
      getResponseBody )
import Data.Time ()
import Data.Time.Clock.POSIX ()
import Data.ByteString (ByteString, empty)
import qualified Data.Vector as V
import Data.Maybe (isJust, fromMaybe, maybeToList)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Aeson
    ( eitherDecode,
      eitherDecodeStrict,
      encode,
      (.:),
      (.:?),
      genericParseJSON,
      withObject,
      defaultOptions,
      FromJSON(parseJSON),
      Options(fieldLabelModifier),
      eitherDecodeStrict )
import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified InputADT as I
import InputADT
    ( GameData(..)
    , LiveGameStatusWrapper(..)
    , LiveGameWrapper(..)
    , LiveGameStatus(..)
    , DateEntry(..)
    , GameSchedule(..)
    , GameID(..)
    , TeamData(..)
    , Player (..)
    , PlayerStats (..)
    )
import qualified OutputADT as O
import OutputADT
    ( PlayerData (..)
    , PlayerStatsOutput (..)
    , OutputData (..)
    )

-- takes a date string "YYYY-MM-DD" and outputs a schedule bytestring of that day schdule
fetchGameScheduleForDate :: String -> IO (Either String I.GameSchedule)
fetchGameScheduleForDate date = fetchAndDecode (scheduleUrl date)

-- Generate the API URL for a single day's schedule
scheduleUrl :: String -> String
scheduleUrl date = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date

-- Takes a schedule bytestring and outputs true if games are happening, false otherwise.
hasGamesForDate :: I.GameSchedule -> Bool
hasGamesForDate schedule = any (isJust . games) (dates schedule)

-- Takes a schedule bytestring and outputs an array of gameId's or errors
extractGameIds :: I.GameSchedule -> [Int]
extractGameIds gameData = concatMap (maybe [] (V.toList . fmap gamePk) . games) (dates gameData)

-- Generate the API URL for live game status
gameStatusUrl :: Int -> String
gameStatusUrl gameId = "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live"

-- Generate the API URL for finished boxscore
boxScoreUrl :: Int -> String
boxScoreUrl gameId = "http://statsapi.mlb.com/api/v1/game/" ++ show gameId ++ "/boxscore"

-- Fetch and decode utility
fetchAndDecode :: FromJSON a => String -> IO (Either String a)
fetchAndDecode url = do
    response <- httpBS (parseRequest_ url)
    return $ eitherDecodeStrict $ getResponseBody response

-- takes a gameId and returns IO (Either String LiveGameWrapper)
fetchGameStatus :: Int -> IO (Either String I.LiveGameWrapper)
fetchGameStatus gameId = fetchAndDecode (gameStatusUrl gameId)

-- takes a gameId and returns IO (Either String GameData)
fetchFinishedBxScore :: Int -> IO (Either String I.GameData)
fetchFinishedBxScore gameId = do
    gameStatusResult <- fetchGameStatus gameId
    case gameStatusResult of
        Right gameDataWrapper -> do
            let liveStatusWrapper = gameData gameDataWrapper
            let liveStatus = gameStatus liveStatusWrapper
            if codedGameState liveStatus == "F"
               then fetchAndDecode (boxScoreUrl gameId)
               else return $ Left "Game isn't finished yet"
        Left err -> return $ Left ("Error fetching game status: " ++ err)

--maps fetchFinishedBxScore over an array of game id's and returns IO (M.Map Int ByteString)
processGameIds :: [Int] -> IO (Either String (M.Map Int I.GameData))
processGameIds gameIds = do
    results <- mapM fetchFinishedBxScore gameIds
    case sequence results of
        Left err -> return $ Left err
        Right gameDataList -> return $ Right $ M.fromList $ zip gameIds gameDataList

-- takes a list of tuples game id's and game data and prints them
printProcessedGameData :: Either String (M.Map Int I.GameData) -> IO ()
printProcessedGameData gameDataMapEither =
    case gameDataMapEither of
        Left errMsg -> putStrLn errMsg
        Right gameDataMap -> mapM_ (\(gameId, gameData) -> putStrLn $ show gameId ++ ": " ++ show gameData) (M.toList gameDataMap)

-- print the schedule bytestring
processAndPrintGames :: Either String I.GameSchedule -> IO ()
processAndPrintGames gameScheduleEither =
    case gameScheduleEither of
        Left errMsg -> putStrLn errMsg
        Right gameSchedule ->
            if hasGamesForDate gameSchedule
            then do
                let gameIds = extractGameIds gameSchedule
                gameDataMap <- processGameIds gameIds
                printProcessedGameData gameDataMap
            else putStrLn "No games scheduled for the provided date."

-- ## OUTPUT CONVERSION ##

-- Convert from Input to Output function
convertGameDataToOutputData :: I.GameData -> O.OutputData
convertGameDataToOutputData gameData =
    let
        -- Extract player data from TeamData and create PlayerData
        extractPlayerData :: I.TeamData -> [O.PlayerData]
        extractPlayerData teamData =
            [ PlayerData
                { pd_player_id = I.personId $ person player
                , pd_fullName = I.fullName $ person player
                , pd_stats = M.fromList [(parentTeamId player, convertPlayerStats player (stats player))]
                }
            | player <- M.elems $ players teamData]
        -- Convert PlayerStats to PlayerStatsOutput
        convertPlayerStats :: I.Player -> I.PlayerStats -> O.PlayerStatsOutput
        convertPlayerStats player playerStats =
            PlayerStatsOutput
            { pso_parentTeamId = I.parentTeamId player
            , pso_allPositions = I.allPositions player
            , pso_status = I.status_code $ status player
            , pso_batting = I.batting playerStats
            , pso_pitching = I.pitching playerStats
            }

        allPlayerData = (extractPlayerData (I.away $ teams gameData) ++ extractPlayerData (I.home $ teams gameData))
        playerMap = M.fromList [(pd_player_id pd, pd) | pd <- allPlayerData]

    in
    OutputData
    { od_players = playerMap
    , od_games = M.empty  -- dummy value, needs to be filled in properly
    , od_checksum = "c0d122700c4f7b97b39d485c179556db02d8ca4113fec5a18ba1cde0b6be28e2"  -- TODO: calculate checksum
    , od_date = "2023_08_22"  -- dummy value, needs to be filled in properly
    }
