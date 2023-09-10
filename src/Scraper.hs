{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DoAndIfThenElse #-}

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
import Data.Time
    ( addDays, diffDays, parseTimeOrError, defaultTimeLocale )
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
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Crypto.Hash.SHA256 as SHA256

import qualified InputADT as IN
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
import qualified OutputADT as OUT
import OutputADT
    ( PlayerData (..)
    , PlayerStatsOutput (..)
    , OutputData (..)
    )

-- takes a date string "YYYY-MM-DD" and outputs a schedule bytestring of that day schdule
fetchGameScheduleForDate :: String -> IO (Either String IN.GameSchedule)
fetchGameScheduleForDate date = fetchAndDecode (scheduleUrl date)

-- Generate the API URL for a single day's schedule
scheduleUrl :: String -> String
scheduleUrl date = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date

-- Takes a schedule bytestring and outputs true if games are happening, false otherwise.
hasGamesForDate :: IN.GameSchedule -> Bool
hasGamesForDate schedule = any (isJust . games) (dates schedule)

-- Takes a schedule bytestring and outputs an array of gameId's or errors
extractGameIds :: IN.GameSchedule -> [Int]
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
fetchGameStatus :: Int -> IO (Either String IN.LiveGameWrapper)
fetchGameStatus gameId = fetchAndDecode (gameStatusUrl gameId)

-- takes a gameId and returns IO (Either String GameData)
fetchFinishedBxScore :: Int -> IO (Either String IN.GameData)
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
processGameIds :: [Int] -> IO (Either String (M.Map Int IN.GameData))
processGameIds gameIds = do
    results <- mapM fetchFinishedBxScore gameIds
    case sequence results of
        Left err -> return $ Left err
        Right gameDataList -> return $ Right $ M.fromList $ zip gameIds gameDataList

-- takes a list of tuples game id's and game data and prints them
printProcessedGameData :: Either String (M.Map Int IN.GameData) -> IO ()
printProcessedGameData gameDataMapEither =
    case gameDataMapEither of
        Left errMsg -> putStrLn errMsg
        Right gameDataMap -> mapM_ (\(gameId, gameData) -> putStrLn $ show gameId ++ ": " ++ show gameData) (M.toList gameDataMap)

-- print the schedule bytestring
processAndPrintGames :: Either String IN.GameSchedule -> IO ()
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
convertGameDataToOutputData :: IN.GameData -> OUT.OutputData
convertGameDataToOutputData gameData =
    let
        -- Extract player data from TeamData and create PlayerData
        extractPlayerData :: IN.TeamData -> [OUT.PlayerData]
        extractPlayerData teamData =
            [ PlayerData
                { pd_player_id = IN.personId $ person player
                , pd_fullName = IN.fullName $ person player
                , pd_stats = M.fromList [(parentTeamId player, convertPlayerStats player (stats player))]
                }
            | player <- M.elems $ players teamData]
        -- Convert PlayerStats to PlayerStatsOutput
        convertPlayerStats :: IN.Player -> IN.PlayerStats -> OUT.PlayerStatsOutput
        convertPlayerStats player playerStats =
            PlayerStatsOutput
            { pso_parentTeamId = IN.parentTeamId player
            , pso_allPositions = IN.allPositions player
            , pso_status = IN.status_code $ status player
            , pso_batting = IN.batting playerStats
            , pso_pitching = IN.pitching playerStats
            }

        allPlayerData = (extractPlayerData (IN.away $ teams gameData) ++ extractPlayerData (IN.home $ teams gameData))
        playerMap = M.fromList [(pd_player_id pd, pd) | pd <- allPlayerData]

    in
    OutputData
    { od_players = playerMap
    , od_games = M.empty  -- dummy value, needs to be filled in properly
    , od_checksum = "c0d122700c4f7b97b39d485c179556db02d8ca4113fec5a18ba1cde0b6be28e2"  -- TODO: calculate checksum
    , od_date = "2023_08_22"  -- dummy value, needs to be filled in properly
    }

-- ## New Stuff ##
{- 
isChecksumDifferent :: BS.ByteString -> String -> Bool
isChecksumDifferent date checksum = do
    let filePath = outputFilePath date
    not (doesFileExist filePath)
  ||
    (do fileData <- BS.readFile filePath
        let fileChecksum = generateChecksum fileData
        fileChecksum /= checksum)

mergeOutputData :: [OUT.OutputData] -> OUT.OutputData
 
outputFilePath :: String -> String

-- Generate the SHA-256 checksum for a given ByteString
generateChecksum :: BS.ByteString -> String
generateChecksum bs = show $ SHA256.hashlazy bs

-- Fetch, process, and save game data for a given date
processDate :: String -> IO ()
processDate date = do
    gameScheduleEither <- fetchGameScheduleForDate date
    case gameScheduleEither of
        Left errMsg -> putStrLn errMsg
        Right gameSchedule -> do
            if hasGamesForDate gameSchedule then do
                let gameIds = extractGameIds gameSchedule
                gameDataMapEither <- processGameIds gameIds
                case gameDataMapEither of
                    Left errMsg -> putStrLn errMsg
                    Right gameDataMap -> do
                        let gameDataList = M.elems gameDataMap
                        let outputDataList = map convertGameDataToOutputData gameDataList
                        let mergedOutputData = mergeOutputData outputDataList
                        let jsonData = encode mergedOutputData
                        let checksum = generateChecksum jsonData
                        if isChecksumDifferent date checksum then
                            BS.writeFile (outputFilePath date) jsonData
                        else
                            putStrLn $ "Data for " ++ date ++ " has not changed, skipping..."
            else
                putStrLn $ "No games scheduled for " ++ date

-- The master function
processDateRange :: String -> String -> IO ()
processDateRange startDate endDate = do
    let startDay = parseTimeOrError False defaultTimeLocale "%Y-%m-%d" startDate :: Day
    let endDay = parseTimeOrError False defaultTimeLocale "%Y-%m-%d" endDate :: Day
    let daysBetween = [0..diffDays endDay startDay]
    let dateStrings = map (\d -> formatTime defaultTimeLocale "%Y-%m-%d" (addDays d startDay)) daysBetween
    mapM_ processDate dateStrings
 -}