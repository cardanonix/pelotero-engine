{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Data.ByteString ( ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Aeson (decode, Result(Success), FromJSON(..), Value, (.:), (.:?), (.!=), fromJSON, withObject, eitherDecodeStrict)
import Data.Aeson.Types (Parser, Result(..))
import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShowM)
import InputADT ( GameData
                , LiveGameWrapper
                , GameSchedule
                )
import OutputADT (OutputData)
import Scraper  ( fetchGameScheduleForDate
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
                )

main :: IO ()
main = do


    jsonData <- B.readFile "testFiles/mlb/boxscore_716896.json"
    let parsedResult = eitherDecodeStrict jsonData :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
    -- handPicked <- B.readFile "testFiles/shortened.json"
    -- let parsedResult = eitherDecodeStrict handPicked :: Either String GameData
    -- case parsedResult of
    --     Left err -> putStrLn $ "Failed to parse JSON: " ++ err
    --     Right gameData -> print gameData
    jsonData <- B.readFile "testFiles/mlb/boxscore_716896.json"
    let parsedResult = eitherDecodeStrict jsonData :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
    -- testing BoxscoreScraper
    -- Fetch game schedule for a specific date, e.g., "2023-09-05"
    -- gameScheduleMaybe <- fetchGameScheduleForDate "2023-08-22"
    -- let parsedThingey = eitherDecodeStrict gameScheduleMaybe :: Either String GameData
    -- case parsedThingey of
    --     Left err -> putStrLn $ "Failed to parse JSON: " ++ err
    --     Right gameData -> print gameData

    -- gameScheduleResult <- fetchGameScheduleForDate "2023-08-22"

    -- case gameScheduleResult of
    --     Right gameSchedule -> do
    --         putStrLn "Processing games..."
    --         putStrLn ".."
    --         putStrLn "..."
    --         putStrLn "HERE THEY ARE!"
    --         processAndPrintGames (Right gameSchedule)
    --     Left errMsg -> putStrLn $ "Failed to fetch game schedule: " ++ errMsg

    -- -- putStrLn "Testing fetchGameScheduleForDate:"
    -- -- testFetchGameScheduleForDate "2023-08-22"

    -- -- putStrLn "Testing fetchFinishedBxScore:"
    -- -- testFetchFinishedBxScore 716896

-- testFetchGameScheduleForDate :: String -> IO ()
-- testFetchGameScheduleForDate  date = do
--     result <- fetchGameScheduleForDate date
--     case result of
--         Left errMsg -> putStrLn $ "Failed to fetch game schedule: " ++ errMsg
--         Right schedule -> putStrLn $ "Fetched game schedule: " ++ show schedule

-- testFetchFinishedBxScore :: Int -> IO ()
-- testFetchFinishedBxScore game = do
--     jsonData <- B.readFile "testFiles/mlb/boxscore_716896.json"
--     let pickedgame = game
--     result <- fetchFinishedBxScore pickedgame
--     case result of
--         Left errMsg -> putStrLn $ "Failed to fetch game box score: " ++ errMsg
--         Right gameData -> putStrLn $ "Fetched box score: " ++ show gameData
--     -- eventually compare the fetched result to jsonData or other expected data...
