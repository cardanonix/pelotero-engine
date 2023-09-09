{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Data.ByteString ( ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString as B (readFile)
import Data.Aeson (decode, Result(Success), FromJSON(..), Value, (.:), (.:?), (.!=), fromJSON, withObject, eitherDecodeStrict)
import Data.Aeson.Types (Parser, Result(..))
import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShowM)
import InputADT
import BoxScoreScraper  ( fetchGameScheduleForDate
                        , hasGamesForDate
                        , extractGameIds
                        , processAndPrintGames
                        , fetchFinishedBxScore
                        , fetchGameStatus
                        )

main :: IO ()
main = do
    gameScheduleResult <- fetchGameScheduleForDate "2023-08-22"

    case gameScheduleResult of
        Right gameSchedule -> do
            putStrLn "Processing games..."
            processAndPrintGames (Right gameSchedule)
        Left errMsg -> putStrLn $ "Failed to fetch game schedule: " ++ errMsg

    putStrLn "Testing fetchGameScheduleForDate:"
    testFetchGameScheduleForDate

    putStrLn "Testing fetchFinishedBxScore:"
    testFetchFinishedBxScore

testFetchGameScheduleForDate :: IO ()
testFetchGameScheduleForDate = do
    result <- fetchGameScheduleForDate "2023-08-22"
    case result of
        Left errMsg -> putStrLn $ "Failed to fetch game schedule: " ++ errMsg
        Right schedule -> putStrLn $ "Fetched game schedule: " ++ show schedule

testFetchFinishedBxScore :: IO ()
testFetchFinishedBxScore = do
    jsonData <- B.readFile "testFiles/716896_boxscore.json"
    let gameId = 716896 -- example game ID
    result <- fetchFinishedBxScore gameId
    case result of
        Left errMsg -> putStrLn $ "Failed to fetch game box score: " ++ errMsg
        Right gameData -> putStrLn $ "Fetched box score: " ++ show gameData
    -- eventually compare the fetched result to jsonData or other expected data...