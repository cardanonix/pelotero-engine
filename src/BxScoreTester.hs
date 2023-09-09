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
    testFetchGameScheduleForDate "2023-08-22"

    putStrLn "Testing fetchFinishedBxScore:"
    testFetchFinishedBxScore 716896

testFetchGameScheduleForDate :: String -> IO ()
testFetchGameScheduleForDate  date = do
    result <- fetchGameScheduleForDate date
    case result of
        Left errMsg -> putStrLn $ "Failed to fetch game schedule: " ++ errMsg
        Right schedule -> putStrLn $ "Fetched game schedule: " ++ show schedule

testFetchFinishedBxScore :: Int -> IO ()
testFetchFinishedBxScore game = do
    jsonData <- B.readFile "testFiles/716896_boxscore.json"
    let pickedgame = game
    result <- fetchFinishedBxScore pickedgame
    case result of
        Left errMsg -> putStrLn $ "Failed to fetch game box score: " ++ errMsg
        Right gameData -> putStrLn $ "Fetched box score: " ++ show gameData
    -- eventually compare the fetched result to jsonData or other expected data...