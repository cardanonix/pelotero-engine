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
import InputADT ( GameData )
import BoxScoreScraper  ( fetchGameScheduleForDate
                        , hasGamesForDate
                        , extractGameIds
                        , processAndPrintGames
                        , fetchFinishedBxScore
                        , fetchGameStatus
                        )

main :: IO ()
main = do
    jsonData <- B.readFile "testFiles/716896_boxscore.json"
    let parsedResult = eitherDecodeStrict jsonData :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
    handPicked <- B.readFile "testFiles/shortened.json"
    let parsedResult = eitherDecodeStrict handPicked :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
    
    -- -- testing BoxscoreScraper
    -- -- Fetch game schedule for a specific date, e.g., "2023-09-05"
    -- gameScheduleMaybe <- fetchGameScheduleForDate "2023-08-22"
    -- let parsedThingey = eitherDecodeStrict gameScheduleMaybe :: Either String GameData
    -- case parsedThingey of
    --     Left err -> putStrLn $ "Failed to parse JSON: " ++ err
    --     Right gameData -> print gameData
    