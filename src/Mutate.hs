{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Data.ByteString ( ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Aeson   ( decode
                    , Result(Success)
                    , FromJSON(..)
                    , Value
                    , (.:), (.:?), (.!=)
                    , fromJSON
                    , withObject
                    , eitherDecodeStrict
                    )
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
                , mergeOutputData
                , outputFilePath
                , generateChecksum 
                , processDate
                , processDateRange
                )

main :: IO ()
main = do

    -- -- Prompt the user for a date
    -- putStrLn "Enter date to process (YYYY-MM-DD):"
    -- date <- getLine

    -- Process the given date
    processDate "2023-08-22"

    -- Completion message
    putStrLn "Processing completed for the given date!"