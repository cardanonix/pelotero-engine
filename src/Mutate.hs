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
import MiddleADT ( JsonPlayerData(..)
                 , JsonStatsData(..)
                 )
import OutputADT (OutputData)
import Scraper  ( withEither
                , fetchGameScheduleForDate
                , fetchActiveRoster
                , scheduleUrl
                , hasGamesForDate
                , extractGameIds
                , gameStatusUrl
                , boxScoreUrl
                , rosterUrl
                , fetchAndDecode
                , fetchGameStatus
                , fetchFinishedBxScore
                , processGameIds
                , printProcessedGameData
                , processAndPrintGames
                ,
                )

main :: IO ()
main = do

    -- -- Prompt the user for a date
    -- putStrLn "Enter date to process (YYYY-MM-DD):"
    -- date <- getLine


    -- Completion message
    putStrLn "Processing completed for the given date!"

