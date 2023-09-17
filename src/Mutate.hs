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
import Scraper  ( fetchGameScheduleForDate
                , fetchActiveRoster
                , fetchGameStatus
                , fetchFinishedBxScore
                , printGameData
                , processAndPrintGames
                , scrapeDataForDateRange
                )
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [start, end] -> scrapeDataForDateRange start end
        _            -> putStrLn "Usage: <executable> <start_date> <end_date>"
