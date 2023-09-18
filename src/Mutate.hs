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
import Scraper ( scrapeDataForDateRange )
import System.Environment (getArgs)

main :: IO ()
main = do
    let startDate = "2023-08-22"  -- or whatever your start date is
        endDate   = "2023-08-22"  -- or whatever your end date is
    scrapeDataForDateRange startDate endDate