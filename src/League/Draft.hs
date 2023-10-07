{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (filterM)
import Data.Aeson (
    FromJSON (..),
    Result (Success),
    Value,
    decode,
    eitherDecodeStrict,
    fromJSON,
    withObject,
    (.!=),
    (.:),
    (.:?),
 )
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShowM)
import Scraper (scrapeDataForDateRange)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [startDate, endDate] -> scrapeDataForDateRange startDate endDate
        _ -> putStrLn "Usage: fetchStats <start-date> <end-date>"
