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
import Scraper
import ADT_Input
import ADT_Middle
import System.Environment (getArgs)

main :: IO ()
main = do
    let rosterPath = "appData/activePlayers.json"
    activeRoster <- fetchActiveRoster 2023 -- assuming 2023 is the season you're fetching for
    case activeRoster of
        Left err -> putStrLn $ "Failed to fetch active roster: " ++ err
        Right rosterData -> writeRosterToFile rosterPath rosterData
