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
-- import Scraper ( scrapeDataForDateRange )
import System.Environment (getArgs)
-- import DayStats

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Match Funded by Two Managers (Can be Randomly Paired or Chosen)"
    putStrLn ""
    putStrLn ""
    putStrLn "Verify Player 1 & 2 Roster"
    putStrLn "Interactive Validation of League Paramaters"
    putStrLn ""
    putStrLn "Scrape Active Rosters"
    putStrLn "Interactive Manager Player-Ranking for Auto draft"
    putStrLn "Match Funded by Two Players which finalizes everything"
    putStrLn "Auto-Draft"
    putStrLn ""
    putStrLn "Verify Player 1 & 2 Rosters"
    putStrLn ""
    putStrLn "Set Lineup"
    putStrLn "Verify Player 1 & 2 Lineup"
    putStrLn "  (players are able to edit their lineup until a specified time Before the natch begins)"
    putStrLn ""
    putStrLn "Wait for Match-Timeframe to Expire"
    putStrLn "Fetch Updated Stats"
    putStrLn ""
    putStrLn "Calculate & compare Player 1 & 2 Points in TimeFrame"
    putStrLn ""
    putStrLn "Disbursing Winnings to Higher Score"