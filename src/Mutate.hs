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
                )
import System.Environment (getArgs)

main :: IO ()
main = do
    -- -- decoding json file with Configuration ADT using Data.Aeson.Types.FromJSON
    -- jsonConfig <- B.readFile "testFiles/prototype_config/config.json"
    -- let parsedResult = eitherDecodeStrict jsonConfig :: Either String Configuration
    -- case parsedResult of
    --     Left err -> putStrLn $ "Failed to parse JSON: " ++ err
    --     Right config -> print config

    -- -- decoding json file with ActivePlayer InputADT using Data.Aeson.Types.FromJSON
    -- jsonRoster <- B.readFile "testFiles/mlb/activePlayers.json"
    -- let parsedResult = eitherDecodeStrict jsonRoster :: Either String ActiveRoster
    -- case parsedResult of
    --     Left err -> putStrLn $ "Failed to parse JSON: " ++ err
    --     Right rosterData -> print rosterData

    -- -- decoding json file with GameSchedule InputADT using Data.Aeson.Types.FromJSON
    -- jsonSchedule <- B.readFile "testFiles/mlb/schedule_2023-08-22.json"
    -- let parsedResult = eitherDecodeStrict jsonSchedule :: Either String GameSchedule
    -- case parsedResult of
    --     Left err -> putStrLn $ "Failed to parse JSON: " ++ err
    --     Right gameSchedule -> print gameSchedule

    -- -- decoding json file with LiveGameWrapper InputADT using Data.Aeson.Types.FromJSON
    -- jsonLiveFeed <- B.readFile "testFiles/mlb/livefeed_716896.json"
    -- let parsedResult = eitherDecodeStrict jsonLiveFeed :: Either String LiveGameWrapper
    -- case parsedResult of
    --     Left err -> putStrLn $ "Failed to parse JSON: " ++ err
    --     Right lvgameStatus -> print lvgameStatus

    -- decoding json file with GameData InputADT using Data.Aeson.Types.FromJSON   
    jsonBoxScore <- B.readFile "testFiles/mlb/boxscore_716896.json"
    let parsedResult = eitherDecodeStrict jsonBoxScore :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData