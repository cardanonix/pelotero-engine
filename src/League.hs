{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Data.ByteString ( ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Aeson (decode, Result(Success), FromJSON(..), Value, (.:), (.:?), (.!=), fromJSON, withObject, eitherDecodeStrict)
import Data.Aeson.Types (Parser, Result(..))
import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShowM)

import ADT_Config (Configuration)
import ADT_Roster (CurrentLineup, LgManager, current_lineup)

main :: IO ()
main = do

    -- decoding json file with ADT_Config using Data.Aeson.Types.FromJSON   
    jsonConfig <- B.readFile "appData/config/config.json"
    let parsedResult = eitherDecodeStrict jsonConfig :: Either String Configuration
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse Config JSON: " ++ err
        Right config -> print config
    
        -- decoding json file with ADT_Config using Data.Aeson.Types.FromJSON   
    jsonRoster <- B.readFile "appData/rosters/roster.json"
    let parsedResult = eitherDecodeStrict jsonRoster :: Either String LgManager
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse Roster JSON: " ++ err
        Right lgManager -> do
            print $ current_lineup lgManager 
            print lgManager  
