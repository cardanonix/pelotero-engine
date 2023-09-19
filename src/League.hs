{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

import ADT_Config
import ADT_Roster 

import Data.Foldable (foldl')

main :: IO ()
main = do
    jsonConfig <- B.readFile "testFiles/prototype_config/config.json"
    let parsedConfig = eitherDecodeStrict jsonConfig :: Either String Configuration

    jsonInvalidRoster <- B.readFile "testFiles/prototype_config/invalid_roster.json"
    let parsedInvalidRoster = eitherDecodeStrict jsonInvalidRoster :: Either String LgManager

    jsonValidRoster <- B.readFile "testFiles/prototype_config/valid_roster.json"
    let parsedValidRoster = eitherDecodeStrict jsonValidRoster :: Either String LgManager

    case parsedConfig of
        Left errConfig -> putStrLn $ "Failed to parse Config JSON: " ++ errConfig
        Right config -> do
            putStrLn "Parsed Configuration:"
            print config
            putStrLn "\nTesting with Valid Roster:"
            testRoster config parsedValidRoster

            putStrLn "\nTesting with Invalid Roster:"
            testRoster config parsedInvalidRoster

testRoster :: Configuration -> Either String LgManager -> IO ()
testRoster _ (Left errRoster) = putStrLn $ "Failed to parse Roster JSON: " ++ errRoster
testRoster config (Right lgManager) = do
    print $ current_lineup lgManager 
    print lgManager
    isValid <- validateAndPrint lgManager config
    if isValid 
        then putStrLn "This Lineup is valid as fuck, yo!"
        else putStrLn "That lineup has some serious discrepancies, bro!"


-- Function to validate the current lineup with the league's roster configuration
-- Pure function that just returns Bool
validateCurrentLineup :: LgManager -> Configuration -> Bool
validateCurrentLineup LgManager{..} Configuration{point_parameters = PointParameters{valid_roster = rosterConfig}} =
    null $ getDiscrepancies current_lineup rosterConfig

getDiscrepancies :: CurrentLineup -> LgRoster -> [(String, Int)]
getDiscrepancies CurrentLineup{..} LgRoster{..} =
    let discrepancies =
          [ ("Catcher", length [cC] - lg_catcher)
          , ("First Base", length [b1C] - lg_first)
          , ("Second Base", length [b2C] - lg_second)
          , ("Third Base", length [b3C] - lg_third)
          , ("Shortstop", length [ssC] - lg_shortstop)
          , ("Outfield", length ofC - lg_outfield)
          , ("Utility", length [uC] - lg_utility)
          , ("Starting Pitcher", length spC - lg_s_pitcher)
          , ("Relief Pitcher", length rpC - lg_r_pitcher)
          ]
    in filter ((/= 0) . snd) discrepancies

-- Function to get the discrepancies between the current lineup and the league's roster configuration
-- Function to print discrepancies
printDiscrepancies :: CurrentLineup -> LgRoster -> IO ()
printDiscrepancies lineup rosterConfig = do
    let discrepancies = getDiscrepancies lineup rosterConfig
    mapM_ printDifference discrepancies
  where
    printDifference (pos, diff)
      | diff > 0 = putStrLn $ "Too many players for " ++ pos ++ " by " ++ show diff
      | diff < 0 = putStrLn $ "Too few players for " ++ pos ++ " by " ++ show (abs diff)

validateAndPrint :: LgManager -> Configuration -> IO Bool
validateAndPrint manager config = do
    let valid = validateCurrentLineup manager config
    if valid
        then return True
        else do
            printDiscrepancies (current_lineup manager) (valid_roster . point_parameters $ config)
            return False
