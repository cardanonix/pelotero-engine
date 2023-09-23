{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Text (Text)
import Data.ByteString ( ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Aeson (decode, Result(Success), FromJSON(..), Value, (.:), (.:?), (.!=), fromJSON, withObject, eitherDecodeStrict)
import Data.Aeson.Types (Parser, Result(..))
import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShowM, traceShow)
import Data.List (nub, (\\))  
import Data.Foldable (foldl')
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B

import ADT_Config
import ADT_Roster 



main :: IO ()
main = do
    jsonConfig <- B.readFile "testFiles/prototype_config/config.json"
    let parsedConfig = eitherDecodeStrict jsonConfig :: Either String Configuration

    jsonInvalidRoster <- B.readFile "testFiles/prototype_config/invalid_roster.json"
    let parsedInvalidRoster = eitherDecodeStrict jsonInvalidRoster :: Either String LgManager

    jsonValidRoster <- B.readFile "testFiles/prototype_config/valid_roster.json"
    let parsedValidRoster = eitherDecodeStrict jsonValidRoster :: Either String LgManager

    jsonInvalidLineup <- B.readFile "testFiles/prototype_config/invalid_lineup.json"
    let parsedInvalidLineup = eitherDecodeStrict jsonInvalidLineup :: Either String LgManager

    case parsedConfig of
        Left errConfig -> putStrLn $ "Failed to parse Config JSON: " ++ errConfig
        Right config -> do
            putStrLn "Parsed Configuration:"
            print config
            putStrLn "\nTesting with Valid Roster:"
            testRoster config parsedValidRoster

            putStrLn "\nTesting with Invalid Roster:"
            testRoster config parsedInvalidRoster

            putStrLn "\nTesting with Invalid Lineup:"
            testRoster config parsedInvalidLineup

testRoster :: Configuration -> Either String LgManager -> IO ()
testRoster _ (Left errRoster) = putStrLn $ "Failed to parse Roster JSON: " ++ errRoster
testRoster config (Right lgManager) = do
    print $ current_lineup lgManager 
    print lgManager
    isValid <- validateAndPrint lgManager config
    if isValid 
        then putStrLn "This Lineup is valid as fuck, yo!"
        else putStrLn "That lineup has some serious discrepancies, bro!"

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

printDiscrepancies :: CurrentLineup -> LgRoster -> IO ()
printDiscrepancies lineup rosterConfig = do
    let discrepancies = getDiscrepancies lineup rosterConfig
    mapM_ printDifference discrepancies
  where
    printDifference (pos, diff)
      | diff > 0 = putStrLn $ "This roster has " ++ show diff ++ " too many players at " ++ pos ++ "."
      | diff < 0 = putStrLn $ "This roster needs " ++ show (abs diff) ++ " more players at " ++ pos ++ "."

validateAndPrint :: LgManager -> Configuration -> IO Bool
validateAndPrint manager config = do
    let validPositions = null $ getDiscrepancies (current_lineup manager) (valid_roster . point_parameters $ config)
    case hasUniquePlayers (current_lineup manager) of
        Left successMessage -> do
            putStrLn successMessage
            return validPositions
        Right duplicates -> do
            putStrLn "Duplicate player IDs found:"
            mapM_ (putStrLn . Text.unpack) duplicates
            return False

validateCurrentLineup :: LgManager -> Configuration -> Bool
validateCurrentLineup LgManager{..} Configuration{point_parameters = PointParameters{valid_roster = rosterConfig}} =
    let positionalValid = null (getDiscrepancies current_lineup rosterConfig)
    in case hasUniquePlayers current_lineup of
        Left _ -> positionalValid
        Right _ -> False

hasUniquePlayers :: CurrentLineup -> Either String [Text]
hasUniquePlayers lineup =
    let allPlayers = getUniquePlayerIds lineup
        duplicates = allPlayers \\ nub allPlayers
    in if null duplicates
       then Left "No duplicate players found."
       else Right duplicates

getUniquePlayerIds :: CurrentLineup -> [Text]
getUniquePlayerIds CurrentLineup{..} =
    cC : b1C : b2C : b3C : ssC : uC : (ofC ++ spC ++ rpC)
