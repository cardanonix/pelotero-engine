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

import qualified ADT_Config as C
import qualified ADT_Roster as R

main :: IO ()
main = do
    parsedConfig <- readJson "testFiles/prototype_config/config.json" :: IO (Either String C.Configuration)
    parsedInvalidRoster <- readJson "testFiles/prototype_config/invalid_roster.json" :: IO (Either String R.LgManager)
    parsedValidRoster <- readJson "testFiles/prototype_config/valid_roster.json" :: IO (Either String R.LgManager)
    parsedInvalidLineup <- readJson "testFiles/prototype_config/invalid_lineup.json" :: IO (Either String R.LgManager)
    teamOneRoster <- readJson "appData/rosters/team_001.json" :: IO (Either String R.LgManager)
    teamTwoRoster <- readJson "appData/rosters/team_002.json" :: IO (Either String R.LgManager)

    processConfigResults parsedConfig parsedValidRoster parsedInvalidRoster parsedInvalidLineup teamOneRoster teamTwoRoster

processConfigResults :: Either String C.Configuration -> Either String R.LgManager -> Either String R.LgManager -> Either String R.LgManager -> Either String R.LgManager -> Either String R.LgManager -> IO ()
processConfigResults (Left errConfig) _ _ _ _ _ = putStrLn $ "Failed to parse Config JSON: " ++ errConfig
processConfigResults (Right config) parsedValidRoster parsedInvalidRoster parsedInvalidLineup teamOneRoster teamTwoRoster = do
    putStrLn "Parsed Configuration:"
    print config
    putStrLn "\nTesting with Valid Roster:"
    testRoster config parsedValidRoster

    putStrLn "\nTesting with Invalid Roster:"
    testRoster config parsedInvalidRoster

    putStrLn "\nTesting with Invalid Lineup:"
    testRoster config parsedInvalidLineup

    putStrLn "\nTesting with Invalid Roster:"
    testRoster config teamOneRoster

    putStrLn "\nTesting with Invalid Roster:"
    testRoster config teamTwoRoster

readJson :: FromJSON a => FilePath -> IO (Either String a)
readJson filePath = eitherDecodeStrict <$> B.readFile filePath

-- This function validates a roster and returns True or False.
isRosterValid :: R.LgManager -> C.Configuration -> Bool
isRosterValid manager config =
    case validateRoster manager config of
        Left _ -> False
        Right _ -> True

-- This function validates a roster and returns the error messages if there are any.
getRosterValidationErrors :: R.LgManager -> C.Configuration -> [String]
getRosterValidationErrors manager config =
    case validateRoster manager config of
        Left errors -> errors
        Right _ -> []

validateRoster :: R.LgManager -> C.Configuration -> Either [String] ()
validateRoster manager config = do
    let discrepancies = getDiscrepancies (R.current_lineup manager) (C.valid_roster . C.point_parameters $ config)
    let duplicateCheck = hasUniquePlayers (R.current_lineup manager)
    case (duplicateCheck, discrepancies) of
        (Left _, []) -> Right ()
        (Right duplicates, []) -> Left (map Text.unpack duplicates ++ ["Duplicate player IDs found."])
        (_, errors) -> Left (map discrepancyToString errors)
    where
        discrepancyToString (pos, diff) 
          | diff > 0 = "This roster has " ++ show diff ++ " too many players at " ++ pos ++ "."
          | diff < 0 = "This roster needs " ++ show (abs diff) ++ " more players at " ++ pos ++ "."


testRoster :: C.Configuration -> Either String R.LgManager -> IO ()
testRoster _ (Left errRoster) = putStrLn $ "Failed to parse Roster JSON: " ++ errRoster
testRoster config (Right lgManager) = do
    print $ R.current_lineup lgManager 
    print lgManager
    isValid <- validateAndPrint lgManager config
    if isValid 
        then putStrLn "This Lineup is valid as fuck, yo!"
        else putStrLn "That lineup has some serious discrepancies, bro!"

validateAndPrint :: R.LgManager -> C.Configuration -> IO Bool
validateAndPrint manager config = do
    let rosterConfig = C.valid_roster . C.point_parameters $ config
    let discrepancies = getDiscrepancies (R.current_lineup manager) rosterConfig
    let validPositions = null discrepancies
    let validRosterSize = all (\(_, diff) -> diff <= 0) discrepancies
    case hasUniquePlayers (R.current_lineup manager) of
        Left successMessage -> do
            putStrLn successMessage
            mapM_ (\(pos, diff) -> putStrLn $ "This roster has " ++ show diff ++ " too many players at " ++ pos ++ ".") discrepancies
            return $ validPositions && validRosterSize
        Right duplicates -> do
            putStrLn "Duplicate player IDs found:"
            mapM_ (putStrLn . Text.unpack) duplicates
            return False

hasUniquePlayers :: R.CurrentLineup -> Either String [Text]
hasUniquePlayers lineup =
    let allPlayers = getUniquePlayerIds lineup
        duplicates = allPlayers \\ nub allPlayers
    in if null duplicates
       then Left "No duplicate players found."
       else Right duplicates

getUniquePlayerIds :: R.CurrentLineup -> [Text]
getUniquePlayerIds R.CurrentLineup{..} =
    cC : b1C : b2C : b3C : ssC : uC : (ofC ++ spC ++ rpC)

getDiscrepancies :: R.CurrentLineup -> C.LgRoster -> [(String, Int)]
getDiscrepancies R.CurrentLineup{..} C.LgRoster{..} =
    let discrepancies =
          [ validatePosition "Catcher" [cC] lg_catcher
          , validatePosition "First Base" [b1C] lg_first
          , validatePosition "Second Base" [b2C] lg_second
          , validatePosition "Third Base" [b3C] lg_third
          , validatePosition "Shortstop" [ssC] lg_shortstop
          , validatePosition "Outfield" ofC lg_outfield
          , validatePosition "Utility" [uC] lg_utility
          , validatePosition "Starting Pitcher" spC lg_s_pitcher
          , validatePosition "Relief Pitcher" rpC lg_r_pitcher
          ]
        totalSizeDiscrepancy = totalPlayersInLineup R.CurrentLineup{..} - lg_max_size
        rosterSizeDiscrepancy = ([("Roster Size", totalSizeDiscrepancy) | totalSizeDiscrepancy > 0])
    in catMaybes discrepancies ++ rosterSizeDiscrepancy

validatePosition :: String -> [a] -> Int -> Maybe (String, Int)
validatePosition positionName players maxAllowed
    | overage > 0 = Just (positionName, overage)
    | otherwise = Nothing
    where overage = length players - maxAllowed


totalPlayersInLineup :: R.CurrentLineup -> Int
totalPlayersInLineup R.CurrentLineup{..} =
    1 + 1 + 1 + 1 + 1 + 1 + length ofC + length spC + length rpC  -- counting players from all positions

printDiscrepancies :: R.CurrentLineup -> C.LgRoster -> IO ()
printDiscrepancies lineup rosterConfig = do
    let discrepancies = getDiscrepancies lineup rosterConfig
    mapM_ printDifference discrepancies
  where
    printDifference (pos, diff)
      | diff > 0 = putStrLn $ "This roster has " ++ show diff ++ " too many players at " ++ pos ++ "."
      | diff < 0 = putStrLn $ "This roster needs " ++ show (abs diff) ++ " more players at " ++ pos ++ "."

validateCurrentLineup :: R.LgManager -> C.Configuration -> Bool
validateCurrentLineup R.LgManager{..} C.Configuration{point_parameters = C.PointParameters{valid_roster = rosterConfig}} =
    let positionalValid = null (getDiscrepancies current_lineup rosterConfig)
    in case hasUniquePlayers current_lineup of
        Left _ -> positionalValid
        Right _ -> False