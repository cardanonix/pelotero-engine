{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Text (Text)
import Data.ByteString ( ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Aeson (decode, Result(Success), ToJSON (..), FromJSON(..), Value, (.:), (.:?), (.!=), fromJSON, withObject, eitherDecodeStrict)
import Data.Aeson.Types (Parser, Result(..))
import Control.Monad (filterM, forM)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShowM, traceShow)
import Data.List (nub, (\\))
import Data.Foldable (foldl', forM_)
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B

import qualified Config as C
import qualified Roster as R
import qualified Input as I
import qualified DayStats as D
import OfficialRoster as O
import qualified OfficialRoster as O
import qualified GHC.Generics as R


type FileName = String
type FileContent = Either String R.LgManager

main :: IO ()
main = do
    parsedConfig <- readJson "testFiles/prototype_config/config.json" :: IO (Either String C.Configuration)
    let fileNames = [
            "testFiles/prototype_config/valid_roster.json",
            "testFiles/prototype_config/invalid_roster.json",
            "testFiles/prototype_config/invalid_lineup.json",
            "testFiles/appData/rosters/team_001.json",
            "testFiles/appData/rosters/team_002.json",
            "testFiles/appData/rosters/team_003.json", -- <-- too many outfielders
            "testFiles/appData/rosters/team_004.json" -- <-- too many outfielders
            ]
    filesContent <- mapM (\path -> readJson path :: IO FileContent) fileNames

    case parsedConfig of
        Left err -> putStrLn $ "Failed to parse Config JSON: " ++ err
        Right config -> processConfigResults config (zip fileNames filesContent)



data StatType = Batting | Pitching deriving (Show, Eq)

processConfigResults :: C.Configuration -> [(FileName, FileContent)] -> IO ()
processConfigResults config files =
    forM_ files $ \(fname, content) -> do
        putStrLn $ "\nTesting with " ++ extractNameFromPath fname ++ ":"
        testRoster config content

extractNameFromPath :: FileName -> String
extractNameFromPath = reverse . takeWhile (/= '/') . reverse


testRoster :: C.Configuration -> Either String R.LgManager -> IO ()
testRoster _ (Left errRoster) = putStrLn $ "Failed to parse Roster JSON: " ++ errRoster
testRoster config (Right lgManager) = do
    print $ R.current_lineup lgManager
    print lgManager
    isValid <- validateAndPrint lgManager config
    if isValid
        then putStrLn "This Lineup is valid."
        else putStrLn "That lineup has discrepancies."

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

validateAndPrint :: R.LgManager -> C.Configuration -> IO Bool
validateAndPrint manager config = do
    let rosterConfig = C.valid_roster . C.point_parameters $ config
    let discrepancies = getDiscrepancies (R.current_lineup manager) rosterConfig
    let validPositions = null discrepancies
    let validRosterSize = all (\(_, diff) -> diff <= 0) discrepancies

    playerIdValidation <- validatePlayerId (R.current_lineup manager)

    case (playerIdValidation, hasUniquePlayers (R.current_lineup manager)) of
        (Left successMessage, Left _) -> do
            putStrLn successMessage
            mapM_ (\(pos, diff) -> putStrLn $ "This roster has " ++ show diff ++ " too many players at " ++ pos ++ ".") discrepancies
            return $ validPositions && validRosterSize
        (Right nonexistent, _) -> do
            putStrLn "Invalid player IDs found:"
            mapM_ (putStrLn . Text.unpack) nonexistent
            return False
        (_, Right duplicates) -> do
            putStrLn "Duplicate player IDs found:"
            mapM_ (putStrLn . Text.unpack) duplicates
            return False

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

hasUniquePlayers :: R.CurrentLineup -> Either String [Text]
hasUniquePlayers lineup =
    let allPlayers = getUniquePlayerIds lineup
        duplicates = allPlayers \\ nub allPlayers
    in if null duplicates
       then Left "No duplicate players found."
       else Right duplicates

-- Lookup a playerId in an OfficialRoster
lookupPlayerInRoster :: Text -> O.OfficialRoster -> Bool
lookupPlayerInRoster pid roster = 
    any (\player -> Text.pack (show $ playerId player) == pid) (people roster)

lookupPlayerId :: Text -> IO Bool
lookupPlayerId playerId = do
    parsedRoster <- readJson "appData/rosters/activePlayers.json" :: IO (Either String O.OfficialRoster)
    case parsedRoster of
        Left _ -> return False
        Right activeRoster -> return $ lookupPlayerInRoster playerId activeRoster

validatePlayerId :: R.CurrentLineup -> IO (Either String [Text])
validatePlayerId lineup = do
    let allPlayers = getUniquePlayerIds lineup
    nonexistent <- filterM (fmap not . lookupPlayerId) allPlayers
    if null nonexistent
       then return $ Left "All Players are valid."
       else return $ Right nonexistent

-- Utility function to convert an Int ID to Text
intToText :: Int -> Text
intToText = Text.pack . show

getUniquePlayerIds :: R.CurrentLineup -> [Text]
getUniquePlayerIds R.CurrentLineup{..} =
    cC : b1C : b2C : b3C : ssC : uC : (ofC ++ spC ++ rpC)

getDiscrepancies :: R.CurrentLineup -> C.LgRoster -> [(String, Int)]
getDiscrepancies R.CurrentLineup{..} C.LgRoster{..} =
    let discrepancies =
          [ validatePositionCount "Catcher" [cC] lg_catcher
          , validatePositionCount "First Base" [b1C] lg_first
          , validatePositionCount "Second Base" [b2C] lg_second
          , validatePositionCount "Third Base" [b3C] lg_third
          , validatePositionCount "Shortstop" [ssC] lg_shortstop
          , validatePositionCount "Outfield" ofC lg_outfield
          , validatePositionCount "Utility" [uC] lg_utility
          , validatePositionCount "Starting Pitcher" spC lg_s_pitcher
          , validatePositionCount "Relief Pitcher" rpC lg_r_pitcher
          ]
        totalSizeDiscrepancy = totalPlayersInLineup R.CurrentLineup{..} - lg_max_size
        rosterSizeDiscrepancy = ([("Team Total", totalSizeDiscrepancy) | totalSizeDiscrepancy > 0])
    in catMaybes discrepancies ++ rosterSizeDiscrepancy

validatePositionCount :: String -> [a] -> Int -> Maybe (String, Int)
validatePositionCount positionName players maxAllowed
    | overage > 0 = Just (positionName, overage)
    | otherwise = Nothing
    where overage = length players - maxAllowed

totalPlayersInLineup :: R.CurrentLineup -> Int
totalPlayersInLineup R.CurrentLineup{..} =
    1 + 1 + 1 + 1 + 1 + 1 + length ofC + length spC + length rpC  -- counting players from all positions

validateCurrentLineup :: R.LgManager -> C.Configuration -> Bool
validateCurrentLineup R.LgManager{..} C.Configuration{point_parameters = C.PointParameters{valid_roster = rosterConfig}} =
    let positionalValid = null (getDiscrepancies current_lineup rosterConfig)
    in case hasUniquePlayers current_lineup of
        Left _ -> positionalValid
        Right _ -> False

-- takes a playerId as a String and a LgManager and returns the player's position or fails with an error message
-- addded failure cases to protect against players being in multiple positions or not being found in the lineup
-- even though this should be impossible if I make the lineup-setting tools correctly
findPlayerPosition :: Text -> R.LgManager -> Either Text Text
findPlayerPosition playerId mgr =
    case matchingPositions of
        []     -> Left "Player not found in current lineup."
        [pos]  -> Right pos
        _      -> Left "Player found in multiple positions in the lineup."
    where
        lineup = R.current_lineup mgr
        checks = [ ((playerId ==), "C", R.cC)
                 , ((playerId ==), "1B", R.b1C)
                 , ((playerId ==), "2B", R.b2C)
                 , ((playerId ==), "3B", R.b3C)
                 , ((playerId ==), "SS", R.ssC)
                 , ((playerId ==), "U", R.uC)
                 , (\x -> x `elem` R.ofC lineup, "OF", const playerId)
                 , (\x -> x `elem` R.spC lineup, "SP", const playerId)
                 , (\x -> x `elem` R.rpC lineup, "RP", const playerId)
                 ]

        matchingPositions = [ pos | (check, pos, access) <- checks, check (access lineup) ]

-- takes a playerId as a String and a LgManager 
-- and returns whether the player is to be fielded as a batter or pitcher for points calculation purposes
batterOrPitcher :: Text -> R.LgManager -> Either Text D.StatType
batterOrPitcher playerName mgr
    | isBatter   = Right D.Batting
    | isPitcher  = Right D.Pitching
    | otherwise  = Left "Player not found in current lineup."
    where
        lineup = R.current_lineup mgr
        batterPositions = [R.cC lineup, R.b1C lineup, R.b2C lineup, R.b3C lineup, R.ssC lineup, R.uC lineup] ++ R.ofC lineup
        pitcherPositions = R.spC lineup ++ R.rpC lineup

        isBatter = playerName `elem` batterPositions
        isPitcher = playerName `elem` pitcherPositions
