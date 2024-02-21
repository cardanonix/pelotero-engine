{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Validators where

import Control.Monad (filterM, forM)
import Data.Aeson (FromJSON (..), Result (Success), ToJSON (..), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (foldl', forM_)
import Data.List (nub, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShow, traceShowM)

import qualified Config as C
import qualified GHC.Generics as R
import qualified Input as I
import OfficialRoster as O
import qualified OfficialRoster as O
import qualified Points as P
import qualified Roster as R

type FileName = String
type FileContent = Either String R.LgManager

-- data StatType = Batting | Pitching deriving (Show, Eq)

readJson :: (FromJSON a) => FilePath -> IO (Either String a)
readJson filePath = eitherDecodeStrict <$> B.readFile filePath

extractNameFromPath :: FileName -> String
extractNameFromPath = reverse . takeWhile (/= '/') . reverse

processConfigResults :: C.Configuration -> [(FileName, FileContent)] -> IO ()
processConfigResults config files =
    forM_ files $ \(fname, content) -> do
        putStrLn $ "\nTesting with " ++ extractNameFromPath fname ++ ":"
        testLineup config content

testLineup :: C.Configuration -> Either String R.LgManager -> IO ()
testLineup _ (Left errRoster) = putStrLn $ "Failed to parse Roster JSON: " ++ errRoster
testLineup config (Right lgManager) = do
    print $ R.current_lineup lgManager
    print lgManager
    isValid <- validateAndPrintLineup lgManager config
    if isValid
        then putStrLn "This Lineup is valid."
        else putStrLn "That lineup has discrepancies."

validateLineup :: R.LgManager -> C.Configuration -> Either [String] ()
validateLineup manager config = do
    let discrepancies = getLineupDiscrepancies (R.current_lineup manager) (C.valid_roster . C.point_parameters $ config)
    let duplicateCheck = lineupHasUniquePlayers (R.current_lineup manager)
    case (duplicateCheck, discrepancies) of
        (Left _, []) -> Right ()
        (Right duplicates, []) -> Left (map Text.unpack duplicates ++ ["Duplicate player IDs found."])
        (_, errors) -> Left (map discrepancyToString errors)
  where
    discrepancyToString (pos, diff)
        | diff > 0 = "This roster has " ++ show diff ++ " too many players at " ++ pos ++ "."
        | diff < 0 = "This roster needs " ++ show (abs diff) ++ " more players at " ++ pos ++ "."

validateAndPrintLineup :: R.LgManager -> C.Configuration -> IO Bool
validateAndPrintLineup manager config = do
    let rosterConfig = C.valid_roster . C.point_parameters $ config
    let discrepancies = getLineupDiscrepancies (R.current_lineup manager) rosterConfig
    let validPositions = null discrepancies
    let validRosterSize = all (\(_, diff) -> diff <= 0) discrepancies

    playerIdValidation <- validatePlayerId (R.current_lineup manager)

    case (playerIdValidation, lineupHasUniquePlayers (R.current_lineup manager)) of
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

-- This function validates a lineup and returns True or False.
isLineupValid :: R.LgManager -> C.Configuration -> Bool
isLineupValid manager config =
    case validateLineup manager config of
        Left _ -> False
        Right _ -> True

-- This function validates a roster and returns the error messages if there are any.
getRosterValidationErrors :: R.LgManager -> C.Configuration -> [String]
getRosterValidationErrors manager config =
    case validateLineup manager config of
        Left errors -> errors
        Right _ -> []

lineupHasUniquePlayers :: R.CurrentLineup -> Either String [Text]
lineupHasUniquePlayers lineup =
    let allPlayers = getUniquePlayerIdsLineup lineup
        duplicates = allPlayers \\ nub allPlayers
     in if null duplicates
            then Left "No duplicate players found."
            else Right duplicates

-- Roster Validation
hasUniqueRosterPlayers :: R.Roster -> Either String [Text]
hasUniqueRosterPlayers roster =
    let allPlayers = concat [R.cR roster, R.b1R roster, R.b2R roster, R.b3R roster, R.ssR roster, R.ofR roster, R.uR roster, R.spR roster, R.rpR roster]
        duplicates = allPlayers \\ nub allPlayers
    in if null duplicates then Left "No duplicate players found in roster." else Right duplicates

lookupPlayerInRoster :: Text -> R.Roster -> Bool
lookupPlayerInRoster playerId roster =
    let allPlayers = concat [R.cR roster, R.b1R roster, R.b2R roster, R.b3R roster, R.ssR roster, R.ofR roster, R.uR roster, R.spR roster, R.rpR roster]
    in playerId `elem` allPlayers


getRosterDiscrepancies :: R.Roster -> C.DraftRoster -> [String]
getRosterDiscrepancies roster limits =
    let discrepancies = [
          validatePosition "catcher" (R.cR roster) (C.dr_catcher limits),
          validatePosition "first" (R.b1R roster) (C.dr_first limits),
          validatePosition "second" (R.b1R roster) (C.dr_second limits),
          validatePosition "third" (R.b1R roster) (C.dr_third limits),
          validatePosition "shortstop" (R.b1R roster) (C.dr_shortstop limits),
          validatePosition "outfield" (R.b1R roster) (C.dr_outfield limits),      
          validatePosition "utility" (R.b1R roster) (C.dr_utility limits), 
          validatePosition "s_pitcher" (R.b1R roster) (C.dr_s_pitcher limits), 
          validatePosition "r_pitcher" (R.b1R roster) (C.dr_r_pitcher limits)     
        ]
    in catMaybes discrepancies
  where
    validatePosition posName players limit =
        let diff = length players - limit
        in if diff > 0 then Just $ "Too many players at " ++ posName ++ ": " ++ show diff else Nothing

validateRoster :: R.Roster -> C.Configuration -> Either [String] ()
validateRoster roster config = do
    let discrepancies = getRosterDiscrepancies roster (C.draft_limits $ C.draft_parameters config)
    let duplicateCheck = hasUniqueRosterPlayers roster
    case (duplicateCheck, discrepancies) of
        (Left _, []) -> Right ()
        (Right duplicates, []) -> Left (map Text.unpack duplicates ++ ["Duplicate player IDs found in roster."])
        (_, errors) -> Left errors

-- also used in Leaderboard to verify that the player is valid except with that different player type
hasValidPositions :: Value -> Bool
hasValidPositions val = case fromJSON val :: Result I.Player of
    Success player -> case I.allPositions player of
        Just positions -> not (null positions)
        Nothing -> False
    _ -> False

-- Lookup a playerId in an OfficialRoster
lookupPlayerInOfficialRoster :: Text -> O.OfficialRoster -> Bool
lookupPlayerInOfficialRoster pid roster =
    any (\player -> Text.pack (show $ playerId player) == pid) (people roster)

lookupPlayerId :: Text -> IO Bool
lookupPlayerId playerId = do
    parsedRoster <- readJson "appData/rosters/activePlayers.json" :: IO (Either String O.OfficialRoster)
    case parsedRoster of
        Left _ -> return False
        Right activeRoster -> return $ lookupPlayerInOfficialRoster playerId activeRoster

validatePlayerId :: R.CurrentLineup -> IO (Either String [Text])
validatePlayerId lineup = do
    let allPlayers = getUniquePlayerIdsLineup lineup
    nonexistent <- filterM (fmap not . lookupPlayerId) allPlayers
    if null nonexistent
        then return $ Left "All Players are valid."
        else return $ Right nonexistent

-- Utility function to convert an Int ID to Text
intToText :: Int -> Text
intToText = Text.pack . show

getUniquePlayerIdsLineup :: R.CurrentLineup -> [Text]
getUniquePlayerIdsLineup R.CurrentLineup{..} =
    cC : b1C : b2C : b3C : ssC : uC : (ofC ++ spC ++ rpC)

getLineupDiscrepancies :: R.CurrentLineup -> C.LgRoster -> [(String, Int)]
getLineupDiscrepancies R.CurrentLineup{..} C.LgRoster{..} =
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
  where
    overage = length players - maxAllowed

totalPlayersInLineup :: R.CurrentLineup -> Int
totalPlayersInLineup R.CurrentLineup{..} =
    1 + 1 + 1 + 1 + 1 + 1 + length ofC + length spC + length rpC -- counting players from all positions

validateCurrentLineup :: R.LgManager -> C.Configuration -> Bool
validateCurrentLineup R.LgManager{..} C.Configuration{point_parameters = C.PointParameters{valid_roster = rosterConfig}} =
    let positionalValid = null (getLineupDiscrepancies current_lineup rosterConfig)
     in case lineupHasUniquePlayers current_lineup of
            Left _ -> positionalValid
            Right _ -> False

-- takes a playerId as a String and a LgManager and returns the player's position or fails with an error message
-- addded failure cases to protect against players being in multiple positions or not being found in the lineup
-- even though this should be impossible if I make the lineup-setting tools correctly
findPlayerPosition :: Text -> R.LgManager -> Either Text Text
findPlayerPosition playerId mgr =
    case matchingPositions of
        [] -> Left "Player not found in current lineup."
        [pos] -> Right pos
        _ -> Left "Player found in multiple positions in the lineup."
  where
    lineup = R.current_lineup mgr
    checks =
        [ ((playerId ==), "C", R.cC)
        , ((playerId ==), "1B", R.b1C)
        , ((playerId ==), "2B", R.b2C)
        , ((playerId ==), "3B", R.b3C)
        , ((playerId ==), "SS", R.ssC)
        , ((playerId ==), "U", R.uC)
        , (\x -> x `elem` R.ofC lineup, "OF", const playerId)
        , (\x -> x `elem` R.spC lineup, "SP", const playerId)
        , (\x -> x `elem` R.rpC lineup, "RP", const playerId)
        ]

    matchingPositions = [pos | (check, pos, access) <- checks, check (access lineup)]

-- takes a playerId as a String and a LgManager
-- and returns whether the player is to be fielded as a batter or pitcher for points calculation purposes
batterOrPitcher :: Text -> R.LgManager -> Either Text P.StatType
batterOrPitcher playerName mgr
    | isBatter = Right P.Batting
    | isPitcher = Right P.Pitching
    | otherwise = Left "Player not found in current lineup."
  where
    lineup = R.current_lineup mgr
    batterPositions = [R.cC lineup, R.b1C lineup, R.b2C lineup, R.b3C lineup, R.ssC lineup, R.uC lineup] ++ R.ofC lineup
    pitcherPositions = R.spC lineup ++ R.rpC lineup

    isBatter = playerName `elem` batterPositions
    isPitcher = playerName `elem` pitcherPositions
