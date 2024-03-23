{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use :" #-}

module Validators where

import Control.Monad (filterM, forM)
import Data.Aeson (FromJSON (..), Result (Success), ToJSON (..), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import Data.List ( find, delete, nub, (\\) )
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (foldl', forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)

import qualified Data.Text as T
import Debug.Trace (traceShow, traceShowM)

import qualified Config as C
import qualified GHC.Generics as R
import qualified Input as I
import OfficialRoster as O
import qualified OfficialRoster as O
import qualified Points as P
import qualified Roster as R
import Utility

type FileName = String
type FileContent = Either String R.LgManager

-- data StatType = Batting | Pitching deriving (Show, Eq)

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

findPlayer :: O.PlayerID -> [O.OfficialPlayer] -> [O.PlayerID] -> Maybe O.OfficialPlayer
findPlayer playerId players availableIds =
    find (\p -> O.playerId p == playerId && playerId `elem` availableIds) players

maxPossibleTeams :: C.Configuration -> OfficialRoster -> Int
maxPossibleTeams config roster =
  let
    positions = ["P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH"]
    -- Calculate the max possible teams for each position and take the minimum
    maxTeamsForAllPositions = map (\pos -> maxPossibleTeamsForPosition pos config roster) positions
  in
    minimum maxTeamsForAllPositions

maxPossibleTeamsForPosition :: T.Text -> C.Configuration -> OfficialRoster -> Int
maxPossibleTeamsForPosition position config roster =
  let
    -- Convert the position code to the draft limit field name
    draftLimit = case positionCodeToText position of
      "pitcher" -> C.dr_s_pitcher (C.draft_limits (C.draft_parameters config)) + C.dr_r_pitcher (C.draft_limits (C.draft_parameters config))
      "catcher" -> C.dr_catcher (C.draft_limits (C.draft_parameters config))
      "first" -> C.dr_first (C.draft_limits (C.draft_parameters config))
      "second" -> C.dr_second (C.draft_limits (C.draft_parameters config))
      "third" -> C.dr_third (C.draft_limits (C.draft_parameters config))
      "shortstop" -> C.dr_shortstop (C.draft_limits (C.draft_parameters config))
      "outfield" -> C.dr_outfield (C.draft_limits (C.draft_parameters config)) * 3 -- Assuming LF, CF, RF are interchangeable
      "utility" -> C.dr_utility (C.draft_limits (C.draft_parameters config))
      _ -> 0

    -- Count the number of players available for the position
    playerCount = length $ filter (\p -> primaryPosition p == position) (people roster)
  in
    -- Calculate the maximum number of teams based on the draft limit and available players
    if draftLimit > 0 then playerCount `div` draftLimit else 0

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

-- This function validates a roster and returns the error messages if there are any.
getLineupValidationErrors :: R.LgManager -> C.Configuration -> [String]
getLineupValidationErrors manager config =
    case validateLineup manager config of
        Left errors -> errors
        Right _ -> []

hasUniqueLineupPlayers :: R.CurrentLineup -> Either String [T.Text]
hasUniqueLineupPlayers lineup =
    let allPlayers = getUniquePlayerIdsLineup lineup
        duplicates = allPlayers \\ nub allPlayers
     in if null duplicates
            then Left "No duplicate players found."
            else Right duplicates

-- Roster Validation
hasUniqueRosterPlayers :: R.Roster -> Either String [T.Text]
hasUniqueRosterPlayers roster =
    let allPlayers = concat [R.cR roster, R.b1R roster, R.b2R roster, R.b3R roster, R.ssR roster, R.ofR roster, R.uR roster, R.spR roster, R.rpR roster]
        duplicates = allPlayers \\ nub allPlayers
    in if null duplicates then Left "No duplicate players found in roster." else Right duplicates

lookupPlayerInRoster :: T.Text -> R.Roster -> Bool
lookupPlayerInRoster playerId roster =
    let allPlayers = concat [R.cR roster, R.b1R roster, R.b2R roster, R.b3R roster, R.ssR roster, R.ofR roster, R.uR roster, R.spR roster, R.rpR roster]
    in playerId `elem` allPlayers

lookupPlayerInLineup :: T.Text -> R.CurrentLineup -> Bool
lookupPlayerInLineup playerId lineup =
    let allPlayers = concat [R.cC lineup, R.b1C lineup, R.b2C lineup, R.b3C lineup, R.ssC lineup, R.ofC lineup, R.uC lineup, R.spC lineup, R.rpC lineup]
    in playerId `elem` allPlayers

getRosterDiscrepancies :: R.Roster -> C.DraftRosterLmts -> [String]
getRosterDiscrepancies roster limits =
    mapMaybe validatePosition [ ("catcher", R.cR roster, C.dr_catcher limits)
    , ("first", R.b1R roster, C.dr_first limits)
    , ("second", R.b2R roster, C.dr_second limits)
    , ("third", R.b3R roster, C.dr_third limits)
    , ("shortstop", R.ssR roster, C.dr_shortstop limits)
    , ("outfield", R.ofR roster, C.dr_outfield limits)
    , ("utility", R.uR roster, C.dr_utility limits)
    , ("s_pitcher", R.spR roster, C.dr_s_pitcher limits)
    , ("r_pitcher", R.rpR roster, C.dr_r_pitcher limits)
    ]
  where
    validatePosition (posName, players, limit) =
        let diff = length players - limit
        in if diff > 0 then Just $ posName ++ ": Too many players in Roster - " ++ show diff else Nothing

getLineupDiscrepancies :: R.CurrentLineup -> C.LgLineupLmts -> [(String, Int)]
getLineupDiscrepancies lineup limits =
    mapMaybe validatePosition [ 
      ("catcher", R.cC lineup, C.lg_catcher limits)
    , ("first", R.b1C lineup, C.lg_first limits)
    , ("second", R.b2C lineup, C.lg_second limits)
    , ("third", R.b3C lineup, C.lg_third limits)
    , ("shortstop", R.ssC lineup, C.lg_shortstop limits)
    , ("outfield", R.ofC lineup, C.lg_outfield limits)
    , ("utility", R.uC lineup, C.lg_utility limits)
    , ("s_pitcher", R.spC lineup, C.lg_s_pitcher limits)
    , ("r_pitcher", R.rpC lineup, C.lg_r_pitcher limits)
    ]
  where
    validatePosition (posName, players, limit) =
        let diff = length players - limit
        in if diff > 0 then Just (posName, diff) else Nothing

-- getLineupDiscrepancies :: R.CurrentLineup -> C.LgLineupLmts -> [String]
-- getLineupDiscrepancies lineup limits =
--     mapMaybe validatePosition [ 
--       ("catcher", R.cC lineup, C.lg_catcher limits)
--     , ("first", R.b1C lineup, C.lg_first limits)
--     , ("second", R.b2C lineup, C.lg_second limits)
--     , ("third", R.b3C lineup, C.lg_third limits)
--     , ("shortstop", R.ssC lineup, C.lg_shortstop limits)
--     , ("outfield", R.ofC lineup, C.lg_outfield limits)
--     , ("utility", R.uC lineup, C.lg_utility limits)
--     , ("s_pitcher", R.spC lineup, C.lg_s_pitcher limits)
--     , ("r_pitcher", R.rpC lineup, C.lg_r_pitcher limits)
--     ]
--   where
--     validatePosition (posName, players, limit) =
--         let diff = length players - limit
--         in if diff > 0 then Just $ posName ++ ": Too many players in Lineup - " ++ show diff else Nothing

countPlayersOnRoster :: T.Text -> R.Roster -> Int
countPlayersOnRoster position roster =
    case position of
        "catcher" -> length $ R.cR roster
        "first" -> length $ R.b1R roster
        "second" -> length $ R.b2R roster
        "third" -> length $ R.b3R roster
        "shortstop" -> length $ R.ssR roster
        "outfield" -> length $ R.ofR roster
        "utility" -> length $ R.uR roster
        "s_pitcher" -> length $ R.spR roster
        "r_pitcher" -> length $ R.rpR roster
        _ -> 0

countPlayersInLineup :: T.Text -> R.CurrentLineup -> Int
countPlayersInLineup position lineup =
    case position of
        "catcher" -> length $ R.cC lineup
        "first" -> length $ R.b1C lineup
        "second" -> length $ R.b2C lineup
        "third" -> length $ R.b3C lineup
        "shortstop" -> length $ R.ssC lineup
        "outfield" -> length $ R.ofC lineup
        "utility" -> length $ R.uC lineup
        "s_pitcher" -> length $ R.spC lineup
        "r_pitcher" -> length $ R.rpC lineup
        _ -> 0

validateRoster :: R.Roster -> C.Configuration -> Either [String] ()
validateRoster roster config = do
    let discrepancies = getRosterDiscrepancies roster (C.draft_limits $ C.draft_parameters config)
    let duplicateCheck = hasUniqueRosterPlayers roster
    case (duplicateCheck, discrepancies) of
        (Left _, []) -> Right ()
        (Right duplicates, []) -> Left (map T.unpack duplicates ++ ["Duplicate player IDs found in roster."])
        (_, errors) -> Left errors

validateAndPrintLineup :: R.LgManager -> C.Configuration -> IO Bool
validateAndPrintLineup manager config = do
    let rosterConfig = C.lineup_limits . C.point_parameters $ config
    let discrepancies = getLineupDiscrepancies (R.current_lineup manager) rosterConfig
    let validPositions = null discrepancies
    let validLineupSize = all (\(_, diff) -> diff <= 0) discrepancies

    playerIdValidation <- validatePlayerId (R.current_lineup manager)

    case (playerIdValidation, hasUniqueLineupPlayers (R.current_lineup manager)) of
        (Left successMessage, Left _) -> do
            putStrLn successMessage
            mapM_ (\(pos, diff) -> putStrLn $ "This lineup has " ++ show diff ++ " too many players at " ++ pos ++ ".") discrepancies
            return $ validPositions && validLineupSize
        (Right nonexistent, _) -> do
            putStrLn "Invalid player IDs found:"
            mapM_ (putStrLn . T.unpack) nonexistent
            return False
        (_, Right duplicates) -> do
            putStrLn "Duplicate player IDs found:"
            mapM_ (putStrLn . T.unpack) duplicates
            return False


-- validateLineup :: R.LgManager -> C.Configuration -> Either [String] ()
-- validateLineup manager config = do
--     let discrepancies = getLineupDiscrepancies (R.current_lineup manager) (C.lineup_limits . C.point_parameters $ config)
--     let duplicateCheck = hasUniqueLineupPlayers (R.current_lineup manager)
--     case (duplicateCheck, discrepancies) of
--         (Left _, []) -> Right ()
--         (Right duplicates, []) -> Left (map T.unpack duplicates ++ ["Duplicate player IDs found."])
--         (_, errors) -> Left (map discrepancyToString errors)
--   where
--     discrepancyToString (pos, diff)
--         | diff > 0 = "This roster has " ++ show diff ++ " too many players at " ++ pos ++ "."
--         | diff < 0 = "This roster needs " ++ show (abs diff) ++ " more players at " ++ pos ++ "."


-- validateLineup :: R.CurrentLineup -> C.Configuration -> Either [String] ()
-- validateLineup lineup config = do
--     let discrepancies = getRosterDiscrepancies lineup (C.lineup_limits $ C.point_parameters config)
--     let duplicateCheck = hasUniqueRosterPlayers lineup
--     case (duplicateCheck, discrepancies) of
--         (Left _, []) -> Right ()
--         (Right duplicates, []) -> Left (map T.unpack duplicates ++ ["Duplicate player IDs found in roster."])
--         (_, errors) -> Left errors

validateLineup :: R.LgManager -> C.Configuration -> Either [String] ()
validateLineup manager config = do
    let discrepancies = getLineupDiscrepancies (R.current_lineup manager) (C.lineup_limits . C.point_parameters $ config)
    let duplicateCheck = hasUniqueLineupPlayers (R.current_lineup manager)
    case (duplicateCheck, discrepancies) of
        (Left _, []) -> Right ()
        (Right duplicates, []) -> Left (map T.unpack duplicates ++ ["Duplicate player IDs found in lineup."])
        (_, errors) -> Left errors

queryLimits :: T.Text -> T.Text -> C.Configuration -> Int
queryLimits limtype position config =
    case limtype of
        "draft" -> queryDraftRosterLmts position (C.draft_limits $ C.draft_parameters config)
        "lineup" -> queryLgLineupLmts position (C.lineup_limits $ C.point_parameters config)
        _ -> 0

queryDraftRosterLmts :: T.Text -> C.DraftRosterLmts -> Int
queryDraftRosterLmts position limits =
    case position of
        "catcher" -> C.dr_catcher limits
        "first" -> C.dr_first limits
        "second" -> C.dr_second limits
        "third" -> C.dr_third limits
        "shortstop" -> C.dr_shortstop limits
        "outfield" -> C.dr_outfield limits
        "utility" -> C.dr_utility limits
        "s_pitcher" -> C.dr_s_pitcher limits
        "r_pitcher" -> C.dr_r_pitcher limits
        _ -> 0

queryLgLineupLmts :: T.Text -> C.LgLineupLmts -> Int
queryLgLineupLmts position limits =
    case position of
        "catcher" -> C.lg_catcher limits
        "first" -> C.lg_first limits
        "second" -> C.lg_second limits
        "third" -> C.lg_third limits
        "shortstop" -> C.lg_shortstop limits
        "outfield" -> C.lg_outfield limits
        "utility" -> C.lg_utility limits
        "s_pitcher" -> C.lg_s_pitcher limits
        "r_pitcher" -> C.lg_r_pitcher limits
        _ -> 0     

-- also used in Leaderboard to verify that the player is valid except with that different player type
hasValidPositions :: Value -> Bool
hasValidPositions val = case fromJSON val :: Result I.Player of
    Success player -> case I.allPositions player of
        Just positions -> not (null positions)
        Nothing -> False
    _ -> False

-- Lookup a playerId in an OfficialRoster
lookupPlayerInOfficialRoster :: T.Text -> O.OfficialRoster -> Bool
lookupPlayerInOfficialRoster pid roster =
    any (\player -> T.pack (show $ playerId player) == pid) (people roster)

lookupPlayerId :: T.Text -> IO Bool
lookupPlayerId playerId = do
    parsedRoster <- readJson "appData/rosters/activePlayers.json" :: IO (Either String O.OfficialRoster)
    case parsedRoster of
        Left _ -> return False
        Right activeRoster -> return $ lookupPlayerInOfficialRoster playerId activeRoster

validatePlayerId :: R.CurrentLineup -> IO (Either String [T.Text])
validatePlayerId lineup = do
    let allPlayers = getUniquePlayerIdsLineup lineup
    nonexistent <- filterM (fmap not . lookupPlayerId) allPlayers
    if null nonexistent
        then return $ Left "All Players are valid."
        else return $ Right nonexistent

-- validatePlayerIdNEW :: R.LgManager -> IO (Either String [T.Text])
-- validatePlayerIdNEW manager = do
--     let allPlayers = getUniquePlayerIdsLineup (R.current_lineup manager)
--     nonexistent <- filterM (fmap not . lookupPlayerId) allPlayers
--     if null nonexistent
--         then return $ Left "All players are valid."
--         else return $ Right nonexistent

-- Utility function to convert an Int ID to T.Text
intToText :: Int -> T.Text
intToText = T.pack . show

getUniquePlayerIdsLineup :: R.CurrentLineup -> [T.Text]
getUniquePlayerIdsLineup R.CurrentLineup{..} =
    cC ++ b1C ++ b2C ++ b3C ++ ssC ++ uC ++ ofC ++ spC ++ rpC

-- getLineupDiscrepancies :: R.CurrentLineup -> C.LgLineupLmts -> [(String, Int)]
-- getLineupDiscrepancies R.CurrentLineup{..} C.LgLineupLmts{..} =
--     let discrepancies =
--             [ validatePositionCount "Catcher" [cC] lg_catcher
--             , validatePositionCount "First Base" [b1C] lg_first
--             , validatePositionCount "Second Base" [b2C] lg_second
--             , validatePositionCount "Third Base" [b3C] lg_third
--             , validatePositionCount "Shortstop" [ssC] lg_shortstop
--             , validatePositionCount "Outfield" ofC lg_outfield
--             , validatePositionCount "Utility" [uC] lg_utility
--             , validatePositionCount "Starting Pitcher" spC lg_s_pitcher
--             , validatePositionCount "Relief Pitcher" rpC lg_r_pitcher
--             ]
--         totalSizeDiscrepancy = totalPlayersInLineup R.CurrentLineup{..} - lg_max_size
--         rosterSizeDiscrepancy = ([("Team Total", totalSizeDiscrepancy) | totalSizeDiscrepancy > 0])
--      in catMaybes discrepancies ++ rosterSizeDiscrepancy

validatePositionCount :: String -> [a] -> Int -> Maybe (String, Int)
validatePositionCount positionName players maxAllowed
    | overage > 0 = Just (positionName, overage)
    | otherwise = Nothing
  where
    overage = length players - maxAllowed

totalPlayersInLineup :: R.CurrentLineup -> Int
totalPlayersInLineup R.CurrentLineup{..} =
    length cC + length b1C + length b2C + length b3C + length ssC + 
    length uC + length ofC + length spC + length rpC

validateCurrentLineup :: R.LgManager -> C.Configuration -> Bool
validateCurrentLineup R.LgManager{..} C.Configuration{point_parameters = C.PointParameters{lineup_limits = rosterConfig}} =
    let positionalValid = null (getLineupDiscrepancies current_lineup rosterConfig)
     in case hasUniqueLineupPlayers current_lineup of
            Left _ -> positionalValid
            Right _ -> False

-- takes a playerId as a String and a LgManager and returns the player's position or fails with an error message
-- addded failure cases to protect against players being in multiple positions or not being found in the lineup
-- even though this should be impossible if I make the lineup-setting tools correctly
findPlayerPosition :: T.Text -> R.LgManager -> Either T.Text T.Text
findPlayerPosition playerId mgr = 
    case concatMap (findPosition playerId) checks of
        [] -> Left "Player not found in current lineup."
        [pos] -> Right pos
        _ -> Left "Player found in multiple positions in the lineup." -- This should not occur with proper management.
  where
    lineup = R.current_lineup mgr
    checks = 
        [ ("C", R.cC lineup)
        , ("1B", R.b1C lineup)
        , ("2B", R.b2C lineup)
        , ("3B", R.b3C lineup)
        , ("SS", R.ssC lineup)
        , ("U", R.uC lineup)
        , ("OF", R.ofC lineup)
        , ("SP", R.spC lineup)
        , ("RP", R.rpC lineup)
        ]
    findPosition pid (pos, players) = if pid `elem` players then [pos] else []

-- takes a playerId as a String and a LgManager
-- and returns whether the player is to be fielded as a batter or pitcher for points calculation purposes
batterOrPitcher :: T.Text -> R.LgManager -> Either T.Text P.StatType
batterOrPitcher playerName mgr
    | any (\posList -> playerName `elem` posList) batterPositions = Right P.Batting
    | any (\posList -> playerName `elem` posList) pitcherPositions = Right P.Pitching
    | otherwise = Left "Player not found in current lineup."
  where
    lineup = R.current_lineup mgr
    batterPositions = [R.cC lineup, R.b1C lineup, R.b2C lineup, R.b3C lineup, R.ssC lineup, R.uC lineup] ++ [R.ofC lineup]
    pitcherPositions = [R.spC lineup] ++ [R.rpC lineup]

