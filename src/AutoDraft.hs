{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, foldM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)
import Data.Maybe (mapMaybe)
import Data.List (find, delete)
import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified Ranking as PR
import Validators

writeJson :: ToJSON a => FilePath -> a -> IO ()
writeJson filePath = BL.writeFile filePath . encode

-- Creates an empty roster based on the league's draft limits
emptyRoster :: R.Roster
emptyRoster = R.Roster [] [] [] [] [] [] [] [] []

draftPlayers :: [PR.PlayerRanking] -> [PR.PlayerRanking] -> [O.OfficialPlayer] -> C.Configuration -> IO (R.Roster, R.Roster)
draftPlayers rankings1 rankings2 officialPlayers config = do
    let officialPlayerIds = map O.playerId officialPlayers
    let validRankedIds1 = filter (\ranking -> PR.playerId ranking `elem` officialPlayerIds) rankings1
    let validRankedIds2 = filter (\ranking -> PR.playerId ranking `elem` officialPlayerIds) rankings2
    let validRankedPlayerIds1 = map PR.playerId validRankedIds1
    let validRankedPlayerIds2 = map PR.playerId validRankedIds2

    -- Use foldM to iterate over ranked player IDs, final step discards the list of available player IDs
    (roster1, roster2, _) <- foldM (draftCycle config officialPlayers) (emptyRoster, emptyRoster, officialPlayerIds) (zip validRankedPlayerIds1 validRankedPlayerIds2)
    return (roster1, roster2)
    
draftCycle :: C.Configuration -> [O.OfficialPlayer] -> (R.Roster, R.Roster, [Int]) -> (Int, Int) -> IO (R.Roster, R.Roster, [Int])
draftCycle config officialPlayers (roster1, roster2, availablePlayers) (rankId1, rankId2) = do
    let player1 = findPlayer rankId1 officialPlayers availablePlayers
    let availablePlayersAfterP1 = maybe availablePlayers (\p -> delete (O.playerId p) availablePlayers) player1
    let player2 = findPlayer rankId2 officialPlayers availablePlayersAfterP1
    let updatedRoster1 = maybe roster1 (\p -> addToRoster config p roster1) player1
    let updatedRoster2 = maybe roster2 (\p -> addToRoster config p roster2) player2
    let availablePlayersAfterP2 = maybe availablePlayersAfterP1 (\p -> delete (O.playerId p) availablePlayersAfterP1) player2
    return (updatedRoster1, updatedRoster2, availablePlayersAfterP2)

-- Finds a player by ID in the list of official players, if available
findPlayer :: Int -> [O.OfficialPlayer] -> [Int] -> Maybe O.OfficialPlayer
findPlayer playerId players availableIds
    | playerId `elem` availableIds = find (\p -> O.playerId p == playerId) players
    | otherwise = Nothing

addToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.Roster
addToRoster config player roster =
    let position = O.primaryPosition player
        rosterLimit = lookupLimit position (C.draft_limits $ C.draft_parameters config)
        currentPositionCount = countPlayers position roster
    in if currentPositionCount < rosterLimit
       then addPlayerToPosition position player roster
       else roster

lookupLimit :: Text.Text -> C.DraftRoster -> Int
lookupLimit position limits =
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

countPlayers :: Text.Text -> R.Roster -> Int
countPlayers position roster =
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

addPlayerToPosition :: Text.Text -> O.OfficialPlayer -> R.Roster -> R.Roster
addPlayerToPosition position player roster =
    let playerIdText = Text.pack $ show $ O.playerId player
    in case position of
        "catcher" -> roster { R.cR = playerIdText : R.cR roster }
        "first" -> roster { R.b1R = playerIdText : R.b1R roster }
        "second" -> roster { R.b2R = playerIdText : R.b2R roster }
        "third" -> roster { R.b3R = playerIdText : R.b3R roster }
        "shortstop" -> roster { R.ssR = playerIdText : R.ssR roster }
        "outfield" -> roster { R.ofR = playerIdText : R.ofR roster }
        "utility" -> roster { R.uR = playerIdText : R.uR roster }
        "s_pitcher" -> roster { R.spR = playerIdText : R.spR roster }
        "r_pitcher" -> roster { R.rpR = playerIdText : R.rpR roster }
        _ -> roster  -- No action if position is not recognized

main :: IO ()
main = do
    rankings1Result <- readJson "testFiles/appData/rankings/team001_rankings.json"
    rankings2Result <- readJson "testFiles/appData/rankings/team002_rankings.json"
    officialPlayersResult <- readJson "testFiles/appData/rosters/activePlayers.json"
    configResult <- readJson "testFiles/appData/config/config.json"

    case rankings1Result of
      Left err -> putStrLn $ "Failed to load team 1 rankings: " ++ err
      Right r1 -> case rankings2Result of
        Left err -> putStrLn $ "Failed to load team 2 rankings: " ++ err
        Right r2 -> case officialPlayersResult of
          Left err -> putStrLn $ "Failed to load official players: " ++ err
          Right op -> case configResult of
            Left err -> putStrLn $ "Failed to load configuration: " ++ err
            Right cfg -> do
              (roster1, roster2) <- draftPlayers r1 r2 op cfg
              putStrLn "Draft completed."
              writeJson "draftResults/team1Roster.json" roster1
              writeJson "draftResults/team2Roster.json" roster2
