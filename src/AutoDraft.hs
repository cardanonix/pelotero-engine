{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, foldM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

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


positionCodeToText :: Text.Text -> Text.Text
positionCodeToText code =
  case code of
    "P" -> "pitcher"    -- Adjusted for general pitcher position, consider splitting into s_pitcher and r_pitcher if necessary
    "C" -> "catcher"
    "1B" -> "first"
    "2B" -> "second"
    "3B" -> "third"
    "SS" -> "shortstop"
    "LF" -> "outfield"  -- outfield positions are generalized
    "CF" -> "outfield"
    "RF" -> "outfield"
    "DH" -> "utility"
    _ -> "Unknown"

positionCodeToOfficialText :: Text.Text -> Text.Text
positionCodeToOfficialText code =
  case code of
    "1" -> "P"
    "2" -> "C"
    "3" -> "1B"
    "4" -> "2B"
    "5" -> "3B"
    "6" -> "SS"
    "7" -> "LF"
    "8" -> "CF"
    "9" -> "RF"
    "10" -> "DH"
    _ -> "Unknown"

-- Corrected function for translating position codes to draft text
positionCodeToDraftText :: Text.Text -> Text.Text
positionCodeToDraftText code =
  let officialText = positionCodeToOfficialText code
  in officialTextToDraftText officialText
  where
    officialTextToDraftText :: Text.Text -> Text.Text
    officialTextToDraftText officialText =
      case officialText of
        "P"  -> "pitcher"
        "C"  -> "catcher"
        "1B" -> "first"
        "2B" -> "second"
        "3B" -> "third"
        "SS" -> "shortstop"
        "LF" -> "outfield"
        "CF" -> "outfield"
        "RF" -> "outfield"
        "DH" -> "utility"
        _    -> "Unknown"
 
positionTextToRosterPosition :: Text.Text -> R.Roster -> O.OfficialPlayer -> R.Roster
positionTextToRosterPosition position roster player =
  -- Implementation depends on how you're managing roster updates
  undefined

-- Adjusted draftPlayers to include logic for unranked players
draftPlayers :: [PR.PlayerRanking] -> [PR.PlayerRanking] -> [O.OfficialPlayer] -> C.Configuration -> IO (R.Roster, R.Roster)
draftPlayers rankings1 rankings2 officialPlayers config = do
  let officialPlayerIds = map O.playerId officialPlayers
      extendedRankings1 = extendRankingsWithUnrankedPlayers rankings1 officialPlayerIds
      extendedRankings2 = extendRankingsWithUnrankedPlayers rankings2 officialPlayerIds
      rankingsPairs = zip extendedRankings1 extendedRankings2
      initialTurn = True -- Assume Team 1 starts
      initialState = (emptyRoster, emptyRoster, officialPlayerIds, initialTurn)

  -- Correctly pass along the turn indicator in each draft cycle
  (roster1, roster2, _, _) <- foldM (draftCycle config officialPlayers) initialState (map (\pair -> (pair, initialTurn)) rankingsPairs)

  return (roster1, roster2)


draftCycle :: C.Configuration -> [O.OfficialPlayer] -> (R.Roster, R.Roster, [Int], Bool) -> ((Int, Int), Bool) -> IO (R.Roster, R.Roster, [Int], Bool)
draftCycle config officialPlayers (roster1, roster2, availablePlayers, isTeam1Turn) ((rankId1, rankId2), nextIsTeam1Turn) = do
    putStrLn $ "Attempting to draft players with IDs: " ++ show rankId1 ++ ", " ++ show rankId2
    let (firstPick, secondPick) = if isTeam1Turn then (rankId1, rankId2) else (rankId2, rankId1)
    let player1 = findPlayer firstPick officialPlayers availablePlayers
    putStrLn $ "Player 1 found: " ++ show (fmap O.playerId player1)
    let availablePlayersAfterP1 = maybe availablePlayers (\p -> delete (O.playerId p) availablePlayers) player1
    let player2 = findPlayer secondPick officialPlayers availablePlayersAfterP1
    putStrLn $ "Player 2 found: " ++ show (fmap O.playerId player2)
    let (updatedRoster1, updatedRoster2) = if isTeam1Turn
                                            then (maybe roster1 (\p -> addToRoster config p roster1) player1, maybe roster2 (\p -> addToRoster config p roster2) player2)
                                            else (maybe roster1 (\p -> addToRoster config p roster1) player2, maybe roster2 (\p -> addToRoster config p roster2) player1)
    putStrLn "Rosters updated."
    let availablePlayersAfterP2 = maybe availablePlayersAfterP1 (\p -> delete (O.playerId p) availablePlayersAfterP1) player2
    return (updatedRoster1, updatedRoster2, availablePlayersAfterP2, not isTeam1Turn)

extendRankingsWithUnrankedPlayers :: [PR.PlayerRanking] -> [Int] -> [Int]
extendRankingsWithUnrankedPlayers rankedPlayers allPlayerIds =
    let rankedPlayerIds = map PR.playerId rankedPlayers
        unrankedPlayerIds = filter (`notElem` rankedPlayerIds) allPlayerIds
    in rankedPlayerIds ++ unrankedPlayerIds -- Concatenate ranked with unranked

findPlayer :: Int -> [O.OfficialPlayer] -> [Int] -> Maybe O.OfficialPlayer
findPlayer playerId players availableIds =
    find (\p -> O.playerId p == playerId && playerId `elem` availableIds) players

addToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.Roster
addToRoster config player roster =
  let positionCode = O.primaryPosition player
      draftPositionText = positionCodeToDraftText positionCode
      -- Assuming any pitcher can be assigned to SP or RP roles based on availability
      isPitcher = draftPositionText == "pitcher"
      addToPosition = if isPitcher then
                        addPitcherToRoster config player roster
                      else
                        addNonPitcherToRoster config player roster draftPositionText
  in addToPosition

addPitcherToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.Roster
addPitcherToRoster config player roster =
  let rosterLimitSP = lookupLimit "s_pitcher" (C.draft_limits $ C.draft_parameters config)
      rosterLimitRP = lookupLimit "r_pitcher" (C.draft_limits $ C.draft_parameters config)
      currentPositionCountSP = countPlayers "s_pitcher" roster
      currentPositionCountRP = countPlayers "r_pitcher" roster
  in if currentPositionCountSP < rosterLimitSP then
       addPlayerToPosition "s_pitcher" player roster
     else if currentPositionCountRP < rosterLimitRP then
       addPlayerToPosition "r_pitcher" player roster
     else
       roster -- No action if no pitching slots available

addNonPitcherToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> Text.Text -> R.Roster
addNonPitcherToRoster config player roster position =
  let rosterLimit = lookupLimit position (C.draft_limits $ C.draft_parameters config)
      currentPositionCount = countPlayers position roster
  in if currentPositionCount < rosterLimit then
       addPlayerToPosition position player roster
     else
       roster -- No action if position is full

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
       "s_pitcher" -> roster { R.spR = playerIdText : R.spR roster }
       "r_pitcher" -> roster { R.rpR = playerIdText : R.rpR roster }
       "catcher" -> roster { R.cR = playerIdText : R.cR roster }
       "first" -> roster { R.b1R = playerIdText : R.b1R roster }
       "second" -> roster { R.b2R = playerIdText : R.b2R roster }
       "third" -> roster { R.b3R = playerIdText : R.b3R roster }
       "shortstop" -> roster { R.ssR = playerIdText : R.ssR roster }
       "outfield" -> roster { R.ofR = playerIdText : R.ofR roster }
       "utility" -> roster { R.uR = playerIdText : R.uR roster }
       _ -> roster -- Default case if position does not match

main :: IO ()
main = do
    eitherR1 <- readJson "testFiles/appData/rankings/team001_rankings.json"
    eitherR2 <- readJson "testFiles/appData/rankings/team002_rankings.json"
    eitherRoster <- readJson "testFiles/appData/rosters/activePlayers.json"
    eitherConfig <- readJson "testFiles/appData/config/config.json"

    case (eitherR1, eitherR2, eitherRoster, eitherConfig) of
        (Right r1, Right r2, Right roster, Right config) -> do
            let rankings1 = PR.rankings r1
                rankings2 = PR.rankings r2
                op = O.people roster
            (finalRoster1, finalRoster2) <- draftPlayers rankings1 rankings2 op config
            writeJson "testFiles/appData/draftResults/team1Roster.json" finalRoster1
            writeJson "testFiles/appData/draftResults/team2Roster.json" finalRoster2
            putStrLn "Draft completed successfully."
        _ -> putStrLn "Failed to load one or more necessary files. Check file paths and data integrity."
