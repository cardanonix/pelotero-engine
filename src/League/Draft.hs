{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}

module Draft where

import Control.Monad (forM, foldM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List
    ( find,
      delete,
      sortOn,
      sortBy,
      findIndex,
      sortOn,
      findIndex,
      sortOn,
      findIndex,
      sortOn,
      findIndex )
import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified Ranking as PR
import Validators ( countPlayers, findPlayer, queryDraftRosterLmt, queryLgRosterLmts )
import Utility

-- Verify if a player is in the official roster
isPlayerInOfficialRoster :: Int -> [O.OfficialPlayer] -> Bool
isPlayerInOfficialRoster playerId officialRoster =
    any (\p -> O.playerId p == playerId) officialRoster

positionTextToRosterPosition :: T.Text -> R.Roster -> O.OfficialPlayer -> R.Roster
positionTextToRosterPosition position roster player =
  -- Implementation depends on how you're managing roster updates
  undefined

getPositionLimit :: T.Text -> C.LgRosterLmts -> Int
getPositionLimit position rosterLimits =
  case position of
    "C"  -> C.lg_catcher rosterLimits
    "1B" -> C.lg_first rosterLimits
    "2B" -> C.lg_second rosterLimits
    "3B" -> C.lg_third rosterLimits
    "SS" -> C.lg_shortstop rosterLimits
    "OF" -> C.lg_outfield rosterLimits
    "U"  -> C.lg_utility rosterLimits
    "SP" -> C.lg_s_pitcher rosterLimits
    "RP" -> C.lg_r_pitcher rosterLimits
    _    -> error $ "Invalid position: " <> T.unpack position

findPlayerRanking :: Int -> [PR.PlayerRanking] -> Maybe Int
findPlayerRanking playerId rankings =
  PR.rank <$> find ((== playerId) . PR.playerId) rankings

-- filters the official roster by position and sorts them by their ranking.
getRankedPlayersForPosition :: [PR.PlayerRanking] -> [O.OfficialPlayer] -> T.Text -> [T.Text]
getRankedPlayersForPosition rankings officialPlayers positionText =
  let
    -- Filter players by position directly without conversion.
    positionFilteredPlayers = filter (\p -> O.primaryPosition p == positionText) officialPlayers
    -- Create a mapping of playerId to rank for quicker lookups.
    playerIdToRankMap = foldr (\r acc -> HM.insert (PR.playerId r) (PR.rank r) acc) HM.empty rankings
    -- Sort players based on their ranking, placing unranked players at the end.
    sortedPlayers = sortOn (\p -> HM.lookup (O.playerId p) playerIdToRankMap) positionFilteredPlayers
  in map (T.pack . show . O.playerId) sortedPlayers

-- Adjusted draftPlayers to include logic for unranked players
draftPlayers :: [PR.PlayerRanking] -> [PR.PlayerRanking] -> [O.OfficialPlayer] -> C.Configuration -> IO (R.Roster, R.Roster)
draftPlayers rankings1 rankings2 officialPlayers config = do
  let officialPlayerIds = map O.playerId officialPlayers
      extendedRankings1 = extendRankingsWithUnrankedPlayers rankings1 officialPlayerIds
      extendedRankings2 = extendRankingsWithUnrankedPlayers rankings2 officialPlayerIds
      rankingsPairs = zip extendedRankings1 extendedRankings2
      initialTurn = True -- assuming Team 1 starts
      initialState = (R.mkEmptyRoster, R.mkEmptyRoster, officialPlayerIds, initialTurn)
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

addToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.Roster
addToRoster config player roster =
  let positionCode = O.primaryPosition player
      draftPositionText = positionCodeToDraftText positionCode
      -- Assuming any pitcher can be assigned to SP or RP roles based on availability
      isPitcher = draftPositionText == "pitcher"
      addToPosition = if isPitcher then
                        addPitcherToRoster config player roster
                      else
                        addBatterToRoster config player roster draftPositionText
  in addToPosition

addPitcherToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.Roster
addPitcherToRoster config player roster =
  let rosterLimitSP = queryDraftRosterLmt "s_pitcher" (C.draft_limits $ C.draft_parameters config)
      rosterLimitRP = queryDraftRosterLmt "r_pitcher" (C.draft_limits $ C.draft_parameters config)
      currentPositionCountSP = countPlayers "s_pitcher" roster
      currentPositionCountRP = countPlayers "r_pitcher" roster
  in if currentPositionCountSP < rosterLimitSP then
       addPlayerToPosition "s_pitcher" player roster
     else if currentPositionCountRP < rosterLimitRP then
       addPlayerToPosition "r_pitcher" player roster
     else
       roster -- No action if no pitching slots available

addBatterToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> T.Text -> R.Roster
addBatterToRoster config player roster position =
  let rosterLimit = queryDraftRosterLmt position (C.draft_limits $ C.draft_parameters config)
      currentPositionCount = countPlayers position roster
  in if currentPositionCount < rosterLimit then
       addPlayerToPosition position player roster
     else
       roster -- No action if position is full

addPlayerToPosition :: T.Text -> O.OfficialPlayer -> R.Roster -> R.Roster
addPlayerToPosition position player roster =
  let playerIdText = T.pack $ show $ O.playerId player
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

createLgManager :: C.Configuration -> T.Text -> R.CurrentLineup -> R.Roster -> R.LgManager
createLgManager config teamId currentLineup roster =
    R.LgManager (C.status config) (C.commissioner config) teamId (C.leagueID config) currentLineup roster

-- New functionality
-- Refactored to return both Rosters and Lineups for two teams

draftPlayersNew :: [PR.PlayerRanking] -> [PR.PlayerRanking] -> [O.OfficialPlayer] -> C.Configuration -> IO ((R.Roster, R.CurrentLineup), (R.Roster, R.CurrentLineup))
draftPlayersNew rankings1 rankings2 officialPlayers config = do
  let officialPlayerIds = map O.playerId officialPlayers
      extendedRankings1 = extendRankingsWithUnrankedPlayers rankings1 officialPlayerIds
      extendedRankings2 = extendRankingsWithUnrankedPlayers rankings2 officialPlayerIds
      rankingsPairs = zip extendedRankings1 extendedRankings2
      initialTurn = True -- Assuming Team 1 starts
      -- Adjusted to match the expected tuple type including lineups
      initialState = (R.mkEmptyRoster, R.mkEmptyLineup, R.mkEmptyRoster, R.mkEmptyLineup, officialPlayerIds, initialTurn)
  
  -- Correct the foldM call to expect the full state, including lineups
  (finalRoster1, finalLineup1, finalRoster2, finalLineup2, _, _) <- foldM (draftCycleNew config officialPlayers) initialState (map (\pair -> (pair, initialTurn)) rankingsPairs)
  
  return ((finalRoster1, finalLineup1), (finalRoster2, finalLineup2))

draftCycleNew :: C.Configuration -> [O.OfficialPlayer] -> (R.Roster, R.CurrentLineup, R.Roster, R.CurrentLineup, [Int], Bool) -> ((Int, Int), Bool) -> IO (R.Roster, R.CurrentLineup, R.Roster, R.CurrentLineup, [Int], Bool)
draftCycleNew config officialPlayers (roster1, lineup1, roster2, lineup2, availablePlayers, isTeam1Turn) ((rankId1, rankId2), nextIsTeam1Turn) = do
    putStrLn $ "Drafting players with IDs: " ++ show (rankId1, rankId2)
    let player1 = findPlayer rankId1 officialPlayers availablePlayers
        player2 = findPlayer rankId2 officialPlayers (maybe availablePlayers (\p -> delete (O.playerId p) availablePlayers) player1)

    let (updatedRoster1, updatedLineup1) = maybe (roster1, lineup1) (\p -> addToRosterAndLineup config p roster1 lineup1) player1
        availablePlayersAfterP1 = maybe availablePlayers (\p -> delete (O.playerId p) availablePlayers) player1

        (updatedRoster2, updatedLineup2) = maybe (roster2, lineup2) (\p -> addToRosterAndLineup config p roster2 lineup2) player2
        availablePlayersAfterP2 = maybe availablePlayersAfterP1 (\p -> delete (O.playerId p) availablePlayersAfterP1) player2

    return (updatedRoster1, updatedLineup1, updatedRoster2, updatedLineup2, availablePlayersAfterP2, not isTeam1Turn)


-- Add to both roster and lineup
-- Add a player to both roster and lineup based on availability and position limits
addToRosterAndLineup :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.CurrentLineup -> (R.Roster, R.CurrentLineup)
addToRosterAndLineup config player roster lineup =
    let draftPositionText = positionCodeToDraftText $ O.primaryPosition player
        isPitcher = draftPositionText `elem` ["s_pitcher", "r_pitcher"]
        positionLimits = C.valid_roster . C.point_parameters $ config
    in if isPitcher
       then -- For pitchers, decide between starting and relief based on available slots
            let spAvailable = countPlayers "s_pitcher" roster < C.lg_s_pitcher positionLimits
                rpAvailable = countPlayers "r_pitcher" roster < C.lg_r_pitcher positionLimits
            in if spAvailable
               then addPlayerToBoth config "s_pitcher" player roster lineup
               else if rpAvailable
                    then addPlayerToBoth config "r_pitcher" player roster lineup
                    else (roster, lineup) -- No slots available
       else -- For batters, directly add based on their primary position
            addPlayerToBoth config draftPositionText player roster lineup


-- Helper function to add a player to both roster and lineup for a given position
-- Adjusted to include Configuration as a parameter
addPlayerToBoth :: C.Configuration -> T.Text -> O.OfficialPlayer -> R.Roster -> R.CurrentLineup -> (R.Roster, R.CurrentLineup)
addPlayerToBoth config position player roster lineup =
    let newRoster = addPlayerToPosition position player roster
        newLineup = addPlayerToLineup position player lineup (C.valid_roster . C.point_parameters $ config)
    in (newRoster, newLineup)


addPlayerToLineup :: T.Text -> O.OfficialPlayer -> R.CurrentLineup -> C.LgRosterLmts -> R.CurrentLineup
addPlayerToLineup position player lineup limits =
    let playerIdText = T.pack . show $ O.playerId player
    in case position of
        "catcher" -> if length (R.cC lineup) < C.lg_catcher limits then lineup { R.cC = playerIdText : R.cC lineup } else lineup
        "first" -> if length (R.b1C lineup) < C.lg_first limits then lineup { R.b1C = playerIdText : R.b1C lineup } else lineup
        "second" -> if length (R.b2C lineup) < C.lg_second limits then lineup { R.b2C = playerIdText : R.b2C lineup } else lineup
        "third" -> if length (R.b3C lineup) < C.lg_third limits then lineup { R.b3C = playerIdText : R.b3C lineup } else lineup
        "shortstop" -> if length (R.ssC lineup) < C.lg_shortstop limits then lineup { R.ssC = playerIdText : R.ssC lineup } else lineup
        "outfield" -> if length (R.ofC lineup) < C.lg_outfield limits then lineup { R.ofC = playerIdText : R.ofC lineup } else lineup
        "utility" -> if length (R.uC lineup) < C.lg_utility limits then lineup { R.uC = playerIdText : R.uC lineup } else lineup
        "s_pitcher" -> if length (R.spC lineup) < C.lg_s_pitcher limits then lineup { R.spC = playerIdText : R.spC lineup } else lineup
        "r_pitcher" -> if length (R.rpC lineup) < C.lg_r_pitcher limits then lineup { R.rpC = playerIdText : R.rpC lineup } else lineup
        _ -> lineup  


