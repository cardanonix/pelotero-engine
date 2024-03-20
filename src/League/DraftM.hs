{-# LANGUAGE OverloadedStrings, FlexibleContexts, DoAndIfThenElse #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}


module DraftM where

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
    ( positionCodeToDraftText, extendRankingsWithUnrankedPlayers, createLgManager )

-- import Draft
import Control.Monad.State
import Control.Monad.Except

data DraftError = PlayerNotFound Int
                | PositionFull T.Text
                | OtherDraftError String
                deriving (Show, Eq)

-- Draft StateT Monad Representation
data DraftState = DraftState {
    rosters :: [(R.Roster, R.CurrentLineup, [Int])],
    currentPick :: Int,
    allPlayers :: [O.OfficialPlayer],
    availableIds :: [Int],
    config :: C.Configuration
}

type DraftM a = ExceptT DraftError (StateT DraftState IO) a

findPlayerMonad :: Int -> [O.OfficialPlayer] -> [Int] -> DraftM O.OfficialPlayer
findPlayerMonad playerId players availableIds = case findPlayer playerId players availableIds of
    Just player -> return player
    Nothing -> throwError $ PlayerNotFound playerId

updateTeamState :: [(R.Roster, R.CurrentLineup, [Int])] -> Int -> (R.Roster, R.CurrentLineup, [Int]) -> [(R.Roster, R.CurrentLineup, [Int])]
updateTeamState teams index (newRoster, newLineup, newAvailableIds) =
    take index teams ++ [(newRoster, newLineup, newAvailableIds)] ++ drop (index + 1) teams

-- serpentine order generator
serpentineOrder :: Int -> Int -> DraftM [[Int]]
serpentineOrder numTeams rounds = return $ map generateOrder [1..rounds]
  where
    generateOrder round = if even round then reverse [1..numTeams] else [1..numTeams]


-- Draft players for each team, given a list of player rankings for each team
draftPlayersMonad :: [[PR.PlayerRanking]] -> DraftM ()
draftPlayersMonad teamRankings = do
  let numTeams = length teamRankings
  draftOrder <- serpentineOrder numTeams (length (head teamRankings)) -- Adjust based on actual rounds
  mapM_ draftCycleMonad (zip draftOrder teamRankings)

-- Assuming allPlayers and availableIds are fields in DraftState
draftCycleMonad :: ([Int], [PR.PlayerRanking]) -> DraftM ()
draftCycleMonad (order, teamRankings) = do
  draftState <- get
  -- Loop through each teamIndex and corresponding playerRanking
  forM_ (zip order teamRankings) $ \(teamIndex, playerRankings) -> do
    -- Retrieve the current roster, lineup, and available IDs for the team
    let (roster, lineup, _) = rosters draftState !! teamIndex
    -- Process each playerRanking for the current team
    -- (Simplified for demonstration; implement your logic here)
    let updatedRoster = roster  -- Placeholder for the actual roster update logic
    let updatedLineup = lineup  -- Placeholder for the actual lineup update logic
    -- Update the state with the new roster and lineup
    let newRosters = updateTeamState (rosters draftState) teamIndex (updatedRoster, updatedLineup, availableIds draftState)
    put $ draftState { rosters = newRosters }

addToRosterAndLineupM :: O.OfficialPlayer -> DraftM ()
addToRosterAndLineupM player = do
  draftState <- get
  let teamIndex = currentPick draftState `mod` length (rosters draftState)
      (currentRoster, currentLineup, currentAvailableIds) = rosters draftState !! teamIndex
      positionText = O.primaryPosition player
      draftPositionText = positionCodeToDraftText positionText
      configData = config draftState  -- Access configuration from state

  -- Perform actions based on player position (pitcher or batter)
  (updatedRoster, updatedLineup) <- if draftPositionText == "pitcher" then do
      updatedRoster <- addPitcherToRosterM player currentRoster
      updatedLineup <- addPlayerToLineupM draftPositionText player currentLineup 
      return (updatedRoster, updatedLineup)
    else do
      (updatedRoster, isAddedToRoster) <- addBatterToRosterM draftPositionText player currentRoster
      updatedLineup <- if isAddedToRoster
                       then addPlayerToLineupM draftPositionText player currentLineup
                       else return currentLineup
      return (updatedRoster, updatedLineup)

  -- Update available IDs and rosters
  let newAvailableIds = delete (O.playerId player) currentAvailableIds
  let newRosters = updateTeamState (rosters draftState) teamIndex (updatedRoster, updatedLineup, newAvailableIds)
  put $ draftState { rosters = newRosters, availableIds = newAvailableIds }

addPitcherToRosterM :: O.OfficialPlayer -> R.Roster -> DraftM R.Roster
addPitcherToRosterM player roster = do
  draftState <- get
  let configData = config draftState  -- Use a different name to avoid shadowing
      spLimit = queryDraftRosterLmt "s_pitcher" $ C.draft_limits $ C.draft_parameters configData
      rpLimit = queryDraftRosterLmt "r_pitcher" $ C.draft_limits $ C.draft_parameters configData
      spCount = length $ R.spR roster
      rpCount = length $ R.rpR roster
  if spCount < spLimit then
      addPlayerToPositionM "s_pitcher" player roster
  else if rpCount < rpLimit then
      addPlayerToPositionM "r_pitcher" player roster
  else
      throwError $ PositionFull "Pitcher positions are full"

addBatterToRosterM :: T.Text -> O.OfficialPlayer -> R.Roster -> DraftM (R.Roster, Bool)
addBatterToRosterM position player roster = do
  draftState <- get
  let configData = config draftState
      currentCount = countPlayers position roster
      limit = queryDraftRosterLmt position $ C.draft_limits $ C.draft_parameters configData
  if currentCount < limit then do
      updatedRoster <- addPlayerToPositionM position player roster
      return (updatedRoster, True)
  else
      return (roster, False)

addPlayerToLineupM :: T.Text -> O.OfficialPlayer -> R.CurrentLineup -> C.LgRosterLmts -> DraftM R.CurrentLineup
addPlayerToLineupM position player lineup limits = do
    draftState <- get
    let playerIdText = T.pack . show $ O.playerId player
        configData = config draftState
    case position of
        "catcher" -> updateLineup (R.cC lineup) (C.lg_catcher limits) R.cC (\l -> lineup{R.cC = l}) playerIdText
        "first" -> updateLineup (R.b1C lineup) (C.lg_first limits) R.b1C (\l -> lineup{R.b1C = l}) playerIdText
        "second" -> updateLineup (R.b2C lineup) (C.lg_second limits) R.b2C (\l -> lineup{R.b2C = l}) playerIdText
        "third" -> updateLineup (R.b3C lineup) (C.lg_third limits) R.b3C (\l -> lineup{R.b3C = l}) playerIdText
        "shortstop" -> updateLineup (R.ssC lineup) (C.lg_shortstop limits) R.ssC (\l -> lineup{R.ssC = l}) playerIdText
        "outfield" -> updateLineup (R.ofC lineup) (C.lg_outfield limits) R.ofC (\l -> lineup{R.ofC = l}) playerIdText
        "utility" -> updateLineup (R.uC lineup) (C.lg_utility limits) R.uC (\l -> lineup{R.uC = l}) playerIdText
        "s_pitcher" -> updateLineup (R.spC lineup) (C.lg_s_pitcher limits) R.spC (\l -> lineup{R.spC = l}) playerIdText
        "r_pitcher" -> updateLineup (R.rpC lineup) (C.lg_r_pitcher limits) R.rpC (\l -> lineup{R.rpC = l}) playerIdText
        _ -> throwError $ OtherDraftError $ "Unknown position " <> T.unpack position
  where
    updateLineup currentPos limitPos accessor updater playerIdText =
      if length currentPos < limitPos
      then return $ updater (playerIdText : accessor lineup)
      else throwError $ PositionFull $ "Position " <> position <> " is full"

addPlayerToPositionM :: T.Text -> O.OfficialPlayer -> R.Roster -> DraftM R.Roster
addPlayerToPositionM position player roster = do
  let playerIdText = T.pack $ show $ O.playerId player
  return $ case position of
    "s_pitcher" -> roster { R.spR = playerIdText : R.spR roster }
    "r_pitcher" -> roster { R.rpR = playerIdText : R.rpR roster }
    "catcher" -> roster { R.cR = playerIdText : R.cR roster }
    "first" -> roster { R.b1R = playerIdText : R.b1R roster }
    "second" -> roster { R.b2R = playerIdText : R.b2R roster }
    "third" -> roster { R.b3R = playerIdText : R.b3R roster }
    "shortstop" -> roster { R.ssR = playerIdText : R.ssR roster }
    "outfield" -> roster { R.ofR = playerIdText : R.ofR roster }
    "utility" -> roster { R.uR = playerIdText : R.uR roster }
    _ -> roster  -- Default case if position does not match


-- Add necessary utility functions here, adapted to work within DraftM
-- For instance, adding players to rosters/lineups, checking position limits, etc.
