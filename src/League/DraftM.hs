{-# LANGUAGE OverloadedStrings, FlexibleContexts, DoAndIfThenElse #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}


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
import Validators ( countPlayers, findPlayer, queryDraftRosterLmts, queryLgLineupLmts )
import Utility
    ( positionCodeToDraftText, extendRankingsWithUnrankedPlayers, createLgManager )

-- import Draft
import Control.Monad.State
import Control.Monad.Except

-- Draft StateT Monad Representation
data DraftState = DraftState {
    rosters :: [(R.Roster, R.CurrentLineup, [Int])],
    currentPick :: Int,
    allPlayers :: [O.OfficialPlayer],
    availableIds :: [Int],
    config :: C.Configuration
}

-- Example: More specific errors for common operations
data DraftError
  = PlayerNotFound Int
  | PositionFull T.Text
  | InvalidTeamIndex Int
  | OtherDraftError String
  deriving (Show, Eq)

type DraftM a = ExceptT DraftError (StateT DraftState IO) a

updateDraftState :: (DraftState -> DraftState) -> DraftM ()
updateDraftState f = get >>= put . f

getTeamByIndexSafe :: Int -> DraftM (R.Roster, R.CurrentLineup, [Int])
getTeamByIndexSafe index = do
  draftState <- get
  let teams = rosters draftState
  maybe (throwError $ InvalidTeamIndex index) return (atMay teams index)
  where
    atMay :: [a] -> Int -> Maybe a
    atMay xs n = if n >= 0 && n < length xs then Just (xs !! n) else Nothing

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

draftPlayersMonad :: [[PR.PlayerRanking]] -> DraftM ()
draftPlayersMonad teamRankings = do
  when (null teamRankings) $
    throwError $ OtherDraftError "Team rankings cannot be empty."
  let numTeams = length teamRankings
  draftOrder <- serpentineOrder numTeams (length $ head teamRankings)
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
  (currentRoster, currentLineup, currentAvailableIds) <- getTeamByIndexSafe teamIndex
  
  let positionText = O.primaryPosition player
      draftPositionText = positionCodeToDraftText positionText

  -- Process player addition based on the position
  (updatedRoster, updatedLineup, newAvailableIds) <- case draftPositionText of
      "pitcher" -> do
        updatedRoster <- addPitcherToRosterM player currentRoster
        updatedLineup <- addPlayerToLineupM draftPositionText player currentLineup
        let newAvailableIds = delete (O.playerId player) currentAvailableIds
        return (updatedRoster, updatedLineup, newAvailableIds)
      position -> do
        (updatedRoster, added) <- addBatterToRosterM position player currentRoster
        updatedLineup <- if added then addPlayerToLineupM position player currentLineup else pure currentLineup
        let newAvailableIds = if added then delete (O.playerId player) currentAvailableIds else currentAvailableIds
        return (updatedRoster, updatedLineup, newAvailableIds)

  -- Update state with new rosters, lineups, and available IDs
  let updatedTeams = updateTeamState (rosters draftState) teamIndex (updatedRoster, updatedLineup, newAvailableIds)
  put draftState { rosters = updatedTeams, availableIds = newAvailableIds }


addPitcherToRosterM :: O.OfficialPlayer -> R.Roster -> DraftM R.Roster
addPitcherToRosterM player roster = do
  draftState <- get
  let configData = config draftState  -- Use a different name to avoid shadowing
      spLimit = queryDraftRosterLmts "s_pitcher" $ C.draft_limits $ C.draft_parameters configData
      rpLimit = queryDraftRosterLmts "r_pitcher" $ C.draft_limits $ C.draft_parameters configData
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
      limit = queryDraftRosterLmts position $ C.draft_limits $ C.draft_parameters configData
  if currentCount < limit then do
      updatedRoster <- addPlayerToPositionM position player roster
      return (updatedRoster, True)
  else
      return (roster, False)

addPlayerToLineupM :: T.Text -> O.OfficialPlayer -> R.CurrentLineup -> DraftM R.CurrentLineup
addPlayerToLineupM position player lineup = do
    draftState <- get
    let playerIdText = T.pack . show $ O.playerId player
        limits = C.valid_roster $ C.point_parameters (config draftState)
        -- the specific limit for the player's position
        limit = queryLgLineupLmts position limits

    case position of
        "catcher" -> updateLineup limit (R.cC lineup) R.cC (\l -> lineup{R.cC = l}) playerIdText
        "first" -> updateLineup limit (R.b1C lineup) R.b1C (\l -> lineup{R.b1C = l}) playerIdText
        "second" -> updateLineup limit (R.b2C lineup) R.b2C (\l -> lineup{R.b2C = l}) playerIdText
        "third" -> updateLineup limit (R.b3C lineup) R.b3C (\l -> lineup{R.b3C = l}) playerIdText
        "shortstop" -> updateLineup limit (R.ssC lineup) R.ssC (\l -> lineup{R.ssC = l}) playerIdText
        "outfield" -> updateLineup limit (R.ofC lineup) R.ofC (\l -> lineup{R.ofC = l}) playerIdText
        "utility" -> updateLineup limit (R.uC lineup) R.uC (\l -> lineup{R.uC = l}) playerIdText
        "s_pitcher" -> updateLineup limit (R.spC lineup) R.spC (\l -> lineup{R.spC = l}) playerIdText
        "r_pitcher" -> updateLineup limit (R.rpC lineup) R.rpC (\l -> lineup{R.rpC = l}) playerIdText
        _ -> throwError $ OtherDraftError $ "Unknown position " <> T.unpack position
  where
    updateLineup limit currentPos accessor updater playerIdText =
        if length currentPos < limit
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

-- Helper function to determine if a player can be added based on position limits
canAddPlayerToPosition :: T.Text -> R.Roster -> C.Configuration -> Bool
canAddPlayerToPosition position roster config = 
  let limit = queryDraftRosterLmts position $ C.draft_limits $ C.draft_parameters config
      currentCount = countPlayers position roster
  in currentCount < limit

updateRosterAndLineup :: Int -> R.Roster -> R.CurrentLineup -> [Int] -> DraftM ()
updateRosterAndLineup teamIndex updatedRoster updatedLineup newAvailableIds = do
  updateDraftStateWithNewTeamData teamIndex updatedRoster updatedLineup newAvailableIds

updateDraftStateWithNewTeamData :: Int -> R.Roster -> R.CurrentLineup -> [Int] -> DraftM ()
updateDraftStateWithNewTeamData teamIndex updatedRoster updatedLineup newAvailableIds = modify' $ \ds ->
  let updatedTeams = take teamIndex (rosters ds) ++
                     [(updatedRoster, updatedLineup, newAvailableIds)] ++
                     drop (teamIndex + 1) (rosters ds)
  in ds { rosters = updatedTeams, availableIds = newAvailableIds }

updateDraftStateWithTeam :: Int -> R.Roster -> R.CurrentLineup -> [Int] -> DraftM ()
updateDraftStateWithTeam teamIndex updatedRoster updatedLineup newAvailableIds = updateDraftState $ \ds ->
    let updatedTeams = updateTeamAtIndex (rosters ds) teamIndex (updatedRoster, updatedLineup, newAvailableIds)
    in ds { rosters = updatedTeams, availableIds = newAvailableIds }

updateTeamAtIndex :: [(R.Roster, R.CurrentLineup, [Int])] -> Int -> (R.Roster, R.CurrentLineup, [Int]) -> [(R.Roster, R.CurrentLineup, [Int])]
updateTeamAtIndex teams index newTeamData =
    take index teams ++ [newTeamData] ++ drop (index + 1) teams