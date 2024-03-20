{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}


module DraftMonad where

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

-- Define your draft state
data DraftState = DraftState {
  rosters :: [(R.Roster, R.CurrentLineup, [Int])],
  currentPick :: Int
}
type DraftMonad a = ExceptT DraftError (StateT DraftState IO) a

findPlayerMonad :: Int -> [O.OfficialPlayer] -> [Int] -> DraftMonad O.OfficialPlayer
findPlayerMonad playerId players availableIds = case findPlayer playerId players availableIds of
    Just player -> return player
    Nothing -> throwError $ PlayerNotFound playerId

updateTeamState :: [(R.Roster, R.CurrentLineup, [Int])] -> Int -> (R.Roster, R.CurrentLineup, [Int]) -> [(R.Roster, R.CurrentLineup, [Int])]
updateTeamState teams index (newRoster, newLineup, newAvailableIds) =
    take index teams ++ [(newRoster, newLineup, newAvailableIds)] ++ drop (index + 1) teams

-- Draft players for each team, given a list of player rankings for each team
draftPlayersMonad :: [[PR.PlayerRanking]] -> DraftMonad ()
draftPlayersMonad teamRankings = do
  let numTeams = length teamRankings
  draftOrder <- serpentineOrder numTeams (length (head teamRankings)) -- Adjust based on actual rounds
  mapM_ draftCycleMonad (zip draftOrder teamRankings)

-- Handle drafting for one cycle
draftCycleMonad :: ([Int], [PR.PlayerRanking]) -> DraftMonad ()
draftCycleMonad (order, teamRankings) = do
  forM_ (zip order teamRankings) $ \(teamIndex, playerRanking) -> do
    draftState <- get
    let (roster, lineup, availableIds) = (rosters draftState) !! teamIndex
    player <- findPlayerMonad (PR.playerId playerRanking) -- Adjust to pass players and availableIds
    -- Additional logic to add player to roster and lineup, update state accordingly
    -- Consider splitting into more utility functions as needed
    put $ draftState { rosters = updateTeamState (rosters draftState) teamIndex (newRoster, newLineup, newAvailableIds) }
    where
      -- Define updateTeamState to replace the state of a specific team in the list
      updateTeamState :: [(R.Roster, R.CurrentLineup, [Int])] -> Int -> (R.Roster, R.CurrentLineup, [Int]) -> [(R.Roster, R.CurrentLineup, [Int])]
      updateTeamState teams index newState = take index teams ++ [newState] ++ drop (index + 1) teams

-- serpentine order generator
serpentineOrder :: Int -> Int -> DraftMonad [[Int]]
serpentineOrder numTeams rounds = return $ concatMap generateOrder [1..rounds]
  where
    generateOrder round = if even round then [1..numTeams] else reverse [1..numTeams]

addToRosterAndLineupMonad :: C.Configuration -> O.OfficialPlayer -> DraftMonad ()
addToRosterAndLineupMonad config player = do
    draftState <- get
    let teamIndex = currentPick draftState `mod` length (rosters draftState) -- Determine which team is picking
        (roster, lineup, availableIds) = (rosters draftState) !! teamIndex
        positionText = O.primaryPosition player
        draftPositionText = positionCodeToDraftText positionText
    if draftPositionText == "pitcher"
      then do
          updatedRoster <- addPitcherToRosterMonad config player roster
          updatedLineup <- addPlayerToLineupMonad draftPositionText player lineup
          let newState = updateTeamState (rosters draftState) teamIndex (updatedRoster, updatedLineup, delete (O.playerId player) availableIds)
          put $ draftState { rosters = newState }
      else do
          (updatedRoster, isAddedToRoster) <- addBatterToRosterMonad config draftPositionText player roster
          updatedLineup <- if isAddedToRoster then addPlayerToLineupMonad draftPositionText player lineup else return lineup
          let newState = updateTeamState (rosters draftState) teamIndex (updatedRoster, updatedLineup, delete (O.playerId player) availableIds)
          put $ draftState { rosters = newState }

addPitcherToRosterMonad :: C.Configuration -> O.OfficialPlayer -> R.Roster -> DraftMonad R.Roster
addPitcherToRosterMonad config player roster = do
    let spLimit = queryDraftRosterLmt "s_pitcher" $ C.draft_limits $ C.draft_parameters config
        rpLimit = queryDraftRosterLmt "r_pitcher" $ C.draft_limits $ C.draft_parameters config
        spCount = length $ R.spR roster
        rpCount = length $ R.rpR roster
    if spCount < spLimit then
        return $ addPlayerToPosition "s_pitcher" player roster
    else if rpCount < rpLimit then
        return $ addPlayerToPosition "r_pitcher" player roster
    else
        throwError $ PositionFull "Pitcher positions are full"

addBatterToRosterMonad :: C.Configuration -> T.Text -> O.OfficialPlayer -> R.Roster -> DraftMonad (R.Roster, Bool)
addBatterToRosterMonad config position player roster = do
    let currentCount = countPlayers position roster
        limit = queryDraftRosterLmt position $ C.draft_limits $ C.draft_parameters config
    if currentCount < limit then 
        return (addPlayerToPosition position player roster, True)
    else 
        return (roster, False)


addPlayerToLineupMonad :: T.Text -> O.OfficialPlayer -> R.CurrentLineup -> C.LgRosterLmts -> DraftMonad R.CurrentLineup
addPlayerToLineupMonad position player lineup limits = do
    let playerIdText = T.pack . show $ O.playerId player
    updatedLineup <- case position of
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
    return updatedLineup
  where
    updateLineup currentPos limitPos accessor updater playerIdText =
      if length currentPos < limitPos
      then return $ updater (playerIdText : accessor lineup)
      else throwError $ PositionFull $ "Position " <> position <> " is full"

-- Add necessary utility functions here, adapted to work within DraftMonad
-- For instance, adding players to rosters/lineups, checking position limits, etc.
