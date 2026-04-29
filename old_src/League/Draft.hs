{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}

module Draft where

import Control.Monad (forM, foldM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Maybe (mapMaybe, fromMaybe, fromJust)
import Data.List (find, delete)
import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified PlayerRanking as PR
import Validators (countPlayersOnRoster, findPlayer, queryDraftRosterLmts, queryLgLineupLmts)
import Utility

-- Configuration and state data structures confined to draft
data DraftConfig = DraftConfig {
    cfg :: C.Configuration,
    officialPlayers :: [O.OfficialPlayer]
}

data DraftState = DraftState {
    teams :: [R.LgManager],
    availablePlayerIds :: [O.PlayerID],
    draftHistory :: [(C.TeamID, O.PlayerID)],
    currentTeamIndex :: Int,
    draftOrder :: [(C.TeamID, Int)],
    draftComplete :: Bool,
    teamRankings :: [PR.RankingData]
} deriving (Show, Eq)

instantiateDraft :: C.Configuration -> O.OfficialRoster -> [PR.RankingData] -> IO DraftState
instantiateDraft config players rankings = do
    let teamIds = C.teamId config
        teamRankings = filter (\r -> PR.teamId r `elem` teamIds) rankings
    draftOrder <- generateDraftOrder config rankings
    let teams = map (\tid -> R.LgManager "active" (C.commissioner config) tid (C.leagueID config) mkEmptyLineup mkEmptyRoster) teamIds
    return DraftState {
        teams = teams,
        availablePlayerIds = map O.playerId $ O.people players,
        draftHistory = [],
        currentTeamIndex = 0,
        draftOrder = draftOrder,
        draftComplete = False,
        teamRankings = teamRankings
    }

draftPlayers :: DraftConfig -> DraftState -> IO DraftState
draftPlayers config state
    | draftComplete state = return state
    | otherwise = do
        let currentTeamOrder = draftOrder state !! currentTeamIndex state
            teamId = fst currentTeamOrder
        let (newState, maybeError) = draftCycle config state teamId
        case maybeError of
            Just err -> putStrLn ("Error: " ++ err) >> return state
            Nothing -> draftPlayers config newState

updateState :: DraftConfig -> DraftState -> O.OfficialPlayer -> C.TeamID -> DraftState
updateState config state player teamId = 
    let team = fromJust $ find (\t -> C.unwrapTeamID (R.teamId t) == C.unwrapTeamID teamId) (teams state)
        (newRosters, isNewPlayer) = addToRosterAndLineup (cfg config) player (R.roster team) (R.current_lineup team)
        newDraftHistory = (teamId, O.playerId player) : draftHistory state
        newAvailablePlayerIds = delete (O.playerId player) (availablePlayerIds state)
        newCurrentTeamIndex = (currentTeamIndex state + 1) `mod` length (draftOrder state)
        newTeams = map (\t -> if C.unwrapTeamID (R.teamId t) == C.unwrapTeamID teamId 
                              then t { R.roster = fst newRosters, R.current_lineup = snd newRosters } 
                              else t) (teams state)
    in if isNewPlayer then state { teams = newTeams, draftHistory = newDraftHistory, availablePlayerIds = newAvailablePlayerIds, currentTeamIndex = newCurrentTeamIndex }
       else state

draftCycle :: DraftConfig -> DraftState -> C.TeamID -> (DraftState, Maybe String)
draftCycle config state teamId =
    case lookup teamId [(tid, t) | t@(R.LgManager _ _ tid _ _ _) <- teams state] of
        Just teamState ->
            let availablePlayers = filter (\p -> O.playerId p `elem` availablePlayerIds state) (officialPlayers config)
                maybePlayer = selectNextPlayer teamId (teamRankings state) availablePlayers
            in case maybePlayer of
                Nothing -> (state { draftComplete = True }, Nothing)
                Just player -> 
                    let newState = updateState config state player teamId
                    in if newState == state
                       then (state, Just "Failed to add player to roster or lineup.")
                       else (newState, Nothing)
        Nothing -> (state, Just "Team not found")

runDraftCycle :: DraftConfig -> DraftState -> R.LgManager -> (DraftState, Maybe String)
runDraftCycle config state teamState = 
    draftCycle config state (R.teamId teamState)

selectNextPlayer :: C.TeamID -> [PR.RankingData] -> [O.OfficialPlayer] -> Maybe O.OfficialPlayer
selectNextPlayer teamId rankings availablePlayers =
    let teamRanking = find (\r -> PR.teamId r == teamId) rankings
        rankedPlayerIds = maybe [] (map PR.playerId . PR.rankings) teamRanking
    in find (\p -> O.playerId p `elem` rankedPlayerIds) availablePlayers

addToRosterAndLineup :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.CurrentLineup -> ((R.Roster, R.CurrentLineup), Bool)
addToRosterAndLineup config player roster lineup =
    let positionText = O.primaryPosition player
        draftPositionText = positionCodeToDraftText positionText
        draftLimits = C.draft_limits $ C.draft_parameters config
        lgLineupLimits = C.lineup_limits $ C.point_parameters config
    in if draftPositionText == "pitcher"
       then 
           let (updatedRoster, pitcherPosition) = addPitcherToRoster config player roster
               updatedLineup = if pitcherPosition /= ""
                               then addPlayerToLineup pitcherPosition player lineup lgLineupLimits
                               else lineup
           in ((updatedRoster, updatedLineup), True)
       else 
           let (updatedRoster, isAddedToRoster) = addBatterToRoster config draftPositionText player roster draftLimits
               updatedLineup = if isAddedToRoster
                               then addPlayerToLineup draftPositionText player lineup lgLineupLimits
                               else lineup
           in ((updatedRoster, updatedLineup), isAddedToRoster)

addPlayerToLineup :: T.Text -> O.OfficialPlayer -> R.CurrentLineup -> C.LgLineupLmts -> R.CurrentLineup
addPlayerToLineup position player lineup limits =
    let playerIdText = O.playerId player
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

addPlayerToPosition :: T.Text -> O.OfficialPlayer -> R.Roster -> R.Roster
addPlayerToPosition position player roster =
    let playerIdText = O.playerId player
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
        _ -> roster

addPitcherToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> (R.Roster, T.Text)
addPitcherToRoster config player roster =
    let spLimit = queryDraftRosterLmts "s_pitcher" $ C.draft_limits $ C.draft_parameters config
        rpLimit = queryDraftRosterLmts "r_pitcher" $ C.draft_limits $ C.draft_parameters config
        spCount = length $ R.spR roster
        rpCount = length $ R.rpR roster
    in if spCount < spLimit
       then (addPlayerToPosition "s_pitcher" player roster, "s_pitcher")
       else if rpCount < rpLimit
            then (addPlayerToPosition "r_pitcher" player roster, "r_pitcher")
            else (roster, "")

addBatterToRoster :: C.Configuration -> T.Text -> O.OfficialPlayer -> R.Roster -> C.DraftRosterLmts -> (R.Roster, Bool)
addBatterToRoster config position player roster limits =
    let playerIdText = T.pack . show $ O.playerId player
        currentCount = countPlayersOnRoster position roster
        limit = queryDraftRosterLmts position limits
    in if currentCount < limit
       then (addPlayerToPosition position player roster, True)
       else (roster, False)
