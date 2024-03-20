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
    ( positionCodeToDraftText, extendRankingsWithUnrankedPlayers, createLgManager )


draftPlayers :: [[PR.PlayerRanking]] -> [O.OfficialPlayer] -> C.Configuration -> IO [(R.Roster, R.CurrentLineup, [Int])]
draftPlayers teamRankings officialPlayers config = do
    let officialPlayerIds = map O.playerId officialPlayers
        extendedTeamRankings = map (`extendRankingsWithUnrankedPlayers` officialPlayerIds) teamRankings
        draftOrder = serpentineOrder $ length teamRankings
        initialState = replicate (length teamRankings) (R.mkEmptyRoster, R.mkEmptyLineup, officialPlayerIds)
        
    foldM (draftCycle config officialPlayers) initialState (zip draftOrder extendedTeamRankings)
    
serpentineOrder :: Int -> [[Int]]
serpentineOrder numTeams = let baseOrder = [0..numTeams - 1]
                               orders = map (\n -> if even n then baseOrder else reverse baseOrder) [0..numTeams-1]
                           in orders

draftCycle :: C.Configuration -> [O.OfficialPlayer] -> [(R.Roster, R.CurrentLineup, [Int])] -> ([Int], [PR.PlayerRanking]) -> IO [(R.Roster, R.CurrentLineup, [Int])]
draftCycle config officialPlayers teamStates (order, teamRankings) = do
    -- Process each team's pick in the current order
    foldM (processPick officialPlayers config) teamStates (zip order teamRankings)

processPick :: [O.OfficialPlayer] -> C.Configuration -> [(R.Roster, R.CurrentLineup, [Int])] -> (Int, PR.PlayerRanking) -> IO [(R.Roster, R.CurrentLineup, [Int])]
processPick officialPlayers config teamStates (teamIndex, playerRanking) = do
    let (roster, lineup, availablePlayers) = teamStates !! teamIndex
        maybePlayer = findPlayer (PR.playerId playerRanking) officialPlayers availablePlayers
    case maybePlayer of
        Just player -> do
            let (updatedRoster, updatedLineup) = addToRosterAndLineup config player roster lineup
                newAvailablePlayers = delete (O.playerId player) availablePlayers
            return $ updateTeamState teamStates teamIndex (updatedRoster, updatedLineup, newAvailablePlayers)
        Nothing -> return teamStates -- If player is not found or not available

updateTeamState :: [(R.Roster, R.CurrentLineup, [Int])] -> Int -> (R.Roster, R.CurrentLineup, [Int]) -> [(R.Roster, R.CurrentLineup, [Int])]
updateTeamState teamStates index newState = 
    take index teamStates ++ [newState] ++ drop (index + 1) teamStates

addToRosterAndLineup :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.CurrentLineup -> (R.Roster, R.CurrentLineup)
addToRosterAndLineup config player roster lineup =
    let positionText = O.primaryPosition player
        draftPositionText = positionCodeToDraftText positionText
        draftLimits = C.draft_limits $ C.draft_parameters config
        lgRosterLimits = C.valid_roster $ C.point_parameters config
    in if draftPositionText == "pitcher"
       then 
           let (updatedRoster, pitcherPosition) = addPitcherToRoster config player roster
               updatedLineup = if pitcherPosition /= ""
                               then addPlayerToLineup pitcherPosition player lineup lgRosterLimits
                               else lineup
           in (updatedRoster, updatedLineup)
       else 
           let (updatedRoster, isAddedToRoster) = addBatterToRoster config draftPositionText player roster draftLimits
               updatedLineup = if isAddedToRoster
                               then addPlayerToLineup draftPositionText player lineup lgRosterLimits
                               else lineup
           in (updatedRoster, updatedLineup)

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

addPitcherToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> (R.Roster, T.Text)
addPitcherToRoster config player roster =
  let spLimit = queryDraftRosterLmt "s_pitcher" $ C.draft_limits $ C.draft_parameters config
      rpLimit = queryDraftRosterLmt "r_pitcher" $ C.draft_limits $ C.draft_parameters config
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
        currentCount = countPlayers position roster
        limit = queryDraftRosterLmt position limits
    in if currentCount < limit
       then (addPlayerToPosition position player roster, True)
       else (roster, False)       