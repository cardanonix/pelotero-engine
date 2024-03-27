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
import Data.Maybe (mapMaybe, fromMaybe, fromJust)
import Data.List (find, delete, sortOn, sortBy, findIndex, sortOn, findIndex, sortOn, findIndex, sortOn, findIndex)
import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified PlayerRanking as PR
import Validators ( countPlayersOnRoster, findPlayer, queryDraftRosterLmts, queryLgLineupLmts )
import Utility


draftPlayers :: [PR.PlayerRanking] -> [PR.PlayerRanking] -> [O.OfficialPlayer] -> C.Configuration -> IO ((R.Roster, R.CurrentLineup), (R.Roster, R.CurrentLineup))
draftPlayers rankings1 rankings2 officialPlayers config = do
  let officialPlayerIds = map O.playerId officialPlayers
      rankingsPairs = zip rankings1 rankings2
      initialTurn = True
      initialState = (mkEmptyRoster, mkEmptyLineup, mkEmptyRoster, mkEmptyLineup, officialPlayerIds, initialTurn)

  -- Correct the structure of rankingsPairs to match the expected input for foldM
  let rankingsPairsWithTurn = map (\pair -> (pair, initialTurn)) rankingsPairs

  (finalRoster1, finalLineup1, finalRoster2, finalLineup2, _, _) <- foldM (draftCycle config officialPlayers) initialState rankingsPairsWithTurn

  return ((finalRoster1, finalLineup1), (finalRoster2, finalLineup2))


findAndRemovePlayer :: O.PlayerID -> [O.OfficialPlayer] -> [O.PlayerID] -> (Maybe O.OfficialPlayer, [O.PlayerID])
findAndRemovePlayer playerId officialPlayers availablePlayers =
  let playerFound = find (\player -> O.playerId player == playerId) officialPlayers
      newAvailablePlayers = filter (/= playerId) availablePlayers
  in (playerFound, newAvailablePlayers)


draftCycle :: C.Configuration -> [O.OfficialPlayer] -> (R.Roster, R.CurrentLineup, R.Roster, R.CurrentLineup, [O.PlayerID], Bool)
           -> ((PR.PlayerRanking, PR.PlayerRanking), Bool) -> IO (R.Roster, R.CurrentLineup, R.Roster, R.CurrentLineup, [O.PlayerID], Bool)
draftCycle config officialPlayers (roster1, lineup1, roster2, lineup2, availablePlayers, isTeam1Turn) ((ranking1, ranking2), _) = do
  let selectedRanking = if isTeam1Turn then ranking1 else ranking2
      playerIdToDraft = PR.playerId selectedRanking
      maybePlayerToDraft = find (\player -> O.playerId player == playerIdToDraft) officialPlayers

  case maybePlayerToDraft of
    Just player -> 
      if playerIdToDraft `elem` availablePlayers then do
        let updateFn = addToRosterAndLineup config player
            updatedAvailablePlayers = delete playerIdToDraft availablePlayers
            (newRoster1, newLineup1) = if isTeam1Turn then updateFn roster1 lineup1 else (roster1, lineup1)
            (newRoster2, newLineup2) = if not isTeam1Turn then updateFn roster2 lineup2 else (roster2, lineup2)

        putStrLn $ "Drafting player with ID: " ++ show playerIdToDraft
        return (newRoster1, newLineup1, newRoster2, newLineup2, updatedAvailablePlayers, not isTeam1Turn)
      else
        -- It's not this player's turn or they've been drafted; don't change the state.
        return (roster1, lineup1, roster2, lineup2, availablePlayers, not isTeam1Turn)

    Nothing -> do
      -- The player was not found in the officialPlayers list; this should not happen if data is consistent.
      putStrLn $ "Player with ID: " ++ show playerIdToDraft ++ " not found in official players list."
      return (roster1, lineup1, roster2, lineup2, availablePlayers, isTeam1Turn)


addToRosterAndLineup :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.CurrentLineup -> (R.Roster, R.CurrentLineup)
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
           in (updatedRoster, updatedLineup)
       else 
           let (updatedRoster, isAddedToRoster) = addBatterToRoster config draftPositionText player roster draftLimits
               updatedLineup = if isAddedToRoster
                               then addPlayerToLineup draftPositionText player lineup lgLineupLimits
                               else lineup
           in (updatedRoster, updatedLineup)

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
       _ -> roster -- Default case if position does not match

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
