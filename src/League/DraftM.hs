{-# LANGUAGE FlexibleContexts, DoAndIfThenElse, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}


module DraftM (runDraft) where

import Control.Monad ( forM, foldM )
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson ( FromJSON, ToJSON, decode, encode, withObject, (.:) )
import Data.Time.Clock ( UTCTime, getCurrentTime )
import Data.Time.Format ( formatTime, defaultTimeLocale, formatTime, defaultTimeLocale )
import Data.Maybe ( mapMaybe, fromMaybe )
import Data.Either (fromRight)
import GHC.Generics (Generic)
import Data.List ( find, delete, sortBy, sortOn, findIndex)
import qualified Data.Text as T
import Data.Text as T ( unpack, take )
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BL

import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified PlayerRanking as PR

import Validators
import Utility

type DraftM a = ExceptT DraftError (ReaderT DraftConst (StateT DraftState IO)) a

data DraftConst = DraftConst {
    config :: C.Configuration,
    officialRoster :: O.OfficialRoster, 
    rankings :: [PR.RankingData] 
}

data DraftState = DraftState {
    availableIds :: [O.PlayerID], -- pool of eligible playerIds
    draft_log :: [(C.TeamID, O.PlayerID)], -- a running log draft picks is filled while availableIds shrinks (teamId, O.PlayerID)
    draft_order :: C.DraftOrder,  -- Enhanced understanding of draft order
    draft_rosters :: [R.LgManager] -- Mapping of team to its drafted players
}

data DraftError
  = PlayerNotFound Int
  | PositionFull T.Text
  | RankingNotFound T.Text
  | ConfigNotFound T.Text
  | InvalidTeamIndex Int
  | OtherDraftError String
  deriving (Show, Eq)

updateDraftState :: (DraftState -> DraftState) -> DraftM ()
updateDraftState f = get >>= put . f

-- Utility function to access DraftConst within a DraftM computation
getDraftConst :: DraftM DraftConst
getDraftConst = ask

instantiateDraft :: C.Configuration -> O.OfficialRoster -> [PR.RankingData] -> IO (DraftConst, DraftState)
instantiateDraft config players rankings = do
    let teamIds = C.teamId config
    let validRankings = filter (\r -> PR.teamId r `elem` teamIds) rankings
    draftOrder <- generateDraftOrder config validRankings  -- generateDraftOrder already validates rankings against config
                                                           -- I recognize the redundancy and have decided to leave it in for
                                                           --  what it gains us in extensibility/correctness
    let managers = mkLgManagers config
    let validManagers = filter (\m -> R.teamId m `elem` map PR.teamId validRankings) managers
    
    return ( DraftConst
             { config = config
             , officialRoster = players
             , rankings = validRankings
             },
             DraftState
             { availableIds = map O.playerId $ O.people players
             , draft_order = draftOrder
             , draft_rosters = validManagers
             }
           )


-- Initiates the draft process
startDraft :: C.Configuration -> O.OfficialRoster -> [PR.RankingData] -> IO (Either DraftError DraftState)
startDraft config officialRoster rankings = do
    let (draftConst, initialState) = instantiateDraft config officialRoster rankings
    -- Run the draft within the defined monadic stack
    runExceptT $ evalStateT (runReaderT runDraftCycles draftConst) initialState

runDraftCycles :: DraftM ()
runDraftCycles = do
    draftState <- get
    unless (draftComplete draftState) $ do
        draftCycleM
        runDraftCycles

draftCycleM :: DraftM ()
draftCycleM = do
    DraftConst {config, officialRoster, rankings} <- ask
    state <- get

    let draftOrder = draft_order state
        currentRound = length (draft_log state) `div` length draftOrder + 1
        currentPickIndex = length (draft_log state) `mod` length draftOrder
        currentTeamId = fst (draftOrder !! currentPickIndex)
        teamRankings = fromMaybe [] (lookup currentTeamId rankings)
        nextPlayerId = getNextPlayerId teamRankings (availableIds state)

    case find (\p -> O.playerId p == nextPlayerId) (O.people officialRoster) of
        Just player -> do
            let updatedDraftRosters = updateTeamRosterAndLineup (draft_rosters state) player currentTeamId config
            let remainingIds = delete (O.playerId player) (availableIds state)
            let newDraftLog = draft_log state ++ [(currentTeamId, O.playerId player)]
            put state {availableIds = remainingIds, draft_log = newDraftLog, draft_rosters = updatedDraftRosters}
        Nothing -> throwError $ PlayerNotFound $ O.unwrapPlayerId nextPlayerId

-- Filters rankings for a specific team based on the teamId.
filterTeamRankings :: [PR.RankingData] -> R.LgManager -> [PR.PlayerRanking]
filterTeamRankings rankings team = maybe [] PR.rankings $ find ((== R.teamId team) . PR.teamId) rankings

-- Finds the highest-ranked player that is still available for drafting.
getNextPlayerId :: [PR.PlayerRanking] -> [O.PlayerID] -> O.PlayerID
getNextPlayerId rankings availableIds = head [PR.playerId r | r <- rankings, PR.playerId r `elem` availableIds]

updateTeamRosterAndLineup :: [R.LgManager] -> O.OfficialPlayer -> C.TeamID -> C.Configuration -> [R.LgManager]
updateTeamRosterAndLineup managers player teamId config =
    map updateManager managers
  where
    -- Check if this is the manager to update and update accordingly
    updateManager manager@(LgManager { teamId = managerTeamId, roster, current_lineup })
        | managerTeamId == teamId = 
            let (newRoster, newLineup) = addToRosterAndLineup config player roster current_lineup
            in manager { roster = newRoster, current_lineup = newLineup }
        | otherwise = manager

-- Utilizes the given configuration and player to update the roster and lineup
addToRosterAndLineup :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.CurrentLineup -> (R.Roster, R.CurrentLineup)
addToRosterAndLineup config player roster lineup =
    -- Assuming the player's primaryPosition corresponds to your roster's positions
    let position = primaryPosition player
        -- Modify this function to work with your Configuration and Roster types
        (updatedRoster, updatedLineup) = case position of
            "Pitcher" -> addPitcherToRosterAndLineup config player roster lineup
            _ -> addPlayerToRosterAndLineup config position player roster lineup
    in (updatedRoster, updatedLineup)

sumDraftRosterLmts :: C.DraftRosterLmts -> Int
sumDraftRosterLmts lmts = 
    C.dr_catcher lmts +
    C.dr_first lmts +
    C.dr_second lmts +
    C.dr_third lmts +
    C.dr_shortstop lmts +
    C.dr_outfield lmts +
    C.dr_utility lmts +
    C.dr_s_pitcher lmts +
    C.dr_r_pitcher lmts