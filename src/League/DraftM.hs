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

-- type DraftM a = ExceptT DraftError (ReaderT DraftConst (StateT DraftState IO)) a
type DraftM a = ReaderT DraftConst (StateT DraftState (ExceptT DraftError IO)) a

data DraftConst = DraftConst {
    config :: C.Configuration,
    officialRoster :: O.OfficialRoster, 
    rankings :: [PR.RankingData] 
}

data DraftState = DraftState {
    availableIds :: [O.PlayerID], -- pool of eligible playerIds
    draft_log :: [(C.TeamID, O.PlayerID)], -- a running log draft picks is filled while availableIds shrinks (teamId, O.PlayerID)
    draft_order :: C.DraftOrder,  -- Enhanced understanding of draft order
    draft_rosters :: [R.LgManager], -- Mapping of team to its drafted players
    draftComplete :: Bool -- only true when draft is complete
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
             , draftComplete = False
             }
           )


-- Initiates the draft process
-- This snippet assumes correct monad stack alignment and focuses on the structure.
startDraft :: C.Configuration -> O.OfficialRoster -> [PR.RankingData] -> IO (Either DraftError DraftState)
startDraft config officialRoster rankings = do
    (draftConst, initialState) <- instantiateDraft config officialRoster rankings
    let runDraft :: ReaderT DraftConst (StateT DraftState (ExceptT DraftError IO)) ()
        runDraft = runDraftCycles
    let stateAction :: StateT DraftState (ExceptT DraftError IO) ()
        stateAction = runReaderT runDraft draftConst
    let exceptAction :: ExceptT DraftError IO DraftState
        exceptAction = execStateT stateAction initialState
    runExceptT exceptAction


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
        teamRankings = fromMaybe [] $ lookup currentTeamId (map (\r -> (PR.teamId r, PR.rankings r)) rankings)
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

updateTeamRosterAndLineup :: O.OfficialPlayer -> C.TeamID -> DraftM ()
updateTeamRosterAndLineup player teamId = do
    draftState <- get
    let managers = draft_rosters draftState
        updatedManagers = map (updateManager teamId player) managers
    put $ draftState { draft_rosters = updatedManagers }

-- Helper function to update a single manager's roster and lineup
updateManager :: C.TeamID -> O.OfficialPlayer -> R.LgManager -> R.LgManager
updateManager teamId player manager@(R.LgManager { R.teamId = managerTeamId, R.roster, R.current_lineup })
    | managerTeamId == teamId =
        let position = O.primaryPosition player
        in if position == "Pitcher" then
               addPitcherToRosterAndLineup player manager
           else
               addPlayerToRosterAndLineup position player manager
    | otherwise = manager

-- Utilizes the given configuration and player to update the roster and lineup
addToRosterAndLineup :: O.OfficialPlayer -> DraftM ()
addToRosterAndLineup player = do
    draftConst <- getDraftConst
    let position = O.primaryPosition player
    if position == "Pitcher" then
        addPitcherToRosterAndLineup draftConst.config player -- need to adjust how we pass and manage rosters/lineups.
    else
        addPlayerToRosterAndLineup draftConst.config position player

addPlayerToRoster :: T.Text -> O.OfficialPlayer -> DraftM ()
addPlayerToRoster position player = updateDraftState $ \ds ->
    let rosterUpdate = addPlayerToPosition position player (draft_rosters ds)
    in ds { draft_rosters = rosterUpdate }

-- addPitcherToRosterAndLineup :: O.OfficialPlayer -> R.LgManager -> R.LgManager
-- addPitcherToRosterAndLineup player manager = 
--     -- Implementation details depend on your roster/lineup logic
--     -- This is a placeholder to illustrate the expected structure
--     manager

-- addPlayerToRosterAndLineup :: T.Text -> O.OfficialPlayer -> R.LgManager -> R.LgManager
-- addPlayerToRosterAndLineup position player manager =
--     -- Similarly, this function needs to be implemented based on your logic
--     manager

addPitcherToRosterAndLineup :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.CurrentLineup -> DraftM (R.Roster, R.CurrentLineup)
addPitcherToRosterAndLineup config player roster lineup = do
    draftConst <- getDraftConst
    let spLimit = queryDraftRosterLmts "s_pitcher" $ C.draft_limits $ C.draft_parameters draftConst.config
        rpLimit = queryDraftRosterLmts "r_pitcher" $ C.draft_limits $ C.draft_parameters draftConst.config
        spCount = length $ R.spR roster
        rpCount = length $ R.rpR roster
    if spCount < spLimit then do
        addPlayerToRoster "s_pitcher" player
        return (roster, lineup) -- You will need to properly update this return value based on the actual roster and lineup changes.
    else if rpCount < rpLimit then do
        addPlayerToRoster "r_pitcher" player
        return (roster, lineup) -- Adjust return value as needed.
    else
        return (roster, lineup) -- Return unchanged if limits are reached.
