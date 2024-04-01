{-# LANGUAGE FlexibleContexts, DoAndIfThenElse, NamedFieldPuns, RecordWildCards #-}
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
    availableIds :: [O.PlayerID], -- pool of eligible playerId's
    draftComplete :: Bool,
    currentPick :: Int,
    currentRound :: Int, -- to facilitate serpentine draft
    draft_rosters :: [(R.LgManager, [Int])] -- teams with indices
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

initializeDraftEnv :: C.Configuration -> O.OfficialRoster -> [PR.RankingData] -> (DraftConst, DraftState)
initializeDraftEnv config validPlayers rankings = 
    let validTeamIds = filterValidTeams (C.lgMembers config) rankings
        -- Ensure mkLgManagers respects the filtered list of valid teams
        managers = mkLgManagersWithFilter config validTeamIds -- Adjusted to filter based on validTeamIds
        rosterLimits = C.draft_limits (C.draft_parameters config)
        totalRosterSlots = sumDraftRosterLmts rosterLimits
        numTeams = length validTeamIds -- Use the length of validTeamIds
        numRounds = if numTeams > 0 then totalRosterSlots `div` numTeams else 0
    in ( DraftConst
        { config = config
        , officialRoster = validPlayers
        , rankings = filterInvalidRankings validTeamIds rankings -- Use filtered list
        },
        DraftState
        { availableIds = map O.playerId $ O.people validPlayers
        , currentPick = 0
        , draftComplete = False
        , currentRound = 1
        , draft_rosters = zip managers (repeat [])
        }
    )

-- Initiates the draft process
startDraft :: C.Configuration -> O.OfficialRoster -> [PR.RankingData] -> IO (Either DraftError DraftState)
startDraft config officialRoster rankings = do
    let (draftConst, initialState) = initializeDraftEnv config officialRoster rankings
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
    state@DraftState {availableIds, currentPick, currentRound, draft_rosters} <- get

    let isTeam1Turn = determineTurn currentPick currentRound
        teamRankings = if isTeam1Turn then filterTeamRankings rankings (fst $ head draft_rosters) else filterTeamRankings rankings (fst $ head $ tail draft_rosters)
        nextPlayerId = getNextPlayerId teamRankings availableIds

    case find (\p -> O.playerId p == nextPlayerId) (O.people officialRoster) of
        Just player -> do
            -- Determine the team and update its roster and lineup
            let updatedDraftRosters = updateTeamRosterAndLineup draft_rosters player isTeam1Turn config
            -- Remove drafted player from available pool
            let remainingIds = delete nextPlayerId availableIds
            -- Update the state with the changes
            put state {availableIds = remainingIds, currentPick = currentPick + 1, draft_rosters = updatedDraftRosters}
        Nothing -> throwError $ PlayerNotFound $ fromIntegral $ O.unwrapPlayerId nextPlayerId

-- Determines which team's turn it is to pick based on the current pick and round, accounting for serpentine order.
determineTurn :: Int -> Int -> Bool
determineTurn currentPick currentRound = (currentRound `mod` 2 == 1 && currentPick `mod` 2 == 1) || (currentRound `mod` 2 == 0 && currentPick `mod` 2 == 0)

-- Filters rankings for a specific team based on the teamId.
filterTeamRankings :: [PR.RankingData] -> R.LgManager -> [PR.PlayerRanking]
filterTeamRankings rankings team = maybe [] PR.rankings $ find ((== R.teamId team) . PR.teamId) rankings

-- Finds the highest-ranked player that is still available for drafting.
getNextPlayerId :: [PR.PlayerRanking] -> [O.PlayerID] -> O.PlayerID
getNextPlayerId rankings availableIds = head [PR.playerId r | r <- rankings, PR.playerId r `elem` availableIds]

-- Updates the roster and lineup for the team that is currently picking.
updateTeamRosterAndLineup :: [(R.LgManager, [Int])] -> O.OfficialPlayer -> Bool -> C.Configuration -> [(R.LgManager, [Int])]
updateTeamRosterAndLineup draft_rosters player isTeam1Turn config =
    -- Placeholder logic to demonstrate updating the first team's roster and lineup.
    -- You need to implement logic to correctly select the team and update its roster and lineup based on the player drafted and configuration settings.
    draft_rosters  -- This should be replaced with actual logic to update rosters.


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