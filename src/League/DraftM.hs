{-# LANGUAGE FlexibleContexts, DoAndIfThenElse #-}
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

data DraftConst = DraftConst {
    config :: C.Configuration,
    officialRoster :: O.OfficialRoster, 
    rankings :: [PR.RankingData] 
}

data DraftState = DraftState {
    availableIds :: [Int], -- pool of eligible playerId's
    currentPick :: Int,
    currentRound :: Int, -- to facilitate serpentine draft
    draft_rosters :: [(R.LgManager, [Int])] -- teams with indices
}

-- take both DraftConst and DraftState, then run the action with both
runDraft :: DraftConst -> DraftState -> DraftM a -> IO (Either DraftError a, DraftState)
runDraft draftConst initialState action = runStateT (runReaderT (runExceptT action) draftConst) initialState

-- DraftM include ReaderT for carrying DraftConst
type DraftM a = ExceptT DraftError (ReaderT DraftConst (StateT DraftState IO)) a

updateDraftState :: (DraftState -> DraftState) -> DraftM ()
updateDraftState f = get >>= put . f

-- Error handling
data DraftError
  = PlayerNotFound Int
  | PositionFull T.Text
  | RankingNotFound T.Text
  | ConfigNotFound T.Text
  | InvalidTeamIndex Int
  | OtherDraftError String
  deriving (Show, Eq)

-- Utility function to access DraftConst within a DraftM computation
getDraftConst :: DraftM DraftConst
getDraftConst = ask

initializeDraftEnv :: C.Configuration -> O.OfficialRoster -> [PR.RankingData] -> (DraftConst, DraftState)
initializeDraftEnv config validPlayers rankings = 
    ( DraftConst
        { config = config
        , officialRoster = validPlayers
        , rankings = filterInvalidRankings (C.lgMembers config) rankings
        }
    , DraftState
        { availableIds = map O.playerId $ O.people validPlayers
        , currentPick = 0
        , currentRound = 1
        , draft_rosters = zip (mkLgManagers config) (repeat [])
        }
    )

-- Filter function to retain only those rankings where the teamId matches any lgMember
filterInvalidRankings :: [T.Text] -> [PR.RankingData] -> [PR.RankingData]
filterInvalidRankings lgMembers rankings =
  filter (\ranking -> PR.teamId ranking `elem` lgMembers) rankings

initializeRosters :: Int -> [(R.Roster, R.CurrentLineup, [Int])]
initializeRosters numTeams = replicate numTeams (mkEmptyRoster, mkEmptyLineup, [])



-- -- serpentine order generator
-- serpentineOrder :: Int -> Int -> DraftM [[Int]]
-- serpentineOrder numTeams rounds = return $ map generateOrder [1..rounds]
--   where
--     generateOrder round = if even round then reverse [1..numTeams] else [1..numTeams]

-- getTeamByIndexSafe :: Int -> DraftM (R.LgManager, [Int])
-- getTeamByIndexSafe index = do
--   draftState <- get
--   let teams = draft_rosters draftState
--   maybe (throwError $ InvalidTeamIndex index) return (atMay teams index)
--   where
--     atMay :: [a] -> Int -> Maybe a
--     atMay xs n = if n >= 0 && n < length xs then Just (xs !! n) else Nothing

-- findPlayerM :: Int -> [O.OfficialPlayer] -> [Int] -> DraftM O.OfficialPlayer
-- findPlayerM playerId players availableIds = case findPlayer playerId players availableIds of
--     Just player -> return player
--     Nothing -> throwError $ PlayerNotFound playerId

-- updateTeamState :: [(R.LgManager, [Int])] -> Int -> (R.LgManager, [Int]) -> [(R.LgManager, [Int])]
-- updateTeamState teams index (newRoster, newLineup, newAvailableIds) =
--     Prelude.take index teams ++ [(newRoster, newLineup, newAvailableIds)] ++ drop (index + 1) teams

-- draftPlayersM :: [[PR.PlayerRanking]] -> DraftM ()
-- draftPlayersM teamRankings = do
--   when (null teamRankings) $
--     throwError $ OtherDraftError "Team rankings cannot be empty."
--   let numTeams = length teamRankings
--   draftOrder <- serpentineOrder numTeams (length $ head teamRankings)
--   mapM_ draftCycleM (zip draftOrder teamRankings)

-- draftCycleM :: ([Int], [PR.PlayerRanking]) -> DraftM ()
-- draftCycleM (order, teamRankings) = do
--   draftState <- get
--   -- Assuming rankings are now handled at a higher level to align with LgManager updates.
--   forM_ order $ \teamIndex -> do
--     -- Ensure we're within bounds and have valid rankings.
--     when (teamIndex < length (draft_rosters draftState)) $ do
--       let (lgManager, availableIds) = draft_rosters draftState !! teamIndex
--       -- Placeholder: Implement logic to select a player based on rankings and availability.
--       let selectedPlayerId = head availableIds -- Simplified for illustration.
--       player <- findPlayerM selectedPlayerId (O.people $ officialRoster draftState) availableIds
--       -- Update LgManager with selected player.
--       let updatedLgManager = updateLgManagerWithPlayer lgManager player
--       let newAvailableIds = delete selectedPlayerId availableIds
--       updateTeamInDraftState teamIndex (updatedLgManager, newAvailableIds)

-- -- Function to update a single team's data in the draft state
-- updateTeamDataInState :: Int -> (R.LgManager, [Int]) -> DraftM ()
-- updateTeamDataInState teamIndex updatedTeamData = modify' $ \ds ->
--   let updatedTeams = updateTeamAtIndex (draft_rosters ds) teamIndex updatedTeamData
--   in ds { draft_rosters = updatedTeams }

-- -- Function to safely update the list of teams at a specific index
-- updateTeamAtIndex :: [(R.LgManager, [Int])] -> Int -> (R.LgManager, [Int]) -> [(R.LgManager, [Int])]
-- updateTeamAtIndex teams index newTeamData =
--   let (before, after) = splitAt index teams
--   in before ++ [newTeamData] ++ drop 1 after
-- addToRosterAndLineupM :: O.OfficialPlayer -> DraftM ()
-- addToRosterAndLineupM player = do
--   draftState <- get
--   let teamIndex = currentPick draftState `mod` length (draft_rosters draftState)
--   (currentRoster, currentLineup, currentAvailableIds) <- getTeamByIndexSafe teamIndex

--   let positionText = O.primaryPosition player
--       draftPositionText = positionCodeToDraftText positionText

--   -- Process player addition based on the position
--   (updatedRoster, updatedLineup, newAvailableIds) <- case draftPositionText of
--       "pitcher" -> do
--         updatedRoster <- addPitcherToRosterM player currentRoster
--         updatedLineup <- addPlayerToLineupM draftPositionText player currentLineup
--         let newAvailableIds = delete (O.playerId player) currentAvailableIds
--         return (updatedRoster, updatedLineup, newAvailableIds)
--       position -> do
--         (updatedRoster, added) <- addBatterToRosterM position player currentRoster
--         updatedLineup <- if added then addPlayerToLineupM position player currentLineup else pure currentLineup
--         let newAvailableIds = if added then delete (O.playerId player) currentAvailableIds else currentAvailableIds
--         return (updatedRoster, updatedLineup, newAvailableIds)

--   -- Update state with new draft_rosters, lineups, and available IDs
--   let updatedTeams = updateTeamState (draft_rosters draftState) teamIndex (updatedRoster, updatedLineup, newAvailableIds)
--   put draftState { draft_rosters = updatedTeams, availableIds = newAvailableIds }


-- addPitcherToRosterM :: O.OfficialPlayer -> R.Roster -> DraftM R.Roster
-- addPitcherToRosterM player roster = do
--   draftState <- get
--   let configData = config draftState  -- Use a different name to avoid shadowing
--       spLimit = queryDraftRosterLmts "s_pitcher" $ C.draft_limits $ C.draft_parameters configData
--       rpLimit = queryDraftRosterLmts "r_pitcher" $ C.draft_limits $ C.draft_parameters configData
--       spCount = length $ R.spR roster
--       rpCount = length $ R.rpR roster
--   if spCount < spLimit then
--       addPlayerToPositionM "s_pitcher" player roster
--   else if rpCount < rpLimit then
--       addPlayerToPositionM "r_pitcher" player roster
--   else
--       throwError $ PositionFull "Pitcher positions are full"

-- addBatterToRosterM :: T.Text -> O.OfficialPlayer -> R.Roster -> DraftM (R.Roster, Bool)
-- addBatterToRosterM position player roster = do
--   draftState <- get
--   let configData = config draftState
--       currentCount = countPlayersOnRoster position roster
--       limit = queryDraftRosterLmts position $ C.draft_limits $ C.draft_parameters configData
--   if currentCount < limit then do
--       updatedRoster <- addPlayerToPositionM position player roster
--       return (updatedRoster, True)
--   else
--       return (roster, False)

-- addPlayerToLineupM :: T.Text -> O.OfficialPlayer -> R.CurrentLineup -> DraftM R.CurrentLineup
-- addPlayerToLineupM position player lineup = do
--     draftState <- get
--     let playerIdText = T.pack . show $ O.playerId player
--         limits = C.lineup_limits $ C.point_parameters (config draftState)
--         -- the specific limit for the player's position
--         limit = queryLgLineupLmts position limits

--     case position of
--         "catcher" -> updateLineup limit (R.cC lineup) R.cC (\l -> lineup{R.cC = l}) playerIdText
--         "first" -> updateLineup limit (R.b1C lineup) R.b1C (\l -> lineup{R.b1C = l}) playerIdText
--         "second" -> updateLineup limit (R.b2C lineup) R.b2C (\l -> lineup{R.b2C = l}) playerIdText
--         "third" -> updateLineup limit (R.b3C lineup) R.b3C (\l -> lineup{R.b3C = l}) playerIdText
--         "shortstop" -> updateLineup limit (R.ssC lineup) R.ssC (\l -> lineup{R.ssC = l}) playerIdText
--         "outfield" -> updateLineup limit (R.ofC lineup) R.ofC (\l -> lineup{R.ofC = l}) playerIdText
--         "utility" -> updateLineup limit (R.uC lineup) R.uC (\l -> lineup{R.uC = l}) playerIdText
--         "s_pitcher" -> updateLineup limit (R.spC lineup) R.spC (\l -> lineup{R.spC = l}) playerIdText
--         "r_pitcher" -> updateLineup limit (R.rpC lineup) R.rpC (\l -> lineup{R.rpC = l}) playerIdText
--         _ -> throwError $ OtherDraftError $ "Unknown position " <> T.unpack position
--   where
--     updateLineup limit currentPos accessor updater playerIdText =
--         if length currentPos < limit
--         then return $ updater (playerIdText : accessor lineup)
--         else throwError $ PositionFull $ "Position " <> position <> " is full"

-- addPlayerToPositionM :: T.Text -> O.OfficialPlayer -> R.Roster -> DraftM R.Roster
-- addPlayerToPositionM position player roster = do
--   let playerIdText = T.pack $ show $ O.playerId player
--   return $ case position of
--     "s_pitcher" -> roster { R.spR = playerIdText : R.spR roster }
--     "r_pitcher" -> roster { R.rpR = playerIdText : R.rpR roster }
--     "catcher" -> roster { R.cR = playerIdText : R.cR roster }
--     "first" -> roster { R.b1R = playerIdText : R.b1R roster }
--     "second" -> roster { R.b2R = playerIdText : R.b2R roster }
--     "third" -> roster { R.b3R = playerIdText : R.b3R roster }
--     "shortstop" -> roster { R.ssR = playerIdText : R.ssR roster }
--     "outfield" -> roster { R.ofR = playerIdText : R.ofR roster }
--     "utility" -> roster { R.uR = playerIdText : R.uR roster }
--     _ -> roster  -- Default case if position does not match

-- -- Helper function to determine if a player can be added based on position limits
-- canAddPlayerToPosition :: T.Text -> R.Roster -> C.Configuration -> Bool
-- canAddPlayerToPosition position roster config =
--   let limit = queryDraftRosterLmts position $ C.draft_limits $ C.draft_parameters config
--       currentCount = countPlayersOnRoster position roster
--   in currentCount < limit

-- updateRosterAndLineup :: Int -> R.LgManager -> [Int] -> DraftM ()
-- updateRosterAndLineup teamIndex updatedRoster updatedLineup newAvailableIds = do
--   updateDraftStateWithNewTeamData teamIndex updatedRoster updatedLineup newAvailableIds

-- updateDraftStateWithNewTeamData :: Int -> R.LgManager -> [Int] -> DraftM ()
-- updateDraftStateWithNewTeamData teamIndex updatedRoster updatedLineup newAvailableIds = modify' $ \ds ->
--   let updatedTeams = Prelude.take teamIndex (draft_rosters ds) ++
--                      [(updatedRoster, updatedLineup, newAvailableIds)] ++
--                      drop (teamIndex + 1) (draft_rosters ds)
--   in ds { draft_rosters = updatedTeams, availableIds = newAvailableIds }

-- updateDraftStateWithTeam :: Int -> R.LgManager -> [Int] -> DraftM ()
-- updateDraftStateWithTeam teamIndex updatedRoster updatedLineup newAvailableIds = updateDraftState $ \ds ->
--     let updatedTeams = updateTeamAtIndex (draft_rosters ds) teamIndex (updatedRoster, updatedLineup, newAvailableIds)
--     in ds { draft_rosters = updatedTeams, availableIds = newAvailableIds }

