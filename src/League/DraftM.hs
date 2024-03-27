{-# LANGUAGE FlexibleContexts, DoAndIfThenElse #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE RecordWildCards #-}

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

-- -- take both DraftConst and DraftState, then run the action with both
-- runDraft :: DraftConst -> DraftState -> DraftM a -> IO (Either DraftError a, DraftState)
-- runDraft draftConst initialState action = runStateT (runReaderT (runExceptT action) draftConst) initialState

initializeDraftEnv :: C.Configuration -> O.OfficialRoster -> [PR.RankingData] -> (DraftConst, DraftState)
initializeDraftEnv config validPlayers rankings = 
    ( DraftConst
        { config = config
        , officialRoster = validPlayers
        , rankings = DraftM.filterInvalidRankings (C.lgMembers config) rankings
        }
    , DraftState
        { availableIds = map O.playerId $ O.people validPlayers
        , currentPick = 0
        , currentRound = 1
        , draft_rosters = zip (mkLgManagers config) (repeat [])
        }
    )

-- Draft cycle operation within DraftM
draftCycleM :: DraftM ()
draftCycleM = do
    DraftConst {..} <- getDraftConst
    DraftState {..} <- get
    -- Simulate a draft decision and update; this is a placeholder for actual logic
    let nextAvailableIds = tail availableIds  -- Example operation: draft the first available player
    let nextCurrentPick = currentPick + 1
    updateDraftState $ \s -> s { availableIds = nextAvailableIds, currentPick = nextCurrentPick }
    -- Add more operations as needed

runDraft :: DraftConst -> DraftState -> DraftM a -> IO (Either DraftError a, DraftState)
runDraft draftConst initialState action =
    runStateT (runReaderT (runExceptT action) draftConst) initialState

-- Placeholder for a function that filters invalid rankings based on league configuration
filterInvalidRankings :: [Int] -> [PR.RankingData] -> [PR.RankingData]
filterInvalidRankings = undefined

-- Example draftPlayers function adapted to the monadic approach
draftPlayersM :: C.Configuration -> O.OfficialRoster -> [PR.RankingData] -> IO ()
draftPlayersM config officialRoster rankings = do
    let (draftConst, draftState) = initializeDraftEnv config officialRoster rankings
    (_, finalState) <- runDraft draftConst draftState (runDraft 10)  -- Assuming 10 picks for simplicity
    print finalState  -- Or perform any final actions with the finalState


-- init
  -- load constants: config, officialRoster, and rankings
  -- populate DraftState with availableIds from officialRoster constant
  -- find all team id's in config and count teams listed
  -- create empty draft_rosters with config info
  -- create empty league managers with appropriate function 
    -- fill indices in draft_rosters with draft order using serpentine order
  -- load rankings for each team
    -- verify
    -- if rankings are incomplete, auto-fill them, 
    -- if ranking contains an unmatching playerid, remove that player 
-- draft one round of players
  -- get next teamid
      -- lookup top ranked player for that team
      -- is player is available? yes continue, no move to next ranked player
        -- if roster slots is full, skip to next player
        -- if not, add to roster
        
      -- reverse order if round is even
