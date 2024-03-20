{-# LANGUAGE OverloadedStrings #-}
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

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T

-- Define a custom error type for your application
data DraftError = PlayerNotFound Int
                | PositionFull T.Text
                | OtherDraftError String
                deriving (Show, Eq)

-- Define your draft state
data DraftState = DraftState {
  rosters :: [(R.Roster, R.CurrentLineup, [Int])],
  currentPick :: Int
  -- Add other necessary state fields
}

type DraftMonad a = ExceptT DraftError (StateT DraftState IO) a

-- -- Example of a function within DraftMonad
-- findPlayerMonad :: Int -> DraftMonad O.OfficialPlayer
-- findPlayerMonad playerId = do
--   draftState <- get
--   -- Logic to find a player
--   maybePlayer <- liftIO $ findPlayerLogic playerId -- replace with actual logic
--   case maybePlayer of
--     Just player -> return player
--     Nothing -> throwError $ PlayerNotFound playerId


findPlayerMonad :: Int -> [O.OfficialPlayer] -> [Int] -> DraftMonad O.OfficialPlayer
findPlayerMonad playerId players availableIds = case findPlayer playerId players availableIds of
    Just player -> return player
    Nothing -> throwError $ PlayerNotFound playerId

draftCycleMonad :: [Int] -> [PR.PlayerRanking] -> DraftMonad ()
draftCycleMonad order teamRankings = do
  -- Example of manipulating state and handling errors
  draftState <- get
  player <- findPlayerMonad 123 -- example player ID
  -- draft logic here...
  -- update state with put if necessary