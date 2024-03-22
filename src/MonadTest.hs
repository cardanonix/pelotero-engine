{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main where

import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified Ranking as PR
import Validators
    ( countPlayers,
      findPlayer,
      queryDraftRosterLmts,
      countPlayers,
      findPlayer,
      queryDraftRosterLmts,
      countPlayers,
      findPlayer,
      queryDraftRosterLmts )

import Utility
    ( readJson,
      writeJson,
      createLgManager 
    )

import DraftM
import Control.Monad.State ( runStateT, runStateT )
import Control.Monad.Except ( runExceptT, runExceptT )
import Data.Text as T (unpack, take)
import Data.Maybe ( fromMaybe, mapMaybe, fromMaybe )
import Data.Either (fromRight)
import Control.Monad (forM, foldM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.List
    ( find,
      delete,
      sortBy,
      findIndex,
      sortOn )

initialDraftState :: C.Configuration -> [O.OfficialPlayer] -> [Int] -> DraftState
initialDraftState config players availableIds = DraftState
    { rosters = []
    , currentPick = 0
    , allPlayers = players
    , availableIds = availableIds
    , config = config
    }

-- Adjusted function to run the draft and handle the result
runDraft :: DraftState -> DraftM a -> IO (Either DraftError a, DraftState)
runDraft initialState action = runStateT (runExceptT action) initialState

main :: IO ()
main = do
    eitherRankings1 <- readJson "testFiles/appData/rankings/_4aeebfdcc387_.json"
    eitherRankings2 <- readJson "testFiles/appData/rankings/_4d0f22bec934_.json"
    eitherPlayers <- readJson "testFiles/appData/rosters/activePlayers.json"
    eitherConfig <- readJson "testFiles/appData/config/config.json"

    case (eitherConfig, eitherPlayers, eitherRankings1, eitherRankings2) of
        (Right config, Right players, Right r1, Right r2) -> do
            let rankings1 = PR.rankings r1
                rankings2 = PR.rankings r2
                availableIds = map O.playerId players
                initialState = initialDraftState config players availableIds

            (result, finalState) <- runDraft initialState (draftPlayersMonad [rankings1, rankings2])

            case result of
                Left err -> putStrLn $ "Draft error: " ++ show err
                Right _ -> do
                    -- Directly pattern match on the first two elements of the rosters list
                    let [(finalRoster1, finalLineup1, _), (finalRoster2, finalLineup2, _)] = rosters finalState
                        teamId1 = T.take 12 $ PR.teamId r1
                        teamId2 = T.take 12 $ PR.teamId r2

                    writeJson ("testFiles/appData/draftResults/" <> T.unpack teamId1 <> "_draft_results_new.json") (createLgManager config teamId1 finalLineup1 finalRoster1)
                    writeJson ("testFiles/appData/draftResults/" <> T.unpack teamId2 <> "_draft_results_new.json") (createLgManager config teamId2 finalLineup2 finalRoster2)

                    putStrLn "Draft and LgManager serialization completed successfully."
        _ -> putStrLn "Failed to load one or more necessary files. Check file paths and data integrity."
