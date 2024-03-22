{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main where

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
import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified PlayerRanking as PR
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

main :: IO ()
main = do
    eitherRankings1 <- readJson "testFiles/appData/rankings/_4aeebfdcc387_.json" :: IO (Either String PR.RankingData)
    eitherRankings2 <- readJson "testFiles/appData/rankings/_4d0f22bec934_.json" :: IO (Either String PR.RankingData)
    eitherPlayers <- readJson "testFiles/appData/draft_rosters/activePlayers.json" :: IO (Either String O.OfficialRoster)
    eitherConfig <- readJson "testFiles/appData/config/config.json" :: IO (Either String C.Configuration)

    case (eitherConfig, eitherPlayers, eitherRankings1, eitherRankings2) of
        (Right config, Right officialRoster, Right rankingsData1, Right rankingsData2) -> do
            let players = O.people officialRoster
                rankings1 = PR.rankings rankingsData1
                rankings2 = PR.rankings rankingsData2
                availableIds = map O.playerId players
                numTeams = 2 -- Ensure this matches the expected number of teams
                initialState = initializeDraftState config players availableIds numTeams
                
            (result, finalState) <- runDraft initialState $ draftPlayersM [rankings1, rankings2]

            case result of
                Left err -> putStrLn $ "Draft error: " ++ show err
                Right _ -> case draft_rosters finalState of
                    (roster1:roster2:_) -> do
                        let (finalRoster1, finalLineup1, _) = roster1
                            (finalRoster2, finalLineup2, _) = roster2
                            teamId1 = T.take 12 $ PR.teamId rankingsData1
                            teamId2 = T.take 12 $ PR.teamId rankingsData2

                        writeJson ("testFiles/appData/draftResults/" <> T.unpack teamId1 <> "_draft_results_monadic.json") (createLgManager config teamId1 finalLineup1 finalRoster1)
                        writeJson ("testFiles/appData/draftResults/" <> T.unpack teamId2 <> "_draft_results_monadic.json") (createLgManager config teamId2 finalLineup2 finalRoster2)

                        putStrLn "Draft and LgManager serialization completed successfully."
                    _ -> putStrLn "Not enough teams in the final state to generate output."

        _ -> putStrLn "Failed to load one or more necessary files. Check file paths and data integrity."
