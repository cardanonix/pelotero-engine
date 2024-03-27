{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main where

import Control.Monad (forM, foldM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
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
import qualified PlayerRanking as PR
import Validators 
    ( countPlayersOnRoster
    , findPlayer
    , queryDraftRosterLmts 
    )
import Utility
    ( positionCodeToDraftText
    ,  readJson
    ,  writeJson
    , positionCodeToDraftText
    , createLgManager
      )

import Draft


main :: IO ()
main = do
    eitherR1 <- readJson "testFiles/appData/rankings/_4aeebfdcc387_.json"
    case eitherR1 of
        Left error -> putStrLn $ "Failed to load rankings 1: " ++ show error
        Right r1 -> do
            eitherR2 <- readJson "testFiles/appData/rankings/_4d0f22bec934_.json"
            case eitherR2 of
                Left error -> putStrLn $ "Failed to load rankings 2: " ++ show error
                Right r2 -> do
                    eitherRoster <- readJson "testFiles/appData/rosters/activePlayers.json"
                    case eitherRoster of
                        Left error -> putStrLn $ "Failed to load rosters: " ++ show error
                        Right roster -> do
                            eitherConfig <- readJson "testFiles/appData/config/config.json"
                            case eitherConfig of
                                Left error -> putStrLn $ "Failed to load config: " ++ show error
                                Right config -> do
                                    let rankings1 = PR.rankings r1
                                        rankings2 = PR.rankings r2
                                        op = O.people roster
                                        teamId1 = PR.teamId r1
                                        teamId2 = PR.teamId r2
                                        teamId1Short = T.take 12 teamId1
                                        teamId2Short = T.take 12 teamId2

                                    -- Draft players for both teams and obtain rosters and lineups
                                    ((finalRoster1, finalLineup1), (finalRoster2, finalLineup2)) <- draftPlayers rankings1 rankings2 op config

                                    let lgManager1 = createLgManager config teamId1 finalLineup1 finalRoster1
                                        lgManager2 = createLgManager config teamId2 finalLineup2 finalRoster2

                                    -- Write the LgManager instances to JSON files
                                    writeJson (T.unpack $ "testFiles/appData/draftResults/" <> teamId1Short <> "_draft_results_new.json") lgManager1
                                    writeJson (T.unpack $ "testFiles/appData/draftResults/" <> teamId2Short <> "_draft_results_new.json") lgManager2

                                    putStrLn "Draft and LgManager serialization completed successfully."
