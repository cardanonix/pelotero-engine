{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Main where

import Control.Monad (foldM, forM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Generics (Generic)

import Data.List (
    delete,
    find,
    findIndex,
    sortBy,
    sortOn,
 )
import Data.Maybe (fromMaybe, mapMaybe)

import qualified Config as C
import qualified OfficialRoster as O
import qualified PlayerRanking as PR
import qualified Roster as R
import Validators

-- ( countPlayersOnRoster
-- , findPlayer
-- , queryDraftRosterLmts
-- )
import Utility

-- ( positionCodeToDraftText
-- ,  readJson
-- ,  writeJson
-- , positionCodeToDraftText
-- , createLgManager
--   )

import Draft

main :: IO ()
main = do
    eitherR1 <- readJson "testFiles/appData/rankings/_087cc1f8c262_.json"
    case eitherR1 of
        Left error -> putStrLn $ "Failed to load rankings 1: " ++ show error
        Right r1 -> do
            eitherR2 <- readJson "testFiles/appData/rankings/_11817bfe52d3_.json"
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
                                        teamId1Short = T.take 12 $ C.unwrapTeamID teamId1
                                        teamId2Short = T.take 12 $ C.unwrapTeamID teamId2

                                    -- Draft players for both teams and obtain rosters and lineups
                                    ((finalRoster1, finalLineup1), (finalRoster2, finalLineup2)) <- draftPlayers rankings1 rankings2 op config

                                    let lgManager1 = createLgManager config teamId1 finalLineup1 finalRoster1
                                        lgManager2 = createLgManager config teamId2 finalLineup2 finalRoster2

                                    -- Analyze the draft results for both teams
                                    putStrLn "\nAnalyzing draft results for validity..."
                                    Validators.analyzeDraftResults config (lgManager1, lgManager2) (r1, r2) 20
                                    -- Write the LgManager instances to JSON files
                                    writeJson (T.unpack $ "testFiles/appData/draftResults/" <> teamId1Short <> "_draft_results_new.json") lgManager1
                                    writeJson (T.unpack $ "testFiles/appData/draftResults/" <> teamId2Short <> "_draft_results_new.json") lgManager2

                                    putStrLn "Draft, analysis, and LgManager serialization completed successfully."
