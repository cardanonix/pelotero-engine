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

import Data.Maybe (mapMaybe)
import Data.List (find, delete)
import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified Ranking as PR
import Validators ( countPlayers, findPlayer, lookupLimit )
import Utility ( readJson, writeJson, positionCodeToDraftText )
import Draft


autoFillLineup :: C.Configuration -> R.Roster -> R.CurrentLineup
autoFillLineup config roster = R.CurrentLineup
    { R.cC = take (C.lg_catcher rosterLimits) $ R.cR roster
    , R.b1C = take (C.lg_first rosterLimits) $ R.b1R roster
    , R.b2C = take (C.lg_second rosterLimits) $ R.b2R roster
    , R.b3C = take (C.lg_third rosterLimits) $ R.b3R roster
    , R.ssC = take (C.lg_shortstop rosterLimits) $ R.ssR roster
    , R.ofC = take (C.lg_outfield rosterLimits) $ R.ofR roster
    , R.uC = take (C.lg_utility rosterLimits) $ R.uR roster
    , R.spC = take (C.lg_s_pitcher rosterLimits) $ R.spR roster
    , R.rpC = take (C.lg_r_pitcher rosterLimits) $ R.rpR roster
    }
  where
    rosterLimits = C.valid_roster $ C.point_parameters config

main :: IO ()
main = do
    eitherR1 <- readJson "testFiles/appData/rankings/_2f70cf31d261_.json"
    eitherR2 <- readJson "testFiles/appData/rankings/_3da9fd7edb1b_.json"
    eitherRoster <- readJson "testFiles/appData/rosters/activePlayers.json"
    eitherConfig <- readJson "testFiles/appData/config/config.json"

    case (eitherR1, eitherR2, eitherRoster, eitherConfig) of
        (Right r1, Right r2, Right roster, Right config) -> do
            let rankings1 = PR.rankings r1
                rankings2 = PR.rankings r2
                op = O.people roster
                teamId1 = PR.teamId r1
                teamId2 = PR.teamId r2
                teamId1Short = T.take 12 teamId1
                teamId2Short = T.take 12 teamId2
            -- Draft players for both teams
            (finalRoster1, finalRoster2) <- draftPlayers rankings1 rankings2 op config
            -- Automatically fill lineups based on drafted rosters and configuration
            let currentLineup1 = autoFillLineup config finalRoster1
                currentLineup2 = autoFillLineup config finalRoster2
            -- Create LgManager instances with the filled lineups
            let lgManager1 = createLgManager config teamId1 currentLineup1 finalRoster1
                lgManager2 = createLgManager config teamId2 currentLineup2 finalRoster2
            -- Write the LgManager instances to JSON files
            writeJson (T.unpack $ "testFiles/appData/draftResults/" <> teamId1Short <> "_draft_results.json") lgManager1
            writeJson (T.unpack $ "testFiles/appData/draftResults/" <> teamId2Short <> "_draft_results.json") lgManager2
            putStrLn "Draft and LgManager serialization completed successfully."
        _ -> putStrLn "Failed to load one or more necessary files. Check file paths and data integrity."
