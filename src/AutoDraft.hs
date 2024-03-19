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
import qualified Ranking as PR
import Validators 
    ( countPlayers
    , findPlayer
    , lookupLimit 
    )
import Utility
    ( positionCodeToDraftText
    ,  readJson
    ,  writeJson
    , positionCodeToDraftText
      )

import Draft

autoFillLineup :: C.Configuration -> [PR.PlayerRanking] -> [O.OfficialPlayer] -> R.CurrentLineup
autoFillLineup config rankings officialRoster =
  let fillPosition position = 
        let limit = getPositionLimit position (C.valid_roster $ C.point_parameters config)
        in take limit $ getRankedPlayersForPosition rankings officialRoster position
  in R.CurrentLineup {
      R.cC  = fillPosition "C",
      R.b1C = fillPosition "1B",
      R.b2C = fillPosition "2B",
      R.b3C = fillPosition "3B",
      R.ssC = fillPosition "SS",
      R.ofC = fillPosition "OF",
      R.uC  = fillPosition "U",
      R.spC = fillPosition "SP",
      R.rpC = fillPosition "RP"
    }

main :: IO ()
main = do
    eitherR1 <- readJson "testFiles/appData/rankings/_4aeebfdcc387_.json"
    eitherR2 <- readJson "testFiles/appData/rankings/_4d0f22bec934_.json"
    -- eitherR3 <- readJson "testFiles/appData/rankings/_8902a8299c1c_.json"
    -- eitherR4 <- readJson "testFiles/appData/rankings/_474414c7ab56_.json"    
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
            let currentLineup1 = autoFillLineup config rankings1 op
            let currentLineup2 = autoFillLineup config rankings2 op

            -- Create LgManager instances with the filled lineups
            let lgManager1 = createLgManager config teamId1 currentLineup1 finalRoster1
                lgManager2 = createLgManager config teamId2 currentLineup2 finalRoster2
            -- Write the LgManager instances to JSON files
            writeJson (T.unpack $ "testFiles/appData/draftResults/" <> teamId1Short <> "_draft_results.json") lgManager1
            writeJson (T.unpack $ "testFiles/appData/draftResults/" <> teamId2Short <> "_draft_results.json") lgManager2
            putStrLn "Draft and LgManager serialization completed successfully."
        _ -> putStrLn "Failed to load one or more necessary files. Check file paths and data integrity."
