{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad (foldM) -- Removed forM since it's not used here directly
import Control.Monad (forM_) -- Explicit import for forM_
import Data.Aeson
    ( decode,
      encode,
      FromJSON,
      ToJSON,
      decode,
      encode,
      withObject,
      (.:) )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import GHC.Generics ( Generic, Generic )
import Data.Time.Clock ( getCurrentTime, UTCTime, getCurrentTime )
import Data.Time.Format
    ( formatTime, defaultTimeLocale, formatTime, defaultTimeLocale )
import System.Directory ( listDirectory, listDirectory )
import System.FilePath
    ( (</>), takeExtension, (</>), takeExtension )
import Data.Maybe ( fromMaybe, mapMaybe, fromMaybe )
import Data.List
    ( find,
      delete,
      sortOn,
      sortBy,
      findIndex,
      find,
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
    ( countPlayers,
      findPlayer,
      queryDraftRosterLmt,
      countPlayers,
      findPlayer,
      queryDraftRosterLmt )
import Utility
    ( readJson,
      writeJson,
      createLgManager,
      positionCodeToDraftText,
      readJson,
      writeJson,
      positionCodeToDraftText,
      createLgManager )
import Draft
import Data.Foldable (forM_) -- Add this if forM_ is still not in scope
import Control.Monad (forM, foldM)

main :: IO ()
main = do
    -- Adjust these paths as needed
    let rankingsDir = "testFiles/appData/rankings"
    eitherRoster <- readJson "testFiles/appData/rosters/activePlayers.json"
    eitherConfig <- readJson "testFiles/appData/config/config.json"

    -- List all JSON files in the rankings directory
    files <- listDirectory rankingsDir
    let rankingFiles = filter (\f -> takeExtension f == ".json") files

    -- Read and decode all ranking files
    rankingDatas <- mapM (\file -> readJson (rankingsDir </> file)) rankingFiles
    let eitherRankings = sequence rankingDatas -- Convert [Either a] to Either [a]

    case (eitherRankings, eitherRoster, eitherConfig) of
        (Right rankings, Right roster, Right config) -> do
            let allRankings = map PR.rankings rankings
                op = O.people roster

            -- Process the draft for all teams
            finalStates <- draftPlayers allRankings op config

            -- Process each team's results
            forM_ (zip finalStates rankings) $ \((finalRoster, finalLineup, _availablePlayerIds), rankingData) -> do
                let teamId = PR.teamId rankingData
                    teamIdShort = T.take 12 teamId

                -- Create the LgManager with the team's final roster and lineup
                let lgManager = createLgManager config teamId finalLineup finalRoster

                -- Write the LgManager instance to a JSON file
                writeJson (T.unpack $ "testFiles/appData/draftResults/" <> teamIdShort <> "_draft_results_new.json") lgManager

            putStrLn "Draft and LgManager serialization completed successfully."

        _ -> putStrLn "Failed to load one or more necessary files. Check file paths and data integrity."
