{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Scraper (
    fetchActiveRoster,
    fetchGameScheduleForDate,
    fetchGameStatus,
    fetchFinishedBxScore,
    fetchFinishedBxScores
)where

import Network.HTTP.Simple
    ( parseRequest_,
      getResponseBody,
      httpBS )
import Data.Time
    ( Day, addDays, diffDays, parseTimeOrError, defaultTimeLocale, formatTime )
import Data.Time.Clock.POSIX ()
import Data.ByteString (ByteString, empty)
import qualified Data.Vector as V
import Data.Maybe (isJust, fromMaybe, maybeToList)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
    ( eitherDecode,
      encode,
      (.:),
      (.:?),
      genericParseJSON,
      withObject,
      defaultOptions,
      FromJSON(parseJSON),
      Options(fieldLabelModifier),
      eitherDecodeStrict )
import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Crypto.Hash.SHA256 as SHA256
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Control.Monad (when, filterM)

import qualified InputADT as I
import InputADT
    ( GameData(..)
    , LiveGameStatusWrapper(..)
    , LiveGameWrapper(..)
    , LiveGameStatus(..)
    , DateEntry(..)
    , GameSchedule(..)
    , GameID(..)
    , TeamData(..)
    , Player (..)
    , PlayerStats (..)
    , ActivePlayer (..)
    )
import qualified MiddleADT as MI
import qualified OutputADT as OUT
import ScrapeTools

-- # Printing 
-- takes a list of tuples game id's and game data and prints them
printGameData :: Either String (M.Map Int I.GameData) -> IO ()
printGameData gameDataMapEither =
    withEither (return gameDataMapEither) $ \gameDataMap ->
        mapM_ (\(gameId, gameData) -> putStrLn $ show gameId ++ ": " ++ show gameData) (M.toList gameDataMap)

-- print the schedule bytestring
processAndPrintGames :: Either String I.GameSchedule -> IO ()
processAndPrintGames gameScheduleEither =
    withEither (return gameScheduleEither) $ \gameSchedule ->
        if hasGamesForDate gameSchedule then do
            let gameIds = extractGameIds gameSchedule
            gameDataMap <- fetchFinishedBxScores gameIds
            printGameData gameDataMap
        else putStrLn "No games scheduled for the provided date."


-- mutation functions:
-- Stat-Mutation stuff
playerToJsonPlayerData :: I.Player -> MI.JsonPlayerData
playerToJsonPlayerData p =
    MI.JsonPlayerData
        { MI.playerId = T.pack $ show $ I.personId (I.person p)
        , MI.fullName = I.fullName (I.person p)
        , MI.stats = M.singleton (maybe "" (T.pack . show) (I.gameid p)) (playerToJsonStatsData p)
        }

playerToJsonStatsData :: I.Player -> MI.JsonStatsData
playerToJsonStatsData p =
    MI.JsonStatsData
        { MI.parentTeamId = I.parentTeamId p
        , MI.allPositions = fromMaybe [] (I.allPositions p)
        , MI.statusCode = I.status_code (I.status p)
        , MI.batting = I.batting (I.stats p)
        , MI.pitching = I.pitching (I.stats p)
        }

convertPlayerToJson :: I.Player -> ByteString
convertPlayerToJson = BL.toStrict . encode . playerToJsonPlayerData

convertGameDataMapToJsonPlayerData :: M.Map Int I.GameData -> M.Map Int [MI.JsonPlayerData]
convertGameDataMapToJsonPlayerData = M.map gameDataToPlayerDataList
  where
    gameDataToPlayerDataList :: I.GameData -> [MI.JsonPlayerData]
    gameDataToPlayerDataList gameData =
        let awayPlayers = M.elems $ I.players $ I.away $ I.teams gameData
            homePlayers = M.elems $ I.players $ I.home $ I.teams gameData
        in map playerToJsonPlayerData (awayPlayers ++ homePlayers)


-- takes a season and outputs a roster bytestring of that season
fetchActiveRoster :: Int -> IO (Either String I.ActivePlayer)
fetchActiveRoster season = fetchAndDecodeJSON (rosterUrl season)

-- takes a date string "YYYY-MM-DD" and outputs a schedule bytestring of that day schdule
fetchGameScheduleForDate :: String -> IO (Either String GameSchedule)
fetchGameScheduleForDate date = do
    scheduleResult <- fetchAndDecodeJSON (scheduleUrl date)
    return $ fmap (assignDateToSchedule (T.pack date)) scheduleResult

-- takes a gameId and returns IO (Either String LiveGameWrapper)
fetchGameStatus :: Int -> IO (Either String I.LiveGameWrapper)
fetchGameStatus gameId = fetchAndDecodeJSON (gameStatusUrl gameId)

-- takes a gameId and returns IO (Either String GameData)
fetchFinishedBxScore :: Int -> IO (Either String I.GameData)
fetchFinishedBxScore gameId = do
    gameStatusResult <- fetchGameStatus gameId
    case gameStatusResult of
        Right gameDataWrapper -> do
            let liveStatusWrapper = gameData gameDataWrapper
            let liveStatus = gameStatus liveStatusWrapper
            if codedGameState liveStatus == "F"
               then do
                   boxscoreResult <- fetchAndDecodeJSON (boxScoreUrl gameId)
                   return $ fmap (assignGameIdToPlayers gameId) boxscoreResult -- *adds gameId attribute to corresponding stats
               else return $ Left "Game isn't finished yet"
        Left err -> return $ Left ("Error fetching game status: " ++ err)

--maps fetchFinishedBxScore over an array of game id's and returns IO (M.Map Int ByteString)
fetchFinishedBxScores :: [Int] -> IO (Either String (M.Map Int I.GameData))
fetchFinishedBxScores gameIds = do
    results <- mapM fetchFinishedBxScore gameIds
    case sequence results of
        Left err -> return $ Left err
        Right gameDataList -> return $ Right $ M.fromList $ zip gameIds gameDataList

-- ## OUTPUT CONVERSION ##

fetchFinishedBxScoresToJsonPlayerData :: [Int] -> IO (Either String (M.Map Int [MI.JsonPlayerData]))
fetchFinishedBxScoresToJsonPlayerData gameIds = do
    gameDataResult <- fetchFinishedBxScores gameIds
    return $ fmap convertGameDataMapToJsonPlayerData gameDataResult

-- ## Very Rough Output Stuff ##

processDate :: String -> IO ()
processDate date = do
    putStrLn $ "Processing " ++ date
    scheduleResult <- fetchGameScheduleForDate date
    processAndPrintGames scheduleResult
    case scheduleResult of
        Left err -> putStrLn $ "Failed to fetch game schedule: " ++ err
        Right schedule -> do
            let gameIds = extractGameIds schedule
            flattenedPlayersResult <- fetchFinishedBxScoresToJsonPlayerData gameIds
            case flattenedPlayersResult of
                Left err -> putStrLn $ "Failed to process JSON: " ++ err
                Right flattenedPlayers -> do
                    putStrLn "Flattened Player Data:"
                    print flattenedPlayers
                    -- Writing data to file
                    let filename = formatFilename date
                    writeDataToFile filename "scrapedData/stats/" flattenedPlayers

-- Main scraper function tying everything together
scrapeDataForDateRange :: String -> String -> IO ()
scrapeDataForDateRange start end = do
    mapM_ processDate (generateDateRange start end)