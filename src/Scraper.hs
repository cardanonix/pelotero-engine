{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Scraper where

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
-- import OutputADT
--     ( PlayerData (..)
--     , PlayerStatsOutput (..)
--     , OutputData (..)
--     )

-- monadic error handling for fetching and decoding
withEither :: IO (Either String a) -> (a -> IO ()) -> IO ()
withEither action successHandler = do
    result <- action
    case result of
        Left err       -> putStrLn err
        Right dataPacket -> successHandler dataPacket

-- takes a date string "YYYY-MM-DD" and outputs a schedule bytestring of that day schdule
fetchGameScheduleForDate :: String -> IO (Either String GameSchedule)
fetchGameScheduleForDate date = do
    scheduleResult <- fetchAndDecode (scheduleUrl date)
    return $ fmap (assignDateToSchedule (T.pack date)) scheduleResult

assignDateToSchedule :: Text -> GameSchedule -> GameSchedule
assignDateToSchedule date schedule = 
    let assignToDateEntry entry = entry { games = fmap (V.map assignToDate) (games entry) }
        assignToDate gameID = gameID { game_date = Just date }
    in schedule { dates = map assignToDateEntry (dates schedule) }

-- takes a season and outputs a roster bytestring of that season
fetchActiveRoster :: Int -> IO (Either String I.ActivePlayer)
fetchActiveRoster season = fetchAndDecode (rosterUrl season)

-- Takes a schedule bytestring and outputs true if games are happening, false otherwise.
hasGamesForDate :: I.GameSchedule -> Bool
hasGamesForDate schedule = any (isJust . games) (dates schedule)

-- Takes a schedule bytestring and outputs an array of gameId's or errors
extractGameIds :: I.GameSchedule -> [Int]
extractGameIds gameData = concatMap (maybe [] (V.toList . fmap gamePk) . games) (dates gameData)

-- Generate the API URL for a single day's schedule
scheduleUrl :: String -> String
scheduleUrl date = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date

-- Generate the API URL for live game status
gameStatusUrl :: Int -> String
gameStatusUrl gameId = "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live"

-- Generate the API URL for finished boxscore
boxScoreUrl :: Int -> String
boxScoreUrl gameId = "http://statsapi.mlb.com/api/v1/game/" ++ show gameId ++ "/boxscore"

-- Generate the API URL for specific year 
rosterUrl :: Int -> String
rosterUrl season = "https://statsapi.mlb.com/api/v1/sports/1/players?activeStatus=ACTIVE&season=" ++ show season

-- Fetch and decode utility
fetchAndDecode :: FromJSON a => String -> IO (Either String a)
fetchAndDecode url = do
    response <- httpBS (parseRequest_ url)
    return $ eitherDecodeStrict $ getResponseBody response

-- takes a gameId and returns IO (Either String LiveGameWrapper)
fetchGameStatus :: Int -> IO (Either String I.LiveGameWrapper)
fetchGameStatus gameId = fetchAndDecode (gameStatusUrl gameId)

assignGameIdToPlayers :: Int -> GameData -> GameData
assignGameIdToPlayers gameId gameData = 
    let assignToTeam team = team { players = M.map assignToPlayer (players team) }
        assignToPlayer player = player { gameid = Just gameId }
    in gameData { teams = (teams gameData) { I.away = assignToTeam (I.away (teams gameData)), 
                                             I.home = assignToTeam (I.home (teams gameData)) } }

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
                   boxscoreResult <- fetchAndDecode (boxScoreUrl gameId)
                   return $ fmap (assignGameIdToPlayers gameId) boxscoreResult
               else return $ Left "Game isn't finished yet"
        Left err -> return $ Left ("Error fetching game status: " ++ err)

--maps fetchFinishedBxScore over an array of game id's and returns IO (M.Map Int ByteString)
processGameIds :: [Int] -> IO (Either String (M.Map Int I.GameData))
processGameIds gameIds = do
    results <- mapM fetchFinishedBxScore gameIds
    case sequence results of
        Left err -> return $ Left err
        Right gameDataList -> return $ Right $ M.fromList $ zip gameIds gameDataList

-- takes a list of tuples game id's and game data and prints them
printProcessedGameData :: Either String (M.Map Int I.GameData) -> IO ()
printProcessedGameData gameDataMapEither =
    withEither (return gameDataMapEither) $ \gameDataMap -> 
        mapM_ (\(gameId, gameData) -> putStrLn $ show gameId ++ ": " ++ show gameData) (M.toList gameDataMap)

-- print the schedule bytestring
processAndPrintGames :: Either String I.GameSchedule -> IO ()
processAndPrintGames gameScheduleEither =
    withEither (return gameScheduleEither) $ \gameSchedule -> 
        if hasGamesForDate gameSchedule then do
            let gameIds = extractGameIds gameSchedule
            gameDataMap <- processGameIds gameIds
            printProcessedGameData gameDataMap
        else putStrLn "No games scheduled for the provided date."

-- ## OUTPUT CONVERSION ##

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

writePlayerToJsonFile :: FilePath -> I.Player -> IO ()
writePlayerToJsonFile path player = B.writeFile path (convertPlayerToJson player)

convertGameDataMapToJsonPlayerData :: M.Map Int I.GameData -> M.Map Int [MI.JsonPlayerData]
convertGameDataMapToJsonPlayerData = M.map gameDataToPlayerDataList
  where
    gameDataToPlayerDataList :: I.GameData -> [MI.JsonPlayerData]
    gameDataToPlayerDataList gameData =
        let awayPlayers = M.elems $ I.players $ I.away $ I.teams gameData
            homePlayers = M.elems $ I.players $ I.home $ I.teams gameData
        in map playerToJsonPlayerData (awayPlayers ++ homePlayers)

processGameIdsToJsonPlayerData :: [Int] -> IO (Either String (M.Map Int [MI.JsonPlayerData]))
processGameIdsToJsonPlayerData gameIds = do
    gameDataResult <- processGameIds gameIds
    return $ fmap convertGameDataMapToJsonPlayerData gameDataResult

