{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use catMaybes" #-}

module Scraper (
    scrapeStatsForDateRange,
    writeRosterToFile,
    fetchActiveRoster,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (filterM, when)
import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (
    FromJSON (parseJSON),
    Options (fieldLabelModifier),
    defaultOptions,
    eitherDecode,
    eitherDecodeStrict,
    encode,
    genericParseJSON,
    withObject,
    (.:),
    (.:?),
 )
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (
    Day,
    addDays,
    defaultTimeLocale,
    diffDays,
    formatTime,
    parseTimeOrError,
 )
import Data.Time.Clock
import Data.Time.Clock.POSIX ()
import Data.Time.Format
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Simple (
    getResponseBody,
    httpBS,
    parseRequest_,
 )
import System.Directory (createDirectoryIfMissing, doesFileExist)

import qualified Input as I
import qualified Middle as MI
import qualified Points as P
import Validators
import Utility
    ( computeChecksum, fetchAndDecodeJSON, getCurrentDate, withEither )

-- A (date String) -> [B] (list of gameIds/GameSchedule)
-- takes a date string "YYYY-MM-DD" and outputs a schedule bytestring of that day schdule
fetchGameScheduleForDate :: String -> IO (Either String I.GameSchedule)
fetchGameScheduleForDate date = do
    scheduleResult <- fetchAndDecodeJSON (scheduleUrl date)
    return $ fmap (assignDateToSchedule (T.pack date)) scheduleResult

-- B (gameId) -> C (status)
-- takes a gameId and returns IO (Either String LiveGameWrapper)
fetchGameStatus :: Int -> IO (Either String I.LiveGameWrapper)
fetchGameStatus gameId = fetchAndDecodeJSON (gameStatusUrl gameId)

-- B (gameId) -> C (status) -> D (boxscore)
-- takes a gameId and returns IO (Either String GameData)
fetchFinishedBxScore :: Int -> IO (Either String (Maybe I.GameData))
fetchFinishedBxScore gameId = do
    gameStatusResult <- fetchGameStatus gameId
    case gameStatusResult of
        Right gameDataWrapper -> do
            let liveStatusWrapper = I.gameData gameDataWrapper
            let liveStatus = I.gameStatus liveStatusWrapper
            if I.codedGameState liveStatus == "F"
                then do
                    boxscoreResult <- fetchAndDecodeJSON (boxScoreUrl gameId)
                    return $ fmap (Just . assignGameIdToPlayers gameId) boxscoreResult
                else -- \*adds gameId attribute to corresponding stats
                    return $ Right Nothing
        Left err -> return $ Left ("Error fetching game status: " ++ err)

-- -- [B] list of gameIds -> C status checks -> [D] list of boxscores
-- fetchFinishedBxScores :: [Int] -> IO (Either String (M.Map Int I.GameData))
fetchFinishedBxScores :: [Int] -> IO (Either String (M.Map Int (Maybe I.GameData)))
fetchFinishedBxScores gameIds = do
    results <- mapConcurrently fetchGame gameIds
    let combinedResults = sequenceA results -- Change the structure from [Either] to Either [..]
    return $ fmap (M.fromList . filter finishedGames) combinedResults
  where
    fetchGame gameId = do
        result <- fetchFinishedBxScore gameId
        return $ fmap (\d -> (gameId, d)) result
    finishedGames (_, Nothing) = False
    finishedGames (_, Just _) = True

-- ## OUTPUT CONVERSION ##
-- [B] list of gameIds -> C status checks -> [D] (list of box scores) -> [E] (list of player data)
fetchFinishedBxScoresToJsonPlayerData :: [Int] -> IO (Either String (M.Map Text MI.JsonPlayerData))
fetchFinishedBxScoresToJsonPlayerData gameIds = do
    gameDataResult <- fetchFinishedBxScores gameIds
    return $ fmap convertGameDataMapToJsonPlayerData gameDataResult

-- Main scraper function tying stats scaping everything together
scrapeStatsForDateRange :: String -> String -> IO ()
scrapeStatsForDateRange start end = do
    mapM_ processDate (generateDateRange start end)

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
                Right _flattenedPlayers -> do
                    -- The printout of flattenedPlayers has been removed
                    let filename = formatFilename date
                    writeDataToFile filename "appData/stats" _flattenedPlayers

flattenedPlayersList :: M.Map Text MI.JsonPlayerData -> M.Map Text MI.JsonPlayerData
flattenedPlayersList = id -- or simply remove this function and use the map directly

-- takes a list of tuples game id's and game data and prints them
printGameData :: Either String (M.Map Int I.GameData) -> IO ()
printGameData gameDataMapEither =
    withEither (return gameDataMapEither) $ \gameDataMap ->
        mapM_ (\(gameId, gameData) -> putStrLn $ show gameId ++ ": " ++ show gameData) (M.toList gameDataMap)

processAndPrintGames :: Either String I.GameSchedule -> IO ()
processAndPrintGames gameScheduleEither =
    withEither (return gameScheduleEither) $ \gameSchedule ->
        if hasGamesForDate gameSchedule
            then do
                let gameIds = extractGameIds gameSchedule
                _ <- fetchFinishedBxScores gameIds
                return ()
            else putStrLn "No games scheduled for the provided date."

-- takes a season and outputs a roster bytestring of that season
-- fetchActiveRoster :: Int -> IO (Either String I.ActivePlayer)
fetchActiveRoster :: Int -> IO (Either String I.ActiveRoster)
fetchActiveRoster season = fetchAndDecodeJSON (rosterUrl season)

writeRosterToFile :: FilePath -> I.ActiveRoster -> IO ()
writeRosterToFile path roster = do
    -- Original player data encoding
    let playerData = encode (I.people roster)

    -- Compute checksum and get date stamp
    dateStamp <- getCurrentDate
    let checksumValue = computeChecksum playerData
    let fullRoster = I.ActiveRoster (I.people roster) (Just dateStamp) (Just checksumValue)

    -- Encode the full roster including the checksum and date stamp
    let jsonData = encode fullRoster

    -- Write to file
    BL.writeFile path jsonData

-- Special Enhancement of fromJSON types that gets called as post-processing in the fetch functions
assignDateToSchedule :: Text -> I.GameSchedule -> I.GameSchedule
assignDateToSchedule date schedule =
    let assignToDateEntry entry = entry{I.games = fmap (V.map assignToDate) (I.games entry)}
        assignToDate gameID = gameID{I.game_date = Just date}
     in schedule{I.dates = map assignToDateEntry (I.dates schedule)}

assignGameIdToPlayers :: Int -> I.GameData -> I.GameData
assignGameIdToPlayers gameId gameData =
    let assignToTeam team = team{I.players = M.map assignToPlayer (I.players team)}
        assignToPlayer player = player{I.gameid = Just gameId}
     in gameData
            { I.teams =
                (I.teams gameData)
                    { I.away = assignToTeam (I.away (I.teams gameData))
                    , I.home = assignToTeam (I.home (I.teams gameData))
                    }
            }



-- Takes a schedule bytestring and outputs true if games are happening, false otherwise.
hasGamesForDate :: I.GameSchedule -> Bool
hasGamesForDate schedule = any (isJust . I.games) (I.dates schedule)

-- Takes a schedule bytestring and outputs an array of gameId's or errors
extractGameIds :: I.GameSchedule -> [Int]
extractGameIds gameData = concatMap (maybe [] (V.toList . fmap I.gamePk) . I.games) (I.dates gameData)

-- ## FileName Manipulation Stuff
-- Takes a filename, path, and the data to save, then writes to a JSON file at the specified path with the given filename.
writeDataToFile :: FilePath -> FilePath -> M.Map Text MI.JsonPlayerData -> IO ()
writeDataToFile filename path dataToSave = do
    createOutputDirectory path
    let fullpath = path ++ "/" ++ filename
    BL.writeFile fullpath (encode dataToSave)

-- Write Player to JSON File
writePlayerToJsonFile :: FilePath -> I.Player -> IO ()
writePlayerToJsonFile path player = B.writeFile path (convertPlayerToJson player)

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

convertGameDataMapToJsonPlayerData :: M.Map Int (Maybe I.GameData) -> M.Map Text MI.JsonPlayerData
convertGameDataMapToJsonPlayerData maybeGameDataMap =
    foldl mergePlayerData M.empty allPlayerDataPairs
  where
    allPlayerDataPairs :: [(Text, MI.JsonPlayerData)]
    allPlayerDataPairs = concatMap gameDataToPlayerDataPairs (mapMaybe id (M.elems maybeGameDataMap))

    gameDataToPlayerDataPairs :: I.GameData -> [(Text, MI.JsonPlayerData)]
    gameDataToPlayerDataPairs gameData =
        let awayPlayers = M.elems $ I.players $ I.away $ I.teams gameData
            homePlayers = M.elems $ I.players $ I.home $ I.teams gameData
            allPlayers = awayPlayers ++ homePlayers
         in map (\player -> (rawStringToText $ MI.playerId (playerToJsonPlayerData player), playerToJsonPlayerData player)) allPlayers

    rawStringToText :: Text -> Text
    rawStringToText = T.replace "\\\"" "\"" . T.replace "\\\\" "\\"

    mergePlayerData :: M.Map Text MI.JsonPlayerData -> (Text, MI.JsonPlayerData) -> M.Map Text MI.JsonPlayerData
    mergePlayerData acc (playerId, newPlayerData) =
        let mergedData = case M.lookup playerId acc of
                Just existingPlayerData -> mergeJsonPlayerData existingPlayerData newPlayerData
                Nothing -> newPlayerData
         in M.insert playerId mergedData acc

mergeJsonPlayerData :: MI.JsonPlayerData -> MI.JsonPlayerData -> MI.JsonPlayerData
mergeJsonPlayerData existing new =
    MI.JsonPlayerData
        { MI.playerId = MI.playerId existing -- assuming playerIds are the same, else there's a bigger problem!
        , MI.fullName = MI.fullName existing -- assuming fullNames are the same
        , MI.stats = M.unionWith mergeJsonStatsData (MI.stats existing) (MI.stats new)
        }

mergeJsonStatsData :: MI.JsonStatsData -> MI.JsonStatsData -> MI.JsonStatsData
mergeJsonStatsData _ new = new

-- Takes a date string and formats it as a filename, like "2023_08_22.json".
formatFilename :: String -> String
formatFilename date = replace '-' '_' date ++ ".json"
  where
    replace old new = T.unpack . T.replace (T.pack [old]) (T.pack [new]) . T.pack

-- Create output directory
createOutputDirectory :: FilePath -> IO ()
createOutputDirectory = createDirectoryIfMissing True

-- ## Dates ##
-- Converts a String of format "YYYY-MM-DD" to a Day
stringToDay :: String -> Day
stringToDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

-- Increments the Day by one
incrementDay :: Day -> Day
incrementDay = addDays 1

-- Convert StatType to a string representation for reasons????
statTypeToString :: P.StatType -> String
statTypeToString P.Batting = "batting"
statTypeToString P.Pitching = "pitching"

-- Generates a list of dates from the start to the end
generateDateRange :: String -> String -> [String]
generateDateRange start end = map (formatTime defaultTimeLocale "%Y-%m-%d") dates
  where
    startDate = stringToDay start
    endDate = stringToDay end
    dates = takeWhile (<= endDate) $ iterate incrementDay startDate

-- Generate the API URL for a single day's schedule
scheduleUrl :: String -> String
scheduleUrl date = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date

-- Generate the API URL for live game status
gameStatusUrl :: Int -> String
gameStatusUrl gameId = "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live"

-- Generate the API URL for finished boxscore
boxScoreUrl :: Int -> String
boxScoreUrl gameId = "http://statsapi.mlb.com/api/v1/game/" ++ show gameId ++ "/boxscore"

-- Generate the API URL for specific years rosters
rosterUrl :: Int -> String
rosterUrl season = "https://statsapi.mlb.com/api/v1/sports/1/players?activeStatus=ACTIVE&season=" ++ show season

-- Generate the API URL for specific year's stat leaders in either batting or pitching (not working well)
seasonStatsUrl :: Int -> P.StatType -> String
seasonStatsUrl season statType = "http://statsapi.mlb.com/api/v1/stats?stats=season&sportId=1&season=" ++ show season ++ "&group=" ++ statTypeToString statType
