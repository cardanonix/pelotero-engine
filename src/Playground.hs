{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DoAndIfThenElse #-}


module Main (main) where


import Network.HTTP.Simple
    ( parseRequest_,
      getResponseBody,
      httpBS )
import Data.Time
    ( Day, addDays, diffDays, parseTimeOrError, defaultTimeLocale, formatTime )
import Data.Time.Clock.POSIX ()
import Data.Time.Clock ()
import Data.Time.Format ()
import Data.ByteString (ByteString, empty)
import qualified Data.Vector as V
import Data.Maybe (isJust, fromMaybe, mapMaybe, maybeToList)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
    ( eitherDecode,
      encode,
      (.:), (.:?),
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
import Crypto.Hash ( hashWith, SHA256(SHA256) )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Control.Monad (when, filterM)
import Control.Concurrent.Async (mapConcurrently)


import qualified Config as C
import qualified Points as P
import qualified Input as I
import qualified Leaderboard as L
import qualified Middle as MI
import qualified OfficialRoster as O 
import qualified Roster as R  
import Validators -- contains a lot of functionality
import Scraper

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Match Funded by Two Managers (Can be Randomly Paired or Chosen)"
    putStrLn ""
    putStrLn ""
    putStrLn "Verify Player 1 & 2 Roster"
    putStrLn "Interactive Validation of League Paramaters"
    putStrLn ""
    putStrLn "Scrape Active Rosters"
    putStrLn "Interactive Manager Player-Ranking for Auto draft"
    putStrLn "Match Funded by Two Players which finalizes everything"
    putStrLn "Auto-Draft"
    putStrLn ""
    putStrLn "Verify Player 1 & 2 Rosters"
    putStrLn ""
    putStrLn "Set Lineup"
    putStrLn "Verify Player 1 & 2 Lineup"
    putStrLn "  (players are able to edit their lineup until a specified time Before the natch begins)"
    putStrLn ""
    putStrLn "Wait for Match-Timeframe to Expire"
    putStrLn "Fetch Updated Stats"
    putStrLn ""
    putStrLn "Calculate & compare Player 1 & 2 Points in TimeFrame"
    putStrLn ""
    putStrLn "Disbursing Winnings to Higher Score"

{- 

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
fetchFinishedBxScore :: Int -> IO (Either String (Maybe L.GameData))
fetchFinishedBxScore gameId = do
    gameStatusResult <- fetchGameStatus gameId
    case gameStatusResult of
        Right gameDataWrapper -> do
            let liveStatusWrapper = gameData gameDataWrapper
            let liveStatus = gameStatus liveStatusWrapper
            if codedGameState liveStatus == "F"
               then do
                   boxscoreResult <- fetchAndDecodeJSON (boxScoreUrl gameId)
                   return $ fmap (Just . assignGameIdToPlayers gameId) boxscoreResult -- *adds gameId attribute to corresponding stats
               else return $ Right Nothing
        Left err -> return $ Left ("Error fetching game status: " ++ err)


-- -- [B] list of gameIds -> C status checks -> [D] list of boxscores
-- fetchFinishedBxScores :: [Int] -> IO (Either String (M.Map Int L.GameData))
fetchFinishedBxScores :: [Int] -> IO (Either String (M.Map Int (Maybe L.GameData)))
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
fetchFinishedBxScoresToJsonPlayerSeasonStats :: [Int] -> IO (Either String (M.Map Text MI.JsonPlayerData))
fetchFinishedBxScoresToJsonPlayerSeasonStats gameIds = do
    gameDataResult <- fetchFinishedBxScores gameIds
    return $ fmap convertGameDataMapToJsonPlayerSeasonData gameDataResult

-- Fetch and decode utility
fetchAndDecodeJSON :: FromJSON a => String -> IO (Either String a)
fetchAndDecodeJSON url = do
    response <- httpBS (parseRequest_ url)
    return $ eitherDecodeStrict $ getResponseBody response

convertGameDataMapToJsonPlayerSeasonData :: M.Map Int (Maybe L.GameData) -> M.Map Text MI.JsonPlayerData
convertGameDataMapToJsonPlayerSeasonData maybeGameDataMap =
    foldl mergePlayerData M.empty allPlayerDataPairs
  where
    allPlayerDataPairs :: [(Text, MI.JsonPlayerData)]
    allPlayerDataPairs = concatMap gameDataToPlayerDataPairs (mapMaybe id (M.elems maybeGameDataMap))

    gameDataToPlayerDataPairs :: L.GameData -> [(Text, MI.JsonPlayerData)]
    gameDataToPlayerDataPairs gameData =
        let awayPlayers = M.elems $ L.players $ L.away $ L.teams gameData
            homePlayers = M.elems $ L.players $ L.home $ L.teams gameData
            allPlayers = awayPlayers ++ homePlayers
        in map (\player -> (rawStringToText $ MI.playerId (playerToJsonPlayerSeasonData player), playerToJsonPlayerSeasonData player)) allPlayers

    rawStringToText :: Text -> Text
    rawStringToText = T.replace "\\\"" "\"" . T.replace "\\\\" "\\"

    mergePlayerData :: M.Map Text MI.JsonPlayerData -> (Text, MI.JsonPlayerData) -> M.Map Text MI.JsonPlayerData
    mergePlayerData acc (playerId, newPlayerData) =
        let mergedData = case M.lookup playerId acc of
                Just existingPlayerData -> mergeJsonPlayerSeasonData existingPlayerData newPlayerData
                Nothing                 -> newPlayerData
        in M.insert playerId mergedData acc

mergeJsonPlayerSeasonData :: MI.JsonPlayerData -> MI.JsonPlayerData -> MI.JsonPlayerData
mergeJsonPlayerSeasonData existing new = 
    MI.JsonPlayerData
        { MI.playerId = MI.playerId existing  -- assuming playerIds are the same, else there's a bigger problem!
        , MI.fullName = MI.fullName existing  -- assuming fullNames are the same
        , MI.stats = M.unionWith mergeJsonSeasonStatsData (MI.stats existing) (MI.stats new)
        }

mergeJsonSeasonStatsData :: MI.JsonStatsData -> MI.JsonStatsData -> MI.JsonStatsData
mergeJsonSeasonStatsData _ new = new

playerToJsonPlayerSeasonData :: L.Player -> MI.JsonPlayerData
playerToJsonPlayerSeasonData p =
    MI.JsonPlayerData
        { MI.playerId = T.pack $ show $ L.personId (L.person p)
        , MI.fullName = L.fullName (L.person p)
        , MI.stats = M.singleton (maybe "" (T.pack . show) (L.gameid p)) (playerToJsonSeasonStatsData p)
        }

playerToJsonSeasonStatsData :: L.Player -> MI.JsonStatsData
playerToJsonSeasonStatsData p =
    MI.JsonStatsData
        { MI.parentTeamId = L.parentTeamId p
        , MI.allPositions = fromMaybe [] (L.allPositions p)
        , MI.statusCode = L.status_code (L.status p)
        , MI.batting = L.batting (L.stats p)
        , MI.pitching = L.pitching (L.stats p)
        }

convertPlayerToJson :: L.Player -> ByteString
convertPlayerToJson = BL.toStrict . encode . playerToJsonPlayerSeasonData

-- unnnecessary for season stats
assignGameIdToPlayers :: Int -> L.GameData -> L.GameData
assignGameIdToPlayers gameId gameData =
    let assignToTeam team = team { L.players = M.map assignToPlayer (players team) }
        assignToPlayer player = player { L.gameid = Just gameId }
    in gameData { L.teams = (teams gameData) { L.away = assignToTeam (L.away (teams gameData)),
                                             L.home = assignToTeam (L.home (teams gameData)) } }

-- ## Output Stuff ##
-- we need to modify this to use the player list to find every single player's stats for the season
-- get player list and put it into an array of tuples with the second element being a Maybe
-- scan the seasonstats and pull only the most recent set of seasonstats into the comprehensive cumulative stat list
- write to a json file where player id's are the keys and the objects are the season stats and other LeaderboardInfo
processDate :: String -> IO ()
processDate date = do
    putStrLn $ "Processing " ++ date
    scheduleResult <- fetchGameScheduleForDate date
    processAndPrintGames scheduleResult
    case scheduleResult of
        Left err -> putStrLn $ "Failed to fetch game schedule: " ++ err
        Right schedule -> do
            let gameIds = extractGameIds schedule
            flattenedPlayersResult <- fetchFinishedBxScoresToJsonPlayerSeasonStats gameIds
            case flattenedPlayersResult of
                Left err -> putStrLn $ "Failed to process JSON: " ++ err
                Right _flattenedPlayers -> do
                    let filename = formatFilename date
                    writeDataToFile filename "appData/stats" _flattenedPlayers

-- Main scraper function tying everything together
scrapeDataForDateRange :: String -> String -> IO ()
scrapeDataForDateRange start end = do
    mapM_ processDate (generateDateRange start end)

flattenedPlayersList :: M.Map Text MI.JsonPlayerData -> M.Map Text MI.JsonPlayerData
flattenedPlayersList = id  -- or simply remove this function and use the map directly

-- takes a list of tuples game id's and game data and prints them
printGameData :: Either String (M.Map Int L.GameData) -> IO ()
printGameData gameDataMapEither =
    withEither (return gameDataMapEither) $ \gameDataMap ->
        mapM_ (\(gameId, gameData) -> putStrLn $ show gameId ++ ": " ++ show gameData) (M.toList gameDataMap)

processAndPrintGames :: Either String I.GameSchedule -> IO ()
processAndPrintGames gameScheduleEither =
    withEither (return gameScheduleEither) $ \gameSchedule ->
        if hasGamesForDate gameSchedule then do
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

-- Edge Cases Handling
-- monadic error handling for fetching and decoding
withEither :: IO (Either String a) -> (a -> IO ()) -> IO ()
withEither action successHandler = do
    result <- action
    case result of
        Left err       -> putStrLn err
        Right dataPacket -> successHandler dataPacket

-- Special Enhancement of fromJSON types that gets called as post-processing in the fetch functions
assignDateToSchedule :: Text -> GameSchedule -> GameSchedule
assignDateToSchedule date schedule =
    let assignToDateEntry entry = entry { games = fmap (V.map assignToDate) (games entry) }
        assignToDate gameID = gameID { game_date = Just date }
    in schedule { dates = map assignToDateEntry (dates schedule) }


-- Takes a schedule bytestring and outputs true if games are happening, false otherwise.
hasGamesForDate :: I.GameSchedule -> Bool
hasGamesForDate schedule = any (isJust . games) (dates schedule)

-- Takes a schedule bytestring and outputs an array of gameId's or errors
extractGameIds :: I.GameSchedule -> [Int]
extractGameIds gameData = concatMap (maybe [] (V.toList . fmap gamePk) . games) (dates gameData)

-- ## FileName Manipulation Stuff 
-- Takes a filename, path, and the data to save, then writes to a JSON file at the specified path with the given filename.
writeDataToFile :: FilePath -> FilePath -> M.Map Text MI.JsonPlayerData -> IO ()
writeDataToFile filename path dataToSave = do
    createOutputDirectory path
    let fullpath = path ++ "/" ++ filename
    BL.writeFile fullpath (encode dataToSave)
    
-- Write Player to JSON File
writePlayerToJsonFile :: FilePath -> L.Player -> IO ()
writePlayerToJsonFile path player = B.writeFile path (convertPlayerToJson player)


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
seasonStatsUrl :: Int -> D.StatType -> String 
seasonStatsUrl season statType = "http://statsapi.mlb.com/api/v1/stats?stats=season&sportId=1&season=" ++ show season ++ "&group=" ++ D.statTypeToString statType

computeChecksum :: BL.ByteString -> Text
computeChecksum bs = T.pack . show . hashWith SHA256 $ BL.toStrict bs

getCurrentDate :: IO Text
getCurrentDate = T.pack . formatTime defaultTimeLocale "%Y_%m_%d_%H_%M" <$> getCurrentTime

 -}