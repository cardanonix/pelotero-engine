{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}

module BoxScoreScraper
    ( fetchGameScheduleForDate
    , hasGamesForDate
    , extractGameIds
    , processAndPrintGames
    , fetchFinishedBxScore
    , fetchGameStatus
    , GameSchedule
    , DateEntry
    , GameID
    , LiveGameStatus
    , LiveGameWrapper
    ) where

import Network.HTTP.Simple
    ( parseRequest_, getResponseBody, httpBS )
import Data.Time ()
import Data.Time.Clock.POSIX ()
import Data.ByteString (ByteString, empty)
import qualified Data.Vector as V
import Data.Maybe (isJust, fromMaybe, maybeToList)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Aeson
    ( eitherDecode,
      eitherDecodeStrict,
      encode,
      (.:),
      (.:?),
      genericParseJSON,
      withObject,
      defaultOptions,
      FromJSON(parseJSON),
      Options(fieldLabelModifier) )
import GHC.Generics ( Generic )
import Network.HTTP.Simple (httpBS, parseRequest_, getResponseBody)
import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified InputADT as IDT
import InputADT
    ( GameData
    , LiveGameStatusWrapper(gameStatus)
    , LiveGameWrapper(gameData)
    , LiveGameStatus(codedGameState)
    , DateEntry(games)
    , GameSchedule(dates)
    , GameID(gamePk) 
    )

-- takes a date string "YYYY-MM-DD" and outputs a schedule bytestring of that day schdule
fetchGameScheduleForDate :: String -> IO (Either String IDT.GameSchedule)
fetchGameScheduleForDate date = fetchAndDecode (scheduleUrl date)

-- Generate the API URL for a single day's schedule
scheduleUrl :: String -> String
scheduleUrl date = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date

-- Takes a schedule bytestring and outputs true if games are happening, false otherwise.
hasGamesForDate :: IDT.GameSchedule -> Bool
hasGamesForDate schedule = any (isJust . games) (dates schedule)

-- Takes a schedule bytestring and outputs an array of gameId's or errors
extractGameIds :: IDT.GameSchedule -> [Int]
extractGameIds gameData = concatMap (maybe [] (V.toList . fmap gamePk) . games) (dates gameData)

-- Generate the API URL for live game status
gameStatusUrl :: Int -> String
gameStatusUrl gameId = "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live"

-- Generate the API URL for finished boxscore
boxScoreUrl :: Int -> String
boxScoreUrl gameId = "http://statsapi.mlb.com/api/v1/game/" ++ show gameId ++ "/boxscore"

-- Fetch and decode utility
fetchAndDecode :: FromJSON a => String -> IO (Either String a)
fetchAndDecode url = do
    response <- httpBS (parseRequest_ url)
    return $ eitherDecodeStrict $ getResponseBody response

fetchGameStatus :: Int -> IO (Either String IDT.LiveGameWrapper)
fetchGameStatus gameId = fetchAndDecode (gameStatusUrl gameId)

fetchFinishedBxScore :: Int -> IO (Either String IDT.GameData)
fetchFinishedBxScore gameId = do
    gameStatusResult <- fetchGameStatus gameId
    case gameStatusResult of
        Right gameDataWrapper -> do
            let liveStatusWrapper = gameData gameDataWrapper
            let liveStatus = gameStatus liveStatusWrapper
            if codedGameState liveStatus == "F"
               then fetchAndDecode (boxScoreUrl gameId)
               else return $ Left "Game isn't finished yet"
        Left err -> return $ Left ("Error fetching game status: " ++ err)

--maps fetchFinishedBxScore over an array of game id's and returns IO (M.Map Int ByteString)
processGameIds :: [Int] -> IO (Either String (M.Map Int IDT.GameData))
processGameIds gameIds = do
    results <- mapM fetchFinishedBxScore gameIds
    case sequence results of
        Left err -> return $ Left err
        Right gameDataList -> return $ Right $ M.fromList $ zip gameIds gameDataList

-- takes a map of game id's and game data and prints the game data
printProcessedGameData :: Either String (M.Map Int IDT.GameData) -> IO ()
printProcessedGameData gameDataMapEither =
    case gameDataMapEither of
        Left errMsg -> putStrLn errMsg
        Right gameDataMap -> mapM_ (\(gameId, gameData) -> putStrLn $ show gameId ++ ": " ++ show gameData) (M.toList gameDataMap)

-- print the schedule bytestring
processAndPrintGames :: Either String IDT.GameSchedule -> IO ()
processAndPrintGames gameScheduleEither = 
    case gameScheduleEither of
        Left errMsg -> putStrLn errMsg
        Right gameSchedule -> 
            if hasGamesForDate gameSchedule
            then do
                let gameIds = extractGameIds gameSchedule
                gameDataMap <- processGameIds gameIds
                printProcessedGameData gameDataMap
            else putStrLn "No games scheduled for the provided date."