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
import qualified Data.ByteString.Lazy as BL

data GameSchedule where
  GameSchedule :: {dates :: [DateEntry]} -> GameSchedule
  deriving (Show, Eq)

data DateEntry where
  DateEntry :: {games :: Maybe (V.Vector Game)} -> DateEntry
  deriving (Show, Eq)

data Game where
  Game :: {gamePk :: Int} -> Game
  deriving (Show, Eq)

instance Data.Aeson.FromJSON GameSchedule where
    parseJSON = Data.Aeson.withObject "GameSchedule" $ \v -> GameSchedule
        <$> v Data.Aeson..: "dates"

instance Data.Aeson.FromJSON DateEntry where
    parseJSON = Data.Aeson.withObject "DateEntry" $ \v -> DateEntry
        <$> v Data.Aeson..:? "games"

instance Data.Aeson.FromJSON Game where
    parseJSON = Data.Aeson.withObject "Game" $ \v -> Game
        <$> v Data.Aeson..: "gamePk"

-- takes a date string "YYYY-MM-DD" and outputs a schedule bytestring of that day schdule
fetchGameScheduleForDate :: String -> IO (Maybe ByteString)
fetchGameScheduleForDate date = do
    let apiUrl = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date
    response <- httpBS (parseRequest_ apiUrl)
    return $ Just (getResponseBody response)

-- Takes a schedule bytestring and outputs true if games are happening, false otherwise.
hasGamesForDate :: ByteString -> Maybe Bool
hasGamesForDate jsonData =
    case eitherDecodeStrict jsonData :: Either String GameSchedule of
        Right schedule -> Just $ any (isJust . games) (dates schedule)
        Left _         -> Nothing

-- takes a schedule bytestring and outputs an array of gameId's or errors
-- Takes a schedule bytestring and outputs an array of gameId's or errors
extractGameIds :: ByteString -> Either String [Int]
extractGameIds jsonData =
    case eitherDecodeStrict jsonData :: Either String GameSchedule of
        Right gameData -> Right $ concatMap (maybe [] (V.toList . fmap gamePk) . games) (dates gameData)
        Left e -> Left e

-- string that indicates the status of the game
data GameStatus where
  GameStatus :: {codedGameState :: Text} -> GameStatus
  deriving (Show, Eq)

data GameDataWrapper where
  GameDataWrapper :: {gameData :: GameStatus} -> GameDataWrapper
  deriving (Show, Eq)

instance FromJSON GameStatus where
    parseJSON = withObject "GameStatus" $ \v -> GameStatus
        <$> v .: "codedGameState"

instance FromJSON GameDataWrapper where
    parseJSON = withObject "GameDataWrapper" $ \v -> GameDataWrapper
        <$> v .: "gameData"

fetchGameStatus :: Int -> IO (Maybe GameDataWrapper)
fetchGameStatus gameId = do
    let apiUrl = "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live"
    response <- httpBS (parseRequest_ apiUrl)
    case eitherDecodeStrict $ getResponseBody response of
        Right gameDataWrapper -> return $ Just gameDataWrapper
        Left _ -> return Nothing

fetchFinishedBxScore :: Int -> IO (Maybe ByteString)
fetchFinishedBxScore gameId = do
    gameStatus <- fetchGameStatus gameId
    case gameStatus of
        Just gameDataWrapper ->
            if codedGameState (gameData gameDataWrapper) == "F"
            then do
                fullGameData <- httpBS (parseRequest_ $ "http://statsapi.mlb.com/api/v1/game/" ++ show gameId ++ "/boxscore")
                return $ Just $ getResponseBody fullGameData
            else return Nothing
        Nothing -> return Nothing

--maps fetchFinishedBxScore over an array of game id's and returns IO (M.Map Int ByteString)
processGameIds :: [Int] -> IO (M.Map Int (Maybe ByteString))
processGameIds gameIds = do
    gameDataResponses <- mapM fetchFinishedBxScore gameIds
    return $ M.fromList $ zip gameIds gameDataResponses

-- 
printProcessedGameData :: M.Map Int (Maybe ByteString) -> IO ()
printProcessedGameData gameDataMap =
    mapM_ (\(gameId, gameData) -> case gameData of
        Just dataStr -> putStrLn $ show gameId ++ ": " ++ show dataStr
        Nothing -> putStrLn $ show gameId ++ " - No data available.") (M.toList gameDataMap)

-- print the schedule bytestring
processAndPrintGames :: ByteString -> IO ()
processAndPrintGames gameSchedule = 
    case hasGamesForDate gameSchedule of
        Just True -> 
            case extractGameIds gameSchedule of
                Left errMsg -> putStrLn $ "Error extracting game IDs: " ++ errMsg
                Right gameIds -> do
                    gameDataMap <- processGameIds gameIds
                    printProcessedGameData gameDataMap
        Just False -> putStrLn "No games scheduled for the provided date."
        Nothing    -> putStrLn "An error occurred while decoding the game schedule."
