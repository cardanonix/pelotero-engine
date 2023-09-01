{-# LANGUAGE OverloadedStrings #-}

module Scraper
    ( fetchGameScheduleForDate
    -- , dateToEpoch
    , hasGamesForDate
    ) where

import Network.HTTP.Simple
import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString (ByteString)
import Data.Aeson
import qualified Data.Vector as V
import Data.Maybe (isJust)

data GameSchedule = GameSchedule {
    dates :: [DateEntry]
} deriving (Show, Eq)

data DateEntry = DateEntry {
    games :: Maybe (V.Vector Game)
} deriving (Show, Eq)

data Game = Game {
    -- Define any fields you need from the 'game' object
} deriving (Show, Eq)

instance FromJSON GameSchedule where
    parseJSON = withObject "GameSchedule" $ \v -> GameSchedule
        <$> v .: "dates"

instance FromJSON DateEntry where
    parseJSON = withObject "DateEntry" $ \v -> DateEntry
        <$> v .:? "games"

instance FromJSON Game where
    parseJSON = withObject "Game" $ \_ -> pure Game

fetchGameScheduleForDate :: String -> IO ByteString
fetchGameScheduleForDate date = do
    let apiUrl = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date
    response <- httpBS (parseRequest_ apiUrl)
    return $ getResponseBody response

hasGamesForDate :: ByteString -> Maybe Bool
hasGamesForDate jsonData =
    case eitherDecodeStrict jsonData :: Either String GameSchedule of
        Right schedule -> Just $ any (isJust . games) (dates schedule)
        Left _ -> Nothing
    
-- getGameIds :: Aeson.Value -> [Text]
-- getGameIds json = json ^.. key "dates" . values . key "games" . values . key "gamePk" . _Array

-- checkGameStatus :: Text -> IO Text
-- checkGameStatus gameId = do
--     response <- get ("https://statsapi.mlb.com//api/v1.1/game/" <> show gameId <> "/feed/live")
--     let status = fromJust $ response ^. responseBody . key "gameData" . key "status" . key "codedGameState" . Aeson._String
--     return status

-- downloadAndCompressGameData :: Text -> IO Aeson.Value
-- downloadAndCompressGameData gameId = do
--     response <- get ("http://statsapi.mlb.com/api/v1/game/" <> show gameId <> "/boxscore")
--     -- Here, I'm assuming you have a function `flattenGameData` in Haskell similar to your Bash code
--     let compressedData = flattenGameData (response ^. responseBody) gameId
--     return compressedData

-- processGames :: [Text] -> IO [(Text, Text)]
-- processGames gameIds = do
--     daySummedData <- pure Aeson.emptyObject
--     gameStatuses <- mapM (\gameId -> do
--         status <- checkGameStatus gameId
--         if status == "F" then do
--             compressedData <- downloadAndCompressGameData gameId
--             daySummedData <- pure $ mergeData daySummedData compressedData
--             return (gameId, status)
--         else
--             return (gameId, status)
--         ) gameIds
--     return gameStatuses

-- mergeData :: Aeson.Value -> Aeson.Value -> Aeson.Value
-- mergeData = HM.union -- assuming Aeson.Value is Object


-- fetchGameData :: UTCTime -> UTCTime -> IO ()
-- fetchGameData start end = do
--     manager <- newManager tlsManagerSettings
--     go start
--   where
--     go currentDate
--         | currentDate > end = pure ()
--         | otherwise = do
--             gameData <- getGameData manager currentDate
--             print gameData -- Do whatever you need with gameData
--             go (addUTCTime (fromIntegral (24*60*60)) currentDate)  -- Move to the next day

-- getGameData :: Manager -> UTCTime -> IO (Maybe Value)
-- getGameData manager date = do
--     initialRequest <- parseRequest $ "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ formatTime defaultTimeLocale "%Y-%m-%d" date
--     response <- httpLbs initialRequest manager
--     let jsonBody = responseBody response
--     case eitherDecode jsonBody of
--         Left err -> putStrLn err >> pure Nothing
--         Right json -> flattenGameData json

-- flattenGameData :: Value -> IO (Maybe Value)
-- flattenGameData jsonData = do
--     -- Implement the flattenGameData logic here similar to the Bash function
--     -- You'll likely want to manipulate the JSON structure using Aeson's functionality
--     -- The Bash script used `jq`, but Aeson provides a Haskell-native way to work with JSON.
--     -- You can extract values, modify them, and create new JSON structures.
--     pure Nothing  -- Replace with actual implementation