{-# LANGUAGE OverloadedStrings #-}
module Scraper
    ( fetchGameScheduleForDate
    , getGameIds
    , checkGameStatus
    , downloadAndCompressGameData
    , processGames
    ) where
    
import System.Environment
import System.Process
import Control.Monad (when, forM)
import Data.Aeson
import Data.Aeson.Lens (_Object, key, _Array, _String, _Bool)
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Simple    
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

fetchGameScheduleForDate :: String -> String -> IO ByteString
fetchGameScheduleForDate start_date end_date = do
    let api_url = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" 
                  ++ start_date 
                  ++ "&endDate=" 
                  ++ end_date
    response <- httpBS (parseRequest_ api_url)
    return $ getResponseBody response

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

mergeData :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeData = HM.union -- assuming Aeson.Value is Object


fetchGameData :: UTCTime -> UTCTime -> IO ()
fetchGameData start end = do
    manager <- newManager tlsManagerSettings
    go start
  where
    go currentDate
        | currentDate > end = pure ()
        | otherwise = do
            gameData <- getGameData manager currentDate
            print gameData -- Do whatever you need with gameData
            go (addUTCTime (fromIntegral (24*60*60)) currentDate)  -- Move to the next day

getGameData :: Manager -> UTCTime -> IO (Maybe Value)
getGameData manager date = do
    initialRequest <- parseRequest $ "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ formatTime defaultTimeLocale "%Y-%m-%d" date
    response <- httpLbs initialRequest manager
    let jsonBody = responseBody response
    case eitherDecode jsonBody of
        Left err -> putStrLn err >> pure Nothing
        Right json -> flattenGameData json

flattenGameData :: Value -> IO (Maybe Value)
flattenGameData jsonData = do
    -- Implement the flattenGameData logic here similar to the Bash function
    -- You'll likely want to manipulate the JSON structure using Aeson's functionality
    -- The Bash script used `jq`, but Aeson provides a Haskell-native way to work with JSON.
    -- You can extract values, modify them, and create new JSON structures.
    pure Nothing  -- Replace with actual implementation