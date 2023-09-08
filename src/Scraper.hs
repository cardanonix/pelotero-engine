{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Data.ByteString ( ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString as B (readFile)
import Data.Aeson (decode, Result(Success), FromJSON(..), Value, (.:), (.:?), (.!=), fromJSON, withObject, eitherDecodeStrict)
import Data.Aeson.Types (Parser, Result(..))
import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShowM)
import InputADT
import BoxScoreScraper  ( fetchGameScheduleForDate
                        , hasGamesForDate
                        , extractGameIds
                        , processAndPrintGames
                        , fetchFinishedBxScore
                        , fetchGameStatus
                        )

main :: IO ()
main = do
    jsonData <- B.readFile "testFiles/716896_boxscore.json"
    let parsedResult = eitherDecodeStrict jsonData :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
    handPicked <- B.readFile "testFiles/shortened.json"
    let parsedResult = eitherDecodeStrict handPicked :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
    
    -- testing BoxscoreScraper
    -- Fetch game schedule for a specific date, e.g., "2023-09-05"
    gameScheduleMaybe <- fetchGameScheduleForDate "2023-08-22"
    
    let hasGames = hasGamesForDate <$> gameScheduleMaybe

    case hasGames of
        Just (Just True) -> do
            putStrLn "Games found for the date. Processing..."
            -- Ensure you handle the case when `gameScheduleMaybe` is Nothing or contains an error.
            case gameScheduleMaybe of
                Just gameSchedule -> processAndPrintGames gameSchedule
                Nothing -> putStrLn "Error processing the game schedule."
        Just (Just False) -> putStrLn "No games for the specified date."
        Just Nothing -> putStrLn "Error in decoding game schedule."
        Nothing -> putStrLn "Error fetching game schedule."
        
    _ <- fetchGameStatus 716896 
    putStrLn "That file is huge!"

    -- Load and parse local JSON
    jsonData <- B.readFile "testFiles/716896_boxscore.json"
    let parsedLocal = eitherDecodeStrict jsonData :: Either String GameData

    -- Fetch and parse fetched JSON
    putStrLn "\nFetching finished box score for game ID 716896..."
    fetchedBoxScoreMaybe <- fetchFinishedBxScore 716896

    let fetchedBoxScoreEither :: Either String ByteString
        fetchedBoxScoreEither = case fetchedBoxScoreMaybe of
            Nothing -> Left "No box score fetched for the game or the game hasn't finished yet."
            Just boxScore -> Right boxScore

    case fetchedBoxScoreEither of
        Left errMsg -> putStrLn errMsg
        Right fetchedBoxScore -> do
            let parsedFetched = eitherDecodeStrict fetchedBoxScore :: Either String GameData
            case (parsedLocal, parsedFetched) of
                (Left localError, Left fetchedError) -> do
                    putStrLn $ "Error decoding local JSON: " ++ localError
                    putStrLn $ "Error decoding fetched JSON: " ++ fetchedError
                (Left localError, _) -> 
                    putStrLn $ "Error decoding local JSON: " ++ localError
                (_, Left fetchedError) -> 
                    putStrLn $ "Error decoding fetched JSON: " ++ fetchedError
                (Right localData, Right fetchedData) -> 
                    if localData == fetchedData
                        then putStrLn "The fetched game data matches the local data!"
                        else putStrLn "The fetched game data does not match the local data."