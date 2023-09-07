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
    gameSchedule <- fetchGameScheduleForDate "2023-08-22"

    -- Check if there are games on that date
    let hasGames = hasGamesForDate gameSchedule

    case hasGames of
        Just True -> do
            putStrLn "Games found for the date. Processing..."
            processAndPrintGames gameSchedule
        Just False -> putStrLn "No games for the specified date."
        Nothing   -> putStrLn "Error checking games for the date."
    
        -- Testing fetchFinishedBxScore
    putStrLn "\nFetching finished box score for game ID 716896..."
    fetchedBoxScore <- fetchFinishedBxScore 716896

    -- Compare fetchedBoxScore with the jsonData loaded from "testFiles/716896_boxscore.json"
    if fetchedBoxScore == jsonData
        then putStrLn "The fetched box score matches the local JSON!"
        else putStrLn "The fetched box score does not match the local JSON."