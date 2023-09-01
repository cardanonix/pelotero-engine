{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment
import System.Process
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Simple

dateToEpoch :: String -> IO Integer
dateToEpoch date = do
    day <- parseTimeM True defaultTimeLocale "%F" date :: IO Day
    return $ floor $ utcTimeToPOSIXSeconds (UTCTime day 0)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [startDate, endDate] -> do
            startTs <- dateToEpoch startDate
            endTs <- dateToEpoch endDate
            currentTs <- dateToEpoch startDate

            putStrLn $ "Start TS: " ++ show startTs
            putStrLn $ "End TS: " ++ show endTs
            putStrLn $ "Current TS: " ++ show currentTs

            let apiUrl = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ startDate ++ "&endDate=" ++ startDate
            response <- httpBS (parseRequest_ apiUrl)
            let gameDataJson = getResponseBody response

            print gameDataJson

        _ -> putStrLn "Usage: program start_date end_date"


