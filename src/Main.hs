{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}


module Main (main) where


import Data.Aeson (eitherDecodeFileStrict, toJSON)
import Data.Aeson.Schema
import Data.HashMap.Strict (union, fromList)
import qualified Data.Text as T
import Scraper
    ( fetchGameScheduleForDate, hasGamesForDate)
import InputSchemas
import OutputSchemas

main :: IO ()
main = do
    gameSchedule <- fetchGameScheduleForDate "2023-08-22"
    print gameSchedule

    let hasGames = hasGamesForDate gameSchedule
    print hasGames

    -- gameStats <- either fail return =<< eitherDecodeFileStrict "path_to_input.json" :: IO (Object GameStats)
    -- let gameId = 123 -- Replace this with how you get the game id
    --     flattenedData = flattenGameData gameStats gameId
    -- print flattenedData

