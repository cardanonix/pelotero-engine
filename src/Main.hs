{-# LANGUAGE OverloadedStrings #-}
import Scraper

module Main (main) where

main :: IO ()
main = do
    -- Test the initial function:
    gameSchedule <- fetchGameScheduleForDate "2021-08-22" "2021-08-23"
    print gameSchedule

    -- Test other parts:
    -- let gameIds = getGameIds gameSchedule
    -- statuses <- processGames gameIds
    -- print statuses


