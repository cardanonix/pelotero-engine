{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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

    -- processAndPrintGames gameSchedule

