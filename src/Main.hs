{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Scraper

main :: IO ()
main = do
    gameSchedule <- fetchGameScheduleForDate "2021-08-22"
    print gameSchedule

    let hasGames = hasGamesForDate gameSchedule
    print hasGames

    processAndPrintGames gameSchedule

