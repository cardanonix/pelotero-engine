{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}


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
    -- gameSchedule <- fetchGameScheduleForDate "2023-08-22"
    -- print gameSchedule

  obj <- either fail return =<<
    eitherDecodeFileStrict "testFiles/716896_boxscore.json" :: IO (Object Boxscore)
  
  print obj
       
  -- print all the users' ids
  
  print [get| obj.teams.away.players.value.person.fullName |]
  print [get| obj.teams.home.players.value.person.fullName |]
