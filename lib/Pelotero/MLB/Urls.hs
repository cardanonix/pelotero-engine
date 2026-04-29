-- | MLB Stats API endpoint URLs. Isolated here so swapping providers
-- means dropping this module rather than grep-replacing URLs.
module Pelotero.MLB.Urls
  ( rosterUrl
  , teamsUrl
  , scheduleUrl
  , scheduleDateUrl
  , boxscoreUrl
  , gameStatusUrl
  ) where

rosterUrl :: Int -> String
rosterUrl season =
  "https://statsapi.mlb.com/api/v1/sports/1/players?activeStatus=ACTIVE&season="
    <> show season

teamsUrl :: Int -> String
teamsUrl season =
  "https://statsapi.mlb.com/api/v1/teams?sportId=1&season=" <> show season

scheduleUrl :: String -> String -> String
scheduleUrl startDate endDate =
  "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate="
    <> startDate <> "&endDate=" <> endDate

scheduleDateUrl :: String -> String
scheduleDateUrl date = scheduleUrl date date

boxscoreUrl :: Int -> String
boxscoreUrl gamePk =
  "https://statsapi.mlb.com/api/v1/game/" <> show gamePk <> "/boxscore"

gameStatusUrl :: Int -> String
gameStatusUrl gamePk =
  "https://statsapi.mlb.com/api/v1.1/game/" <> show gamePk <> "/feed/live"