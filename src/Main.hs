{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}


module Main (main) where


import Data.Aeson (eitherDecodeFileStrict, toJSON)
import Data.Aeson.Schema
import Data.HashMap.Strict (union, fromList)
import qualified Data.Text as T
-- import Scraper
    -- ( fetchGameScheduleForDate, hasGamesForDate)
-- import InputSchemas
-- import OutputSchemas

type Boxscore = [schema|
  {
    teams: {
      away: {
        players: List Player
      },
      home: {
        players: List PLayer
      }, 
    }, 
  }
|]

type Player = [schema|
  {
    person: List {
      id: Int,
      fullName: Text,
    },
    parentTeamId: Int,
    allPositions: Maybe List {
      code: Text,
    },
    status: {
      code: Text,
    },
    stats: List {
        batting: Maybe BattingStats,
        pitching: Maybe PitchingStats,
    },
  }
|]

type BattingStats = [schema|
  {
    gamesPlayed: Int,
    flyOuts: Int,
    groundOuts: Int,
    runs: Int,
    doubles: Int,
    triples: Int,
    homeRuns: Int,
    strikeOuts: Int,
    baseOnBalls: Int,
    intentionalWalks: Int,
    hits: Int,
    hitByPitch: Int,
    atBats: Int,
    caughtStealing: Int,
    stolenBases: Int,
    groundIntoDoublePlay: Int,
    groundIntoTriplePlay: Int,
    plateAppearances: Int,
    totalBases: Int,
    rbi: Int,
    leftOnBase: Int,
    sacBunts: Int,
    sacFlies: Int,
    catchersInterference: Int,
    pickoffs: Int,
  }
|]

type PitchingStats = [schema|
  {
    gamesPlayed: Int,
    gamesStarted: Int,
    groundOuts: Int,
    airOuts: Int,
    runs: Int,
    doubles: Int,
    triples: Int,
    homeRuns: Int,
    strikeOuts: Int,
    baseOnBalls: Int,
    intentionalWalks: Int,
    hits: Int,
    hitByPitch: Int,
    atBats: Int,
    caughtStealing: Int,
    stolenBases: Int,
    numberOfPitches: Int,
    inningsPitched: Text,
    wins: Int,
    losses: Int,
    saves: Int,
    saveOpportunities: Int,
    holds: Int,
    blownSaves: Int,
    earnedRuns: Int,
    battersFaced: Int,
    outs: Int,
    gamesPitched: Int,
    completeGames: Int,
    shutouts: Int,
    pitchesThrown: Int,
    balls: Int,
    strikes: Int,
    hitBatsmen: Int,
    balks: Int,
    wildPitches: Int,
    pickoffs: Int,
    gamesFinished: Int,
    inheritedRunners: Int,
    inheritedRunnersScored: Int,
    catchersInterference: Int,
    sacBunts: Int,
    sacFlies: Int,
    passedBall: Int,
  }
|]


main :: IO ()
main = do
    -- gameSchedule <- fetchGameScheduleForDate "2023-08-22"
    -- print gameSchedule

    -- let hasGames = hasGamesForDate gameSchedule
    -- print hasGames

  -- Then, load data from a file
  obj <- either fail return =<<
    eitherDecodeFileStrict "testFiles/716896_boxscore.json" :: IO (Object Boxscore)
       
  -- print all the users' ids
  
  print [get| obj.teams.away.players.stats[] |]
  print [get| obj.teams.home.players.stats[] |]

