{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}


module OutputSchemas
    ( -- DailyStats
    -- , GamesStatus
    -- , PlayerData
    -- , Batting
    -- , Pitching
    -- , GamesStatus
    , 
    ) where

import Prelude hiding (id)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema

-- Schema Describing Transformed Daily Stats

type DailyStats = [schema|
  {
    playerData: Map Text PlayerData,
    games: Map Text GamesStatus, 
    checksum: Text,
    date: Text
  }
|]

type GamesStatus = [schema|
  {
    id: Text
  }
|]

type PlayerData = [schema|
  {
    id: Int,
    fullName: Text,
    stats: Map Text GameStats
  }
|]

type GameStats = [schema|
  {
    parentTeamId: Int,
    allPositions: List Int,
    status: Text,
    batting: Batting,
    pitching: Pitching
  }
|]

type Batting = [schema|
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
    pickoffs: Int
  }
|]

type Pitching = [schema|
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
    inningsPitched: Float,
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
    passedBall: Int
  }
|]

-- Schema Describing Transformed Roster List
type RosterList = [schema|
  {
    playerInfo: Map Text PlayerInfo
    dataPulled: Text,
    checksum: Text
  }
|]

type PlayerInfo = [schema|
  {
    id: Int,
    useName: Text,
    useLastName: Text,
    nameSlug: Text,,
    currentTeam: Int,
    primaryPosition: Text,
    batSide: Text,
    pitchHand: Text,
    active: Bool
  }
|]

