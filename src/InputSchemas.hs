{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}


module InputSchemas
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

type GameStats = [schema|
  {
    teams: Teams
  }
|]

type Teams = [schema|
  {
    away: TeamDetails,
    home: TeamDetails
  }
|]

type TeamDetails = [schema|
  {
    players: Map Text PlayerDetails
  }
|]

type PlayerDetails = [schema|
  {
    person: {
      id: Int,
      fullName: Text
    },
    parentTeamId: Int,
    allPositions: Map Text Position
    status: {
      code: Text
    },
    stats: {
      batting: Maybe BattingStats,
      pitching: Maybe PitchingStats
    }, 
    seasonStats: {
      batting: Maybe SeasonBattingStats,
      pitching: Maybe SeasonPitchingStats
    }
  }
|]

type Position = [schema|
  {
    code: Text
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
    note: Text,
    summary: Text,
    stolenBasePercentage: Float,
    atBatsPerHomeRun: Float
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
    note: Text,
    summary: Text,
    stolenBasePercentage: Float,
    strikePercentage: Float,
    homeRunsPer9: Float,
    runsScoredPer9: Float
  }
|]

type SeasonBattingStats = [schema|
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
    avg: Text,
    atBats: Int,
    obp: Text,
    slg: Text,
    ops: Text,
    caughtStealing: Int,
    stolenBases: Int,
    stolenBasePercentage: Text,
    groundIntoDoublePlay: Int,
    groundIntoTriplePlay: Int,
    plateAppearances: Int,
    totalBases: Int,
    rbi: Int,
    leftOnBase: Int,
    sacBunts: Int,
    sacFlies: Int,
    babip: Text,
    catchersInterference: Int,
    pickoffs: Int,
    atBatsPerHomeRun: Text
  }
|]

type SeasonPitchingStats = [schema|
  {
    gamesPlayed: Int,
    gamesStarted: Int,
    flyOuts: Int,
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
    obp: Text,
    caughtStealing: Int,
    stolenBases: Int,
    stolenBasePercentage: Text,
    numberOfPitches: Int,
    era: Text,
    inningsPitched: Text,
    wins: Int,
    losses: Int,
    saves: Int,
    saveOpportunities: Int,
    holds: Int,
    blownSaves: Int,
    earnedRuns: Int,
    whip: Text,
    battersFaced: Int,
    outs: Int,
    gamesPitched: Int,
    completeGames: Int,
    shutouts: Int,
    pitchesThrown: Int,
    balls: Int,
    strikes: Int,
    strikePercentage: Text,
    hitBatsmen: Int,
    balks: Int,
    wildPitches: Int,
    pickoffs: Int,
    groundOutsToAirouts: Text,
    rbi: Int,
    winPercentage: Text,
    pitchesPerInning: Text,
    gamesFinished: Int,
    strikeoutWalkRatio: Text,
    strikeoutsPer9Inn: Text,
    walksPer9Inn: Text,
    hitsPer9Inn: Text,
    runsScoredPer9: Text,
    homeRunsPer9: Text,
    inheritedRunners: Int,
    inheritedRunnersScored: Int,
    catchersInterference: Int,
    sacBunts: Int,
    sacFlies: Int,
    passedBall: Int
  }
|]
