{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module InputSchemas
    ( flattenGameData
    ) where
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema
import Data.HashMap.Strict (union, fromList)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)



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
    players: List PlayerDetails
  }
|]

type PlayerDetails = [schema|
  {
    person: {
      id: Int,
      fullName: Text
    },
    parentTeamId: Int,
    allPositions: List {
      code: Text
    },
    status: {
      code: Text
    },
    stats: StatsType,
    seasonStats: SeasonStatsType
  }
|]

type StatsType = [schema|
  {
    batting: Maybe BattingStats,
    pitching: Maybe PitchingStats
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

type SeasonStatsType = [schema|
  {
    batting: Maybe SeasonBattingStats,
    pitching: Maybe SeasonPitchingStats
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

flattenGameData :: Object GameStats -> Int -> Map.Map Int PlayerDetails
flattenGameData gameData gameId = 
  let
    awayPlayers = [get| gameData.teams.away.players[] |]
    homePlayers = [get| gameData.teams.home.players[] |]
    allPlayers = awayPlayers ++ homePlayers

    transformPlayer :: PlayerDetails -> Maybe (Int, PlayerDetails)
    transformPlayer player 
      | null (fromMaybe [] $ playerDetailsAllPositions player) = Nothing
      | otherwise = 
        let
            playerId = playerDetailsPersonId $ playerDetailsPerson player
            positions = map ((read . T.unpack) . allPositionsCode) (playerDetailsAllPositions player)
            battingStats = fmap delFieldsBatting (playerDetailsStatsBatting $ playerDetailsStats player)
            pitchingStats = fmap delFieldsPitching (playerDetailsStatsPitching $ playerDetailsStats player)
            statsForGame = StatsType battingStats pitchingStats
            newPlayerDetails = PlayerDetails { 
                playerDetailsPerson = playerDetailsPerson player,
                playerDetailsParentTeamId = playerDetailsParentTeamId player,
                playerDetailsAllPositions = transformedPositions,
                playerDetailsStatus = playerDetailsStatus player,
                playerDetailsStats = statsForGame,
                playerDetailsSeasonStats = playerDetailsSeasonStats player
            }
        in Just (playerId, newPlayerDetails)

    delFieldsBatting :: BattingStats -> BattingStats
    delFieldsBatting stats = stats { note = "", summary = "", stolenBasePercentage = 0.0, atBatsPerHomeRun = 0.0 }

    delFieldsPitching :: PitchingStats -> PitchingStats
    delFieldsPitching stats = stats { note = "", summary = "", stolenBasePercentage = 0.0, strikePercentage = 0.0, homeRunsPer9 = 0.0, runsScoredPer9 = 0.0 }
  in
    Map.fromList $ catMaybes $ map transformPlayer allPlayers
