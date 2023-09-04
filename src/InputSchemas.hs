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
    teams:   {
      away:   {
        players: List Player
      },
      home:   {
        players: List Player
      } 
    } 
  }
|]

type Player = [schema|
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
    stats:   {
      batting: Maybe BattingStats,
      pitching: Maybe PitchingStats
    },
    seasonStats:   {
      batting: Maybe SeasonBattingStats,
      pitching: Maybe SeasonPitchingStats
    }
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
    stolenBasePercentage: Text,
    atBatsPerHomeRun: Text
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
    stolenBasePercentage: Text,
    strikePercentage: Text,
    homeRunsPer9: Text,
    runsScoredPer9: Text
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

flattenGameData :: Object GameStats -> Int -> Map.Map Int Player
flattenGameData gameData gameId = 
  let
    awayPlayers = [get| gameData.teams.away.players[] |]
    homePlayers = [get| gameData.teams.home.players[] |]
    allPlayers = awayPlayers ++ homePlayers

    transformPlayer :: Player -> Maybe (Int, Player)
    transformPlayer player 
      | null (fromMaybe [] $ playerAllPositions player) = Nothing
      | otherwise = 
        let
            playerId = playerPersonId $ playerPerson player
            positions = map ((read . T.unpack) . allPositionsCode) (playerAllPositions player)
            battingStats = fmap delFieldsBatting (playerStatsBatting $ playerStats player)
            pitchingStats = fmap delFieldsPitching (playerStatsPitching $ playerStats player)
            statsForGame = StatsType battingStats pitchingStats
            newPlayer = [put|
            {
                person: {
                    id: ${playerPersonId $ playerPerson player},
                    fullName: ${playerPersonFullName $ playerPerson player}
                },
                parentTeamId: ${playerParentTeamId player},
                allPositions: ${map (\pos -> [put| { code: ${allPositionsCode pos} } |]) (playerAllPositions player)},
                status: {
                    code: ${statusCode $ playerStatus player}
                },
                stats: {
                    batting: ${fmap delFieldsBatting (playerStatsBatting $ playerStats player)},
                    pitching: ${fmap delFieldsPitching (playerStatsPitching $ playerStats player)}
                },
                seasonStats: {
                    batting: ${playerSeasonStatsBatting $ playerSeasonStats player},
                    pitching: ${playerSeasonStatsPitching $ playerSeasonStats player}
                }
            }
            |]
        in Just (playerId, newPlayer)

    delFieldsBatting :: BattingStats -> BattingStats
    delFieldsBatting stats = stats { note = "", summary = "", stolenBasePercentage = "", atBatsPerHomeRun = "" }

    delFieldsPitching :: PitchingStats -> PitchingStats
    delFieldsPitching stats = stats { note = "", summary = "", stolenBasePercentage = "", strikePercentage = "", homeRunsPer9 = "", runsScoredPer9 = "" }
  in
    Map.fromList $ catMaybes $ mapMaybe transformPlayer allPlayers