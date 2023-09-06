{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module InputSchemas
  ( Boxscore,
    Player,
  )
where

import Data.Aeson (eitherDecodeFileStrict, toJSON)
import Data.Aeson.Schema
import Data.HashMap.Strict (fromList, union)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T

type Boxscore =
  [schema|
  {
    teams: {
      away: {
        players: Player
      },
      home: {
        players: Player
      } 
    }
  }
|]

type Player =
  [schema|
  {
    person: List {
        id: Int,
        fullName: Text,
      },
    parentTeamId: Int,
    allPositions: Maybe List {
      code: Text,
    },
    status: List {
      code: Text,
    },
    stats: {
      batting: Maybe List {
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
      },
      pitching: Maybe List {
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
    }
  }
|]

-- flattenGameData :: Object GameStats -> Int -> Map.Map Int Player
-- flattenGameData gameData gameId =
--   let
--     awayPlayers = [get| gameData.teams.away.players[] |]
--     homePlayers = [get| gameData.teams.home.players[] |]
--     allPlayers = awayPlayers ++ homePlayers

--     transformPlayer :: Player -> Maybe (Int, Player)
--     transformPlayer player
--       | null (fromMaybe [] $ playerAllPositions player) = Nothing
--       | otherwise =
--         let
--             playerId = playerPersonId $ playerPerson player
--             positions = map ((read . T.unpack) . allPositionsCode) (playerAllPositions player)
--             battingStats = fmap delFieldsBatting (playerStatsBatting $ playerStats player)
--             pitchingStats = fmap delFieldsPitching (playerStatsPitching $ playerStats player)
--             statsForGame = StatsType battingStats pitchingStats
--             newPlayer = [put|
--             {
--                 person: {
--                     id: ${playerPersonId $ playerPerson player},
--                     fullName: ${playerPersonFullName $ playerPerson player}
--                 },
--                 parentTeamId: ${playerParentTeamId player},
--                 allPositions: ${map (\pos -> [put| { code: ${allPositionsCode pos} } |]) (playerAllPositions player)},
--                 status: {
--                     code: ${statusCode $ playerStatus player}
--                 },
--                 stats: {
--                     batting: ${fmap delFieldsBatting (playerStatsBatting $ playerStats player)},
--                     pitching: ${fmap delFieldsPitching (playerStatsPitching $ playerStats player)}
--                 },
--                 seasonStats: {
--                     batting: ${playerSeasonStatsBatting $ playerSeasonStats player},
--                     pitching: ${playerSeasonStatsPitching $ playerSeasonStats player}
--                 }
--             }
--             |]
--         in Just (playerId, newPlayer)

--     delFieldsBatting :: BattingStats -> BattingStats
--     delFieldsBatting stats = stats { note = "", summary = "", stolenBasePercentage = "", atBatsPerHomeRun = "" }

--     delFieldsPitching :: PitchingStats -> PitchingStats
--     delFieldsPitching stats = stats { note = "", summary = "", stolenBasePercentage = "", strikePercentage = "", homeRunsPer9 = "", runsScoredPer9 = "" }
--   in
--     Map.fromList $ catMaybes $ mapMaybe transformPlayer allPlayers