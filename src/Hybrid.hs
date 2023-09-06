{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Aeson (Value, eitherDecodeFileStrict)
import Data.Aeson.Schema 
import qualified Data.Text as T
import Data.HashMap.Strict (fromList, union)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)


-- -- Extract the 'players' fields as generic 'Value' types:
-- firstTraversal :: ByteString -> ByteString
-- firstTraversal = [get| boxscore.teams.away.players |]
-- firstTraversal = [get| boxscore.teams.home.players |]


type Boxscore =
  [schema|
  {
    teams: {
      away: {
        players: JSONValue
      },
      home: {
        players: JSONValue
      } 
    }
  }
|]

type JSONValue = Data.Aeson.Value

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


main :: IO ()
main = do
  --open json file and parse into Boxscore type
  obj <-
    either fail return
      =<< eitherDecodeFileStrict "testFiles/716896_boxscore.json" :: IO (Data.Aeson.Schema.Object Boxscore)

  print obj
