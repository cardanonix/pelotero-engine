{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module OutputSchemas
    ( --flattenGameData
    ) where
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema
import Data.HashMap.Strict (union, fromList)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)

-- Schema Describing Transformed Daily Stats
type DailyStats = [schema|
  {
    playerData: List {
      id: Int,
      fullName: Text,
      stats: List   {
        parentTeamId: Int,
        allPositions: List Int,
        status: Text,
        batting:   {
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
        pitching:   {
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
        },
      },
    },
    games: List   {
      id: Text,
    }, 
    checksum: Text,
    date: Text,
  }
|]

-- Schema Describing Transformed Roster List
type RosterList = [schema|
  {
    playerInfo: List   {
      id: Int,
      useName: Text,
      useLastName: Text,
      nameSlug: Text,
      currentTeam: Int,
      primaryPosition: Text,
      batSide: Text,
      pitchHand: Text,
      active: Bool,
    },
    dataPulled: Text,
    checksum: Text,
  }
|]