{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Pelotero.Score
  ( PlayerScore (..)
  , TeamScore (..)
  , LeagueScore (..)

  , rowToBattingStats
  , rowToPitchingStats
  , sumBattingPoints
  , sumPitchingPoints
  , scorePlayerPure

  , scoreLeague
  , scoreTeam

  , buildPlayerMaps
  ) where

import           Data.Int                       (Int32)
import qualified Data.Map.Strict                as Map
import           Data.Time.Calendar             (Day)
import           Data.Time.Clock                (utctDay)
import           Effectful

import           Pelotero.DB.BoxscoreEntry      (BattingRow (..), PitchingRow (..))
import           Pelotero.DB.LeagueConfig       (LoadedLeagueConfig (..))
import           Pelotero.DB.LeagueTeam         (LoadedLeagueTeam (..))
import           Pelotero.DB.LineupSnapshot     (LineupSnapshotRow (..))
import           Pelotero.Domain.Id
import           Pelotero.Domain.Scoring
                     ( BattingMultipliers (..)
                     , LeagueScoring (..)
                     , PitchingMultipliers (..)
                     , Points (..)
                     , addPoints
                     , scoreBatting
                     , scorePitching
                     , sumPoints
                     )
import           Pelotero.Domain.Stats
                     (BattingStats (..), PitchingStats (..), emptyBatting, emptyPitching)
import qualified Pelotero.Effects.BoxscoreEntry as Box
import           Pelotero.Effects.BoxscoreEntry (BoxscoreEntry)
import qualified Pelotero.Effects.LeagueConfig  as LC
import           Pelotero.Effects.LeagueConfig  (LeagueConfig)
import qualified Pelotero.Effects.LeagueTeam    as LT
import           Pelotero.Effects.LeagueTeam    (LeagueTeam)
import qualified Pelotero.Effects.LineupSnapshot as LSnap
import           Pelotero.Effects.LineupSnapshot (LineupSnapshot)

data PlayerScore = PlayerScore
  { psPlayer         :: !DbPlayerId
  , psBattingPoints  :: !Points
  , psPitchingPoints :: !Points
  , psTotalPoints    :: !Points
  }
  deriving stock (Show, Eq)

data TeamScore = TeamScore
  { tsTeam        :: !DbLeagueTeamId
  , tsPlayers     :: ![PlayerScore]
  , tsTotalPoints :: !Points
  }
  deriving stock (Show, Eq)

data LeagueScore = LeagueScore
  { lscLeague      :: !DbLeagueConfigId
  , lscPeriodStart :: !Day
  , lscPeriodEnd   :: !Day
  , lscTeams       :: ![TeamScore]
  }
  deriving stock (Show, Eq)

rowToBattingStats :: BattingRow -> BattingStats
rowToBattingStats r = emptyBatting
  { batGamesPlayed          = i (battingGamesPlayed r)
  , batPlateAppearances     = i (battingPlateAppearances r)
  , batAtBats               = i (battingAtBats r)
  , batRuns                 = i (battingRuns r)
  , batHits                 = i (battingHits r)
  , batDoubles              = i (battingDoubles r)
  , batTriples              = i (battingTriples r)
  , batHomeRuns             = i (battingHomeRuns r)
  , batRbi                  = i (battingRbi r)
  , batBaseOnBalls          = i (battingBaseOnBalls r)
  , batIntentionalWalks     = i (battingIntentionalWalks r)
  , batStrikeOuts           = i (battingStrikeOuts r)
  , batStolenBases          = i (battingStolenBases r)
  , batCaughtStealing       = i (battingCaughtStealing r)
  , batHitByPitch           = i (battingHitByPitch r)
  , batSacBunts             = i (battingSacBunts r)
  , batSacFlies             = i (battingSacFlies r)
  , batGroundIntoDoublePlay = i (battingGroundIntoDoublePlay r)
  , batGroundIntoTriplePlay = i (battingGroundIntoTriplePlay r)
  , batLeftOnBase           = i (battingLeftOnBase r)
  , batTotalBases           = i (battingTotalBases r)
  , batFlyOuts              = i (battingFlyOuts r)
  , batGroundOuts           = i (battingGroundOuts r)
  , batCatchersInterference = i (battingCatchersInterference r)
  , batPickoffs             = i (battingPickoffs r)
  }
  where i = fmap fromIntegral :: Maybe Int32 -> Maybe Int

rowToPitchingStats :: PitchingRow -> PitchingStats
rowToPitchingStats r = emptyPitching
  { pitGamesPlayed             = i (pitchingGamesPlayed r)
  , pitGamesStarted            = i (pitchingGamesStarted r)
  , pitGamesFinished           = i (pitchingGamesFinished r)
  , pitCompleteGames           = i (pitchingCompleteGames r)
  , pitShutouts                = i (pitchingShutouts r)
  , pitWins                    = i (pitchingWins r)
  , pitLosses                  = i (pitchingLosses r)
  , pitSaves                   = i (pitchingSaves r)
  , pitSaveOpportunities       = i (pitchingSaveOpportunities r)
  , pitHolds                   = i (pitchingHolds r)
  , pitBlownSaves              = i (pitchingBlownSaves r)
  , pitOuts                    = i (pitchingInningsPitchedOuts r)
  , pitBattersFaced            = i (pitchingBattersFaced r)
  , pitNumberOfPitches         = i (pitchingNumberOfPitches r)
  , pitStrikes                 = i (pitchingStrikes r)
  , pitBalls                   = i (pitchingBalls r)
  , pitHits                    = i (pitchingHits r)
  , pitDoubles                 = i (pitchingDoubles r)
  , pitTriples                 = i (pitchingTriples r)
  , pitHomeRuns                = i (pitchingHomeRuns r)
  , pitRuns                    = i (pitchingRuns r)
  , pitEarnedRuns              = i (pitchingEarnedRuns r)
  , pitStrikeOuts              = i (pitchingStrikeOuts r)
  , pitBaseOnBalls             = i (pitchingBaseOnBalls r)
  , pitIntentionalWalks        = i (pitchingIntentionalWalks r)
  , pitHitBatsmen              = i (pitchingHitBatsmen r)
  , pitWildPitches             = i (pitchingWildPitches r)
  , pitBalks                   = i (pitchingBalks r)
  , pitPickoffs                = i (pitchingPickoffs r)
  , pitFlyOuts                 = i (pitchingFlyOuts r)
  , pitGroundOuts              = i (pitchingGroundOuts r)
  , pitAirOuts                 = i (pitchingAirOuts r)
  , pitInheritedRunners        = i (pitchingInheritedRunners r)
  , pitInheritedRunnersScored  = i (pitchingInheritedRunnersScored r)
  , pitStolenBases             = i (pitchingStolenBases r)
  , pitCaughtStealing          = i (pitchingCaughtStealing r)
  , pitAtBats                  = i (pitchingAtBats r)
  , pitRbi                     = i (pitchingRbi r)
  , pitSacBunts                = i (pitchingSacBunts r)
  , pitSacFlies                = i (pitchingSacFlies r)
  , pitCatchersInterference    = i (pitchingCatchersInterference r)
  , pitPassedBall              = i (pitchingPassedBall r)
  }
  where i = fmap fromIntegral :: Maybe Int32 -> Maybe Int

sumBattingPoints :: BattingMultipliers -> [BattingRow] -> Points
sumBattingPoints m = sumPoints . map (scoreBatting m . rowToBattingStats)

sumPitchingPoints :: PitchingMultipliers -> [PitchingRow] -> Points
sumPitchingPoints m = sumPoints . map (scorePitching m . rowToPitchingStats)

scorePlayerPure
  :: LeagueScoring
  -> [BattingRow]
  -> [PitchingRow]
  -> DbPlayerId
  -> PlayerScore
scorePlayerPure scoring bs ps pid =
  let bp = sumBattingPoints  (lsBatting  scoring) bs
      pp = sumPitchingPoints (lsPitching scoring) ps
  in PlayerScore
       { psPlayer         = pid
       , psBattingPoints  = bp
       , psPitchingPoints = pp
       , psTotalPoints    = addPoints bp pp
       }

-- | Score every team in a league for the period defined by the league config.
--
-- Three batched effect calls regardless of period length:
--   * one LeagueConfig fetch
--   * one LeagueTeam fetch
--   * two stats fetches (batting + pitching, both date-range)
--   * one snapshot fetch (date-range, joined to game)
--
-- All cross-product/grouping work is then pure.
scoreLeague
  :: ( LeagueConfig   :> es
     , LeagueTeam     :> es
     , LineupSnapshot :> es
     , BoxscoreEntry  :> es
     )
  => DbLeagueConfigId
  -> Eff es (Maybe LeagueScore)
scoreLeague lcid = do
  mConfig <- LC.getById lcid
  case mConfig of
    Nothing     -> pure Nothing
    Just config -> do
      let startDay = utctDay (llcScoringStart config)
          endDay   = utctDay (llcScoringEnd   config)
          scoring  = llcScoring config
      teams        <- LT.getForLeague lcid
      (bm, pm)     <- buildPlayerMaps startDay endDay
      allSnapshots <- LSnap.getSnapshotsForDateRange startDay endDay
      let snapsByTeam :: Map.Map DbLeagueTeamId [LineupSnapshotRow]
          snapsByTeam = Map.fromListWith (++)
            [(lsnapLeagueTeamId s, [s]) | s <- allSnapshots]
          teamScores =
            [ scoreOneTeam scoring bm pm
                (Map.findWithDefault [] (lltId t) snapsByTeam) t
            | t <- teams
            ]
      pure $ Just LeagueScore
        { lscLeague      = lcid
        , lscPeriodStart = startDay
        , lscPeriodEnd   = endDay
        , lscTeams       = teamScores
        }

scoreTeam
  :: ( LeagueConfig   :> es
     , LeagueTeam     :> es
     , LineupSnapshot :> es
     , BoxscoreEntry  :> es
     )
  => DbLeagueConfigId
  -> DbLeagueTeamId
  -> Eff es (Maybe TeamScore)
scoreTeam lcid ltid = do
  mConfig <- LC.getById lcid
  mTeam   <- LT.getById ltid
  case (mConfig, mTeam) of
    (Just config, Just team) -> do
      let startDay = utctDay (llcScoringStart config)
          endDay   = utctDay (llcScoringEnd   config)
          scoring  = llcScoring config
      (bm, pm)     <- buildPlayerMaps startDay endDay
      allSnapshots <- LSnap.getSnapshotsForDateRange startDay endDay
      let teamSnapshots =
            filter ((== ltid) . lsnapLeagueTeamId) allSnapshots
      pure (Just (scoreOneTeam scoring bm pm teamSnapshots team))
    _ -> pure Nothing

-- | Pre-fetch the period's batting and pitching rows, indexed by player.
buildPlayerMaps
  :: BoxscoreEntry :> es
  => Day -> Day
  -> Eff es ( Map.Map DbPlayerId [BattingRow]
            , Map.Map DbPlayerId [PitchingRow]
            )
buildPlayerMaps startDay endDay = do
  bs <- Box.getBattingForDateRange  startDay endDay
  ps <- Box.getPitchingForDateRange startDay endDay
  let bm = Map.fromListWith (++) [(battingPlayerId  r, [r]) | r <- bs]
      pm = Map.fromListWith (++) [(pitchingPlayerId r, [r]) | r <- ps]
  pure (bm, pm)

-- | Pure: given a team's snapshot rows, compute its TeamScore.
--
-- Grouping by game is intentional: it makes per-game scoring local, which
-- preserves the property that a player snapshotted into game G receives
-- exactly G's stats (and zero from games where they were not snapshotted).
scoreOneTeam
  :: LeagueScoring
  -> Map.Map DbPlayerId [BattingRow]
  -> Map.Map DbPlayerId [PitchingRow]
  -> [LineupSnapshotRow]
  -> LoadedLeagueTeam
  -> TeamScore
scoreOneTeam scoring bm pm snaps team =
  let snapsByGame :: Map.Map DbGameId [DbPlayerId]
      snapsByGame = Map.fromListWith (++)
        [(lsnapGameId s, [lsnapPlayerId s]) | s <- snaps]
      perGame :: [PlayerScore]
      perGame = concat
        [ [ scoreOnePlayerForGame scoring bm pm gid pid | pid <- pids ]
        | (gid, pids) <- Map.toList snapsByGame
        ]
      grouped = Map.fromListWith mergePlayerScores
                   [(psPlayer p, p) | p <- perGame]
      teamPlayerList = Map.elems grouped
      total          = sumPoints (map psTotalPoints teamPlayerList)
  in TeamScore
       { tsTeam        = lltId team
       , tsPlayers     = teamPlayerList
       , tsTotalPoints = total
       }

scoreOnePlayerForGame
  :: LeagueScoring
  -> Map.Map DbPlayerId [BattingRow]
  -> Map.Map DbPlayerId [PitchingRow]
  -> DbGameId
  -> DbPlayerId
  -> PlayerScore
scoreOnePlayerForGame scoring bm pm gid pid =
  let bs = filter ((== gid) . battingGameId)
             (Map.findWithDefault [] pid bm)
      ps = filter ((== gid) . pitchingGameId)
             (Map.findWithDefault [] pid pm)
  in scorePlayerPure scoring bs ps pid

mergePlayerScores :: PlayerScore -> PlayerScore -> PlayerScore
mergePlayerScores a b = PlayerScore
  { psPlayer         = psPlayer a
  , psBattingPoints  = addPoints (psBattingPoints a)  (psBattingPoints b)
  , psPitchingPoints = addPoints (psPitchingPoints a) (psPitchingPoints b)
  , psTotalPoints    = addPoints (psTotalPoints a)    (psTotalPoints b)
  }