{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Pelotero.Score
  ( PlayerScore (..)
  , TeamScore (..)
  , LeagueScore (..)
    -- * Pure scoring kernel
  , rowToBattingStats
  , rowToPitchingStats
  , sumBattingPoints
  , sumPitchingPoints
  , scorePlayerPure
    -- * Effectful entry points
  , scoreLeague
  , scoreTeam
    -- * Internals exposed for testing
  , buildPlayerMaps
  ) where

import           Data.Int                       (Int32)
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Data.Time.Calendar             (Day)
import           Data.Time.Clock                (utctDay)
import           Effectful

import           Pelotero.DB.BoxscoreEntry      (BattingRow (..), PitchingRow (..))
import           Pelotero.DB.Game               (GameRow (..))
import           Pelotero.DB.LeagueConfig       (LeagueConfigRow (..))
import           Pelotero.DB.LeagueTeam         (LeagueTeamRow (..))
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
import qualified Pelotero.Effects.Games         as G
import           Pelotero.Effects.Games         (Games)
import qualified Pelotero.Effects.LeagueConfig  as LC
import           Pelotero.Effects.LeagueConfig  (LeagueConfig)
import qualified Pelotero.Effects.LeagueTeam    as LT
import           Pelotero.Effects.LeagueTeam    (LeagueTeam)
import qualified Pelotero.Effects.LineupSnapshot as LSnap
import           Pelotero.Effects.LineupSnapshot (LineupSnapshot)

-- ============================================================================
-- Result types
-- ============================================================================

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

-- ============================================================================
-- Pure kernel (unchanged from pre-B.3)
-- ============================================================================

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

-- ============================================================================
-- Effectful: snapshot-driven scoring
-- ============================================================================

scoreLeague
  :: ( LeagueConfig   :> es
     , LeagueTeam     :> es
     , LineupSnapshot :> es
     , Games          :> es
     , BoxscoreEntry  :> es
     )
  => DbLeagueConfigId
  -> Eff es (Maybe LeagueScore)
scoreLeague lcid = do
  mConfig <- LC.getById lcid
  case mConfig of
    Nothing     -> pure Nothing
    Just config -> do
      let startDay = utctDay (lcScoringStart config)
          endDay   = utctDay (lcScoringEnd   config)
          scoring  = lcScoring config
      gameIds  <- collectGameIds startDay endDay
      teams    <- LT.getForLeague lcid
      (bm, pm) <- buildPlayerMaps gameIds
      teamScores <- traverse (scoreOneTeam scoring bm pm gameIds) teams
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
     , Games          :> es
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
      let startDay = utctDay (lcScoringStart config)
          endDay   = utctDay (lcScoringEnd   config)
          scoring  = lcScoring config
      gameIds  <- collectGameIds startDay endDay
      (bm, pm) <- buildPlayerMaps gameIds
      Just <$> scoreOneTeam scoring bm pm gameIds team
    _ -> pure Nothing

collectGameIds
  :: Games :> es
  => Day -> Day -> Eff es [DbGameId]
collectGameIds startDay endDay = do
  games <- G.getGamesByDateRange startDay endDay
  pure (mapMaybe gameRowId games)

-- | Bucket batting and pitching rows for the given games into
-- per-player lists. One round-trip per game; aggregating into a
-- single date-range query is a separate optimization (C.2).
buildPlayerMaps
  :: BoxscoreEntry :> es
  => [DbGameId]
  -> Eff es ( Map.Map DbPlayerId [BattingRow]
            , Map.Map DbPlayerId [PitchingRow]
            )
buildPlayerMaps gameIds = do
  perGame <- traverse fetchGame gameIds
  let bm = Map.fromListWith (++)
             [(battingPlayerId  r, [r]) | (bs, _) <- perGame, r <- bs]
      pm = Map.fromListWith (++)
             [(pitchingPlayerId r, [r]) | (_, ps) <- perGame, r <- ps]
  pure (bm, pm)
  where
    fetchGame gid = do
      bs <- Box.getBattingForGame  gid
      ps <- Box.getPitchingForGame gid
      pure (bs, ps)

scoreOneTeam
  :: LineupSnapshot :> es
  => LeagueScoring
  -> Map.Map DbPlayerId [BattingRow]
  -> Map.Map DbPlayerId [PitchingRow]
  -> [DbGameId]
  -> LeagueTeamRow
  -> Eff es TeamScore
scoreOneTeam scoring bm pm gameIds team = do
  let ltid = fromMaybe (error "scoreOneTeam: team without surrogate id") (ltId team)
  perGame <- traverse (scoreTeamForGame scoring bm pm ltid) gameIds
  let allScores      = concat perGame
      grouped        = Map.fromListWith mergePlayerScores
                         [(psPlayer p, p) | p <- allScores]
      teamPlayerList = Map.elems grouped
      total          = sumPoints (map psTotalPoints teamPlayerList)
  pure TeamScore
    { tsTeam        = ltid
    , tsPlayers     = teamPlayerList
    , tsTotalPoints = total
    }

scoreTeamForGame
  :: LineupSnapshot :> es
  => LeagueScoring
  -> Map.Map DbPlayerId [BattingRow]
  -> Map.Map DbPlayerId [PitchingRow]
  -> DbLeagueTeamId
  -> DbGameId
  -> Eff es [PlayerScore]
scoreTeamForGame scoring bm pm ltid gid = do
  snapshot <- LSnap.getSnapshotForTeamGame ltid gid
  pure (map (scoreOnePlayerForGame scoring bm pm gid . lsnapPlayerId) snapshot)

scoreOnePlayerForGame
  :: LeagueScoring
  -> Map.Map DbPlayerId [BattingRow]
  -> Map.Map DbPlayerId [PitchingRow]
  -> DbGameId
  -> DbPlayerId
  -> PlayerScore
scoreOnePlayerForGame scoring bm pm gid pid =
  let bs = filter ((== gid) . battingGameId)  (Map.findWithDefault [] pid bm)
      ps = filter ((== gid) . pitchingGameId) (Map.findWithDefault [] pid pm)
  in scorePlayerPure scoring bs ps pid

mergePlayerScores :: PlayerScore -> PlayerScore -> PlayerScore
mergePlayerScores a b = PlayerScore
  { psPlayer         = psPlayer a
  , psBattingPoints  = addPoints (psBattingPoints a)  (psBattingPoints b)
  , psPitchingPoints = addPoints (psPitchingPoints a) (psPitchingPoints b)
  , psTotalPoints    = addPoints (psTotalPoints a)    (psTotalPoints b)
  }