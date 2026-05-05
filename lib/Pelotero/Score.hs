{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Scoring orchestration: turn league config + stored boxscore rows into
-- per-team point totals.
--
-- Pure scoring math lives in "Pelotero.Domain.Scoring"; this module wires
-- it to the database. The split keeps the math testable with hedgehog
-- without spinning up Postgres.
--
-- The current scoring assumption: a team's score is the sum of points
-- earned by every player in that team's current 'lineup_slot' rows, over
-- every game in the league's scoring period. That treats the lineup as
-- constant across the period — real fantasy leagues lock lineups daily.
-- Lineup-per-day is a future enhancement that needs either a
-- @lineup_slot_history@ table or per-game lineup snapshots.
module Pelotero.Score
  ( -- * Result types
    PlayerScore(..)
  , TeamScore(..)
  , LeagueScore(..)

    -- * Effectful entry points
  , scoreLeague
  , scoreTeam

    -- * Pure helpers (testable in isolation)
  , scorePlayerPure
  , sumBattingPoints
  , sumPitchingPoints
  , rowToBattingStats
  , rowToPitchingStats
  ) where

import qualified Data.Map.Strict        as Map
import           Data.Maybe             (mapMaybe)
import           Data.Time.Calendar     (Day)
import           Data.Time.Clock        (utctDay)

import Effectful (Eff, (:>))

import           Pelotero.DB.BoxscoreEntry (BattingRow(..), PitchingRow(..))
import           Pelotero.DB.Game          (GameRow(..))
import           Pelotero.DB.LeagueConfig  (LeagueConfigRow(..))
import           Pelotero.DB.LeagueTeam    (LeagueTeamRow(..))
import           Pelotero.DB.LineupSlot    (LineupSlotRow(..))
import           Pelotero.Domain.Id
                   ( DbGameId
                   , DbLeagueConfigId
                   , DbLeagueTeamId
                   , DbPlayerId
                   )
import           Pelotero.Domain.Scoring
                   ( BattingMultipliers
                   , LeagueScoring(..)
                   , PitchingMultipliers
                   , Points
                   , addPoints
                   , scoreBatting
                   , scorePitching
                   , sumPoints
                   )
import           Pelotero.Domain.Stats
                   ( BattingStats(..)
                   , PitchingStats(..)
                   , renderInningsPitched
                   )

import           Pelotero.Effects.BoxscoreEntry (BoxscoreEntry)
import qualified Pelotero.Effects.BoxscoreEntry as Box
import           Pelotero.Effects.Games         (Games)
import qualified Pelotero.Effects.Games         as G
import           Pelotero.Effects.LeagueConfig  (LeagueConfig)
import qualified Pelotero.Effects.LeagueConfig  as LC
import           Pelotero.Effects.LeagueTeam    (LeagueTeam)
import qualified Pelotero.Effects.LeagueTeam    as LT
import           Pelotero.Effects.LineupSlot    (LineupSlot)
import qualified Pelotero.Effects.LineupSlot    as LS

--------------------------------------------------------------------------------
-- Result types

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

--------------------------------------------------------------------------------
-- Pure: row → stats → points

-- | Convert a stored 'BattingRow' to the domain 'BattingStats' shape that
-- 'scoreBatting' consumes. The conversion is the trivial 'Int32 -> Int'
-- widening on every Maybe field; lossless.
rowToBattingStats :: BattingRow -> BattingStats
rowToBattingStats r = BattingStats
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
  where
    i = fmap fromIntegral

-- | Convert a stored 'PitchingRow' to 'PitchingStats'. The DB stores innings
-- as exact outs; we synthesize the wire-format \"X.Y\" string via
-- 'renderInningsPitched' so 'scorePitching' can consume the same shape it
-- does for live data. The round-trip is exact:
-- @parseInningsPitched . renderInningsPitched == id@ for valid outs.
rowToPitchingStats :: PitchingRow -> PitchingStats
rowToPitchingStats r = PitchingStats
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
  , pitInningsPitched          =
      renderInningsPitched . fromIntegral <$> pitchingInningsPitchedOuts r
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
  where
    i = fmap fromIntegral

sumBattingPoints :: BattingMultipliers -> [BattingRow] -> Points
sumBattingPoints m = sumPoints . map (scoreBatting m . rowToBattingStats)

sumPitchingPoints :: PitchingMultipliers -> [PitchingRow] -> Points
sumPitchingPoints m = sumPoints . map (scorePitching m . rowToPitchingStats)

-- | Score one player given their batting and pitching rows over the period.
-- The unit-testable kernel: hedgehog tests can synthesize rows directly
-- without touching effects.
scorePlayerPure
  :: LeagueScoring
  -> [BattingRow]
  -> [PitchingRow]
  -> DbPlayerId
  -> PlayerScore
scorePlayerPure cfg bRows pRows pid =
  let bp = sumBattingPoints  (lsBatting cfg)  bRows
      pp = sumPitchingPoints (lsPitching cfg) pRows
  in PlayerScore
       { psPlayer         = pid
       , psBattingPoints  = bp
       , psPitchingPoints = pp
       , psTotalPoints    = addPoints bp pp
       }

--------------------------------------------------------------------------------
-- Effectful: orchestration

-- | Score every team in a league for the league's configured scoring
-- period. Returns 'Nothing' if the league config doesn't exist; otherwise
-- one 'TeamScore' per team. Empty lineups produce zero-point teams rather
-- than being dropped.
--
-- Implementation reads every batting and pitching row for games in the
-- period (one transaction per game per stat type), then buckets by player.
-- For one league across a one-week period this is ~2N reads where N is
-- games in the period (typically ~100). If this becomes the hot path,
-- replace it with a single join query against game.game_date.
scoreLeague
  :: ( LeagueConfig  :> es
     , LeagueTeam    :> es
     , LineupSlot    :> es
     , Games         :> es
     , BoxscoreEntry :> es
     )
  => DbLeagueConfigId
  -> Eff es (Maybe LeagueScore)
scoreLeague leagueId = do
  mCfg <- LC.getById leagueId
  case mCfg of
    Nothing  -> pure Nothing
    Just cfg -> do
      let startDay = utctDay (lcScoringStart cfg)
          endDay   = utctDay (lcScoringEnd   cfg)

      (bMap, pMap) <- buildPlayerMaps startDay endDay

      teamRows <- LT.getForLeague leagueId
      let teamIds = mapMaybe ltId teamRows
      teamScores <- traverse (scoreOneTeam (lcScoring cfg) bMap pMap) teamIds

      pure $ Just LeagueScore
        { lscLeague      = leagueId
        , lscPeriodStart = startDay
        , lscPeriodEnd   = endDay
        , lscTeams       = teamScores
        }

-- | Score a single team using the league's configured period. Useful for
-- previewing one team's score without touching the rest of the league.
-- Returns 'Nothing' if either the league config or the team isn't in the
-- DB; doesn't validate that the team belongs to the league (caller's
-- responsibility).
scoreTeam
  :: ( LeagueConfig  :> es
     , LeagueTeam    :> es
     , LineupSlot    :> es
     , Games         :> es
     , BoxscoreEntry :> es
     )
  => DbLeagueConfigId
  -> DbLeagueTeamId
  -> Eff es (Maybe TeamScore)
scoreTeam leagueId teamId = do
  mCfg  <- LC.getById leagueId
  mTeam <- LT.getById teamId
  case (mCfg, mTeam) of
    (Just cfg, Just _) -> do
      let startDay = utctDay (lcScoringStart cfg)
          endDay   = utctDay (lcScoringEnd   cfg)
      (bMap, pMap) <- buildPlayerMaps startDay endDay
      Just <$> scoreOneTeam (lcScoring cfg) bMap pMap teamId
    _ -> pure Nothing

--------------------------------------------------------------------------------
-- Internal

buildPlayerMaps
  :: ( Games         :> es
     , BoxscoreEntry :> es
     )
  => Day
  -> Day
  -> Eff es ( Map.Map DbPlayerId [BattingRow]
            , Map.Map DbPlayerId [PitchingRow]
            )
buildPlayerMaps startDay endDay = do
  games <- G.getGamesByDateRange startDay endDay
  let gameIds = mapMaybe gameRowId games
  bMap <- buildBattingMap  gameIds
  pMap <- buildPitchingMap gameIds
  pure (bMap, pMap)

buildBattingMap
  :: BoxscoreEntry :> es
  => [DbGameId] -> Eff es (Map.Map DbPlayerId [BattingRow])
buildBattingMap gids = do
  rowsPerGame <- traverse Box.getBattingForGame gids
  pure $ foldr indexByPlayer Map.empty (concat rowsPerGame)
  where
    indexByPlayer row =
      Map.insertWith (++) (battingPlayerId row) [row]

buildPitchingMap
  :: BoxscoreEntry :> es
  => [DbGameId] -> Eff es (Map.Map DbPlayerId [PitchingRow])
buildPitchingMap gids = do
  rowsPerGame <- traverse Box.getPitchingForGame gids
  pure $ foldr indexByPlayer Map.empty (concat rowsPerGame)
  where
    indexByPlayer row =
      Map.insertWith (++) (pitchingPlayerId row) [row]

scoreOneTeam
  :: LineupSlot :> es
  => LeagueScoring
  -> Map.Map DbPlayerId [BattingRow]
  -> Map.Map DbPlayerId [PitchingRow]
  -> DbLeagueTeamId
  -> Eff es TeamScore
scoreOneTeam scoring bMap pMap tid = do
  slots <- LS.getSlotsForTeam tid
  let pids        = map lsPlayerId slots
      playerScores = map (lookupAndScore scoring bMap pMap) pids
      total        = sumPoints (map psTotalPoints playerScores)
  pure TeamScore
    { tsTeam        = tid
    , tsPlayers     = playerScores
    , tsTotalPoints = total
    }

lookupAndScore
  :: LeagueScoring
  -> Map.Map DbPlayerId [BattingRow]
  -> Map.Map DbPlayerId [PitchingRow]
  -> DbPlayerId
  -> PlayerScore
lookupAndScore scoring bMap pMap pid =
  let bRows = Map.findWithDefault [] pid bMap
      pRows = Map.findWithDefault [] pid pMap
  in scorePlayerPure scoring bRows pRows pid