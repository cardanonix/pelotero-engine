-- | Repository for the per-game stat tables: @game_player_batting@ and
-- @game_player_pitching@.
--
-- Each player who appears in a game gets at most one row in each table.
-- Two-way players (Ohtani) get a row in each. Stat columns are nullable —
-- NULL means \"this player didn't bat/pitch in this game\". Scoring queries
-- COALESCE to zero where appropriate.
--
-- This module is long because the upstream schema is long. There are
-- ~25 batting stats and ~45 pitching stats; we store them all rather than
-- pre-aggregate, because scoring rules differ between leagues and we
-- want every league to scan the same source data.
--
-- Column-order conventions:
--
--   * SQL column lists, encoder tuple positions, decoder applicative chains,
--     and field order in the row record all match. If you add a column,
--     touch all four.
--   * Stat columns appear in the same order across batting and pitching
--     where they overlap (e.g. @runs@, @hits@, @home_runs@, @rbi@, @at_bats@).
module Pelotero.DB.BoxscoreEntry
  ( -- * Row types
    BattingRow(..)
  , PitchingRow(..)
    -- * Transaction-level API
  , upsertBattingT
  , upsertPitchingT
  , getBattingForGameT
  , getPitchingForGameT
  , deleteBattingForGameT
  , deletePitchingForGameT
    -- * Pool/IO API (wrappers)
  , upsertBatting
  , upsertPitching
  , getBattingForGame
  , getPitchingForGame
  , deleteBattingForGame
  , deletePitchingForGame
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int                   (Int32)
import qualified Data.Vector as V
import qualified Hasql.Decoders   as D
import qualified Hasql.Encoders   as E
import qualified Hasql.Statement  as Stmt
import qualified Hasql.Transaction as Tx

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Statement
import Pelotero.Domain.Id    (DbGameId, DbPlayerId, DbTeamId)

--------------------------------------------------------------------------------
-- Batting row type

data BattingRow = BattingRow
  { battingGameId                 :: !DbGameId
  , battingPlayerId               :: !DbPlayerId
  , battingTeamId                 :: !(Maybe DbTeamId)
  , battingGamesPlayed            :: !(Maybe Int32)
  , battingPlateAppearances       :: !(Maybe Int32)
  , battingAtBats                 :: !(Maybe Int32)
  , battingRuns                   :: !(Maybe Int32)
  , battingHits                   :: !(Maybe Int32)
  , battingDoubles                :: !(Maybe Int32)
  , battingTriples                :: !(Maybe Int32)
  , battingHomeRuns               :: !(Maybe Int32)
  , battingRbi                    :: !(Maybe Int32)
  , battingBaseOnBalls            :: !(Maybe Int32)
  , battingIntentionalWalks       :: !(Maybe Int32)
  , battingStrikeOuts             :: !(Maybe Int32)
  , battingStolenBases            :: !(Maybe Int32)
  , battingCaughtStealing         :: !(Maybe Int32)
  , battingHitByPitch             :: !(Maybe Int32)
  , battingSacBunts               :: !(Maybe Int32)
  , battingSacFlies               :: !(Maybe Int32)
  , battingGroundIntoDoublePlay   :: !(Maybe Int32)
  , battingGroundIntoTriplePlay   :: !(Maybe Int32)
  , battingLeftOnBase             :: !(Maybe Int32)
  , battingTotalBases             :: !(Maybe Int32)
  , battingFlyOuts                :: !(Maybe Int32)
  , battingGroundOuts             :: !(Maybe Int32)
  , battingCatchersInterference   :: !(Maybe Int32)
  , battingPickoffs               :: !(Maybe Int32)
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Pitching row type

data PitchingRow = PitchingRow
  { pitchingGameId                 :: !DbGameId
  , pitchingPlayerId               :: !DbPlayerId
  , pitchingTeamId                 :: !(Maybe DbTeamId)
  , pitchingGamesPlayed            :: !(Maybe Int32)
  , pitchingGamesStarted           :: !(Maybe Int32)
  , pitchingGamesFinished          :: !(Maybe Int32)
  , pitchingCompleteGames          :: !(Maybe Int32)
  , pitchingShutouts               :: !(Maybe Int32)
  , pitchingWins                   :: !(Maybe Int32)
  , pitchingLosses                 :: !(Maybe Int32)
  , pitchingSaves                  :: !(Maybe Int32)
  , pitchingSaveOpportunities      :: !(Maybe Int32)
  , pitchingHolds                  :: !(Maybe Int32)
  , pitchingBlownSaves             :: !(Maybe Int32)
  , pitchingInningsPitchedOuts     :: !(Maybe Int32)
  , pitchingBattersFaced           :: !(Maybe Int32)
  , pitchingNumberOfPitches        :: !(Maybe Int32)
  , pitchingStrikes                :: !(Maybe Int32)
  , pitchingBalls                  :: !(Maybe Int32)
  , pitchingHits                   :: !(Maybe Int32)
  , pitchingDoubles                :: !(Maybe Int32)
  , pitchingTriples                :: !(Maybe Int32)
  , pitchingHomeRuns               :: !(Maybe Int32)
  , pitchingRuns                   :: !(Maybe Int32)
  , pitchingEarnedRuns             :: !(Maybe Int32)
  , pitchingStrikeOuts             :: !(Maybe Int32)
  , pitchingBaseOnBalls            :: !(Maybe Int32)
  , pitchingIntentionalWalks       :: !(Maybe Int32)
  , pitchingHitBatsmen             :: !(Maybe Int32)
  , pitchingWildPitches            :: !(Maybe Int32)
  , pitchingBalks                  :: !(Maybe Int32)
  , pitchingPickoffs               :: !(Maybe Int32)
  , pitchingFlyOuts                :: !(Maybe Int32)
  , pitchingGroundOuts             :: !(Maybe Int32)
  , pitchingAirOuts                :: !(Maybe Int32)
  , pitchingInheritedRunners       :: !(Maybe Int32)
  , pitchingInheritedRunnersScored :: !(Maybe Int32)
  , pitchingStolenBases            :: !(Maybe Int32)
  , pitchingCaughtStealing         :: !(Maybe Int32)
  , pitchingAtBats                 :: !(Maybe Int32)
  , pitchingRbi                    :: !(Maybe Int32)
  , pitchingSacBunts               :: !(Maybe Int32)
  , pitchingSacFlies               :: !(Maybe Int32)
  , pitchingCatchersInterference   :: !(Maybe Int32)
  , pitchingPassedBall             :: !(Maybe Int32)
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Transaction-level API

upsertBattingT :: BattingRow -> Tx.Transaction ()
upsertBattingT row = Tx.statement row upsertBattingStmt

upsertPitchingT :: PitchingRow -> Tx.Transaction ()
upsertPitchingT row = Tx.statement row upsertPitchingStmt

getBattingForGameT :: DbGameId -> Tx.Transaction [BattingRow]
getBattingForGameT gid = V.toList <$> Tx.statement gid selectBattingForGameStmt

getPitchingForGameT :: DbGameId -> Tx.Transaction [PitchingRow]
getPitchingForGameT gid = V.toList <$> Tx.statement gid selectPitchingForGameStmt

deleteBattingForGameT :: DbGameId -> Tx.Transaction ()
deleteBattingForGameT gid = Tx.statement gid deleteBattingStmt

deletePitchingForGameT :: DbGameId -> Tx.Transaction ()
deletePitchingForGameT gid = Tx.statement gid deletePitchingStmt

--------------------------------------------------------------------------------
-- Pool/IO API (wrappers)

upsertBatting :: Pool -> BattingRow -> IO (Either DBError ())
upsertBatting pool row = runTransaction pool (upsertBattingT row)

upsertPitching :: Pool -> PitchingRow -> IO (Either DBError ())
upsertPitching pool row = runTransaction pool (upsertPitchingT row)

getBattingForGame :: Pool -> DbGameId -> IO (Either DBError [BattingRow])
getBattingForGame pool gid = runTransaction pool (getBattingForGameT gid)

getPitchingForGame :: Pool -> DbGameId -> IO (Either DBError [PitchingRow])
getPitchingForGame pool gid = runTransaction pool (getPitchingForGameT gid)

deleteBattingForGame :: Pool -> DbGameId -> IO (Either DBError ())
deleteBattingForGame pool gid = runTransaction pool (deleteBattingForGameT gid)

deletePitchingForGame :: Pool -> DbGameId -> IO (Either DBError ())
deletePitchingForGame pool gid = runTransaction pool (deletePitchingForGameT gid)

--------------------------------------------------------------------------------
-- Encoders
--
-- Each row type has its own encoder. We compose by record-field selector
-- rather than by tuple positional projection because at 28 / 45 columns
-- the tuple-position approach becomes a maintenance hazard. Record
-- selectors give the compiler a chance to catch a mismatch.

battingEncoder :: E.Params BattingRow
battingEncoder =
     (battingGameId               >$< encDbGameId)
  <> (battingPlayerId             >$< encDbPlayerId)
  <> (battingTeamId               >$< encDbTeamIdMaybe)
  <> (battingGamesPlayed          >$< encInt32Maybe)
  <> (battingPlateAppearances     >$< encInt32Maybe)
  <> (battingAtBats               >$< encInt32Maybe)
  <> (battingRuns                 >$< encInt32Maybe)
  <> (battingHits                 >$< encInt32Maybe)
  <> (battingDoubles              >$< encInt32Maybe)
  <> (battingTriples              >$< encInt32Maybe)
  <> (battingHomeRuns             >$< encInt32Maybe)
  <> (battingRbi                  >$< encInt32Maybe)
  <> (battingBaseOnBalls          >$< encInt32Maybe)
  <> (battingIntentionalWalks     >$< encInt32Maybe)
  <> (battingStrikeOuts           >$< encInt32Maybe)
  <> (battingStolenBases          >$< encInt32Maybe)
  <> (battingCaughtStealing       >$< encInt32Maybe)
  <> (battingHitByPitch           >$< encInt32Maybe)
  <> (battingSacBunts             >$< encInt32Maybe)
  <> (battingSacFlies             >$< encInt32Maybe)
  <> (battingGroundIntoDoublePlay >$< encInt32Maybe)
  <> (battingGroundIntoTriplePlay >$< encInt32Maybe)
  <> (battingLeftOnBase           >$< encInt32Maybe)
  <> (battingTotalBases           >$< encInt32Maybe)
  <> (battingFlyOuts              >$< encInt32Maybe)
  <> (battingGroundOuts           >$< encInt32Maybe)
  <> (battingCatchersInterference >$< encInt32Maybe)
  <> (battingPickoffs             >$< encInt32Maybe)

pitchingEncoder :: E.Params PitchingRow
pitchingEncoder =
     (pitchingGameId                 >$< encDbGameId)
  <> (pitchingPlayerId               >$< encDbPlayerId)
  <> (pitchingTeamId                 >$< encDbTeamIdMaybe)
  <> (pitchingGamesPlayed            >$< encInt32Maybe)
  <> (pitchingGamesStarted           >$< encInt32Maybe)
  <> (pitchingGamesFinished          >$< encInt32Maybe)
  <> (pitchingCompleteGames          >$< encInt32Maybe)
  <> (pitchingShutouts               >$< encInt32Maybe)
  <> (pitchingWins                   >$< encInt32Maybe)
  <> (pitchingLosses                 >$< encInt32Maybe)
  <> (pitchingSaves                  >$< encInt32Maybe)
  <> (pitchingSaveOpportunities      >$< encInt32Maybe)
  <> (pitchingHolds                  >$< encInt32Maybe)
  <> (pitchingBlownSaves             >$< encInt32Maybe)
  <> (pitchingInningsPitchedOuts     >$< encInt32Maybe)
  <> (pitchingBattersFaced           >$< encInt32Maybe)
  <> (pitchingNumberOfPitches        >$< encInt32Maybe)
  <> (pitchingStrikes                >$< encInt32Maybe)
  <> (pitchingBalls                  >$< encInt32Maybe)
  <> (pitchingHits                   >$< encInt32Maybe)
  <> (pitchingDoubles                >$< encInt32Maybe)
  <> (pitchingTriples                >$< encInt32Maybe)
  <> (pitchingHomeRuns               >$< encInt32Maybe)
  <> (pitchingRuns                   >$< encInt32Maybe)
  <> (pitchingEarnedRuns             >$< encInt32Maybe)
  <> (pitchingStrikeOuts             >$< encInt32Maybe)
  <> (pitchingBaseOnBalls            >$< encInt32Maybe)
  <> (pitchingIntentionalWalks       >$< encInt32Maybe)
  <> (pitchingHitBatsmen             >$< encInt32Maybe)
  <> (pitchingWildPitches            >$< encInt32Maybe)
  <> (pitchingBalks                  >$< encInt32Maybe)
  <> (pitchingPickoffs               >$< encInt32Maybe)
  <> (pitchingFlyOuts                >$< encInt32Maybe)
  <> (pitchingGroundOuts             >$< encInt32Maybe)
  <> (pitchingAirOuts                >$< encInt32Maybe)
  <> (pitchingInheritedRunners       >$< encInt32Maybe)
  <> (pitchingInheritedRunnersScored >$< encInt32Maybe)
  <> (pitchingStolenBases            >$< encInt32Maybe)
  <> (pitchingCaughtStealing         >$< encInt32Maybe)
  <> (pitchingAtBats                 >$< encInt32Maybe)
  <> (pitchingRbi                    >$< encInt32Maybe)
  <> (pitchingSacBunts               >$< encInt32Maybe)
  <> (pitchingSacFlies               >$< encInt32Maybe)
  <> (pitchingCatchersInterference   >$< encInt32Maybe)
  <> (pitchingPassedBall             >$< encInt32Maybe)

--------------------------------------------------------------------------------
-- Decoders

battingRowDecoder :: D.Row BattingRow
battingRowDecoder = BattingRow
  <$> decDbGameId
  <*> decDbPlayerId
  <*> decDbTeamIdMaybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe
  <*> decInt32Maybe

pitchingRowDecoder :: D.Row PitchingRow
pitchingRowDecoder = PitchingRow
  <$> decDbGameId
  <*> decDbPlayerId
  <*> decDbTeamIdMaybe
  <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe
  <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe
  <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe
  <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe
  <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe
  <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe
  <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe
  <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe
  <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe
  <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe <*> decInt32Maybe
  <*> decInt32Maybe <*> decInt32Maybe

--------------------------------------------------------------------------------
-- Statements

upsertBattingStmt :: Stmt.Statement BattingRow ()
upsertBattingStmt = Stmt.Statement sql battingEncoder D.noResult True
  where
    sql = "INSERT INTO game_player_batting \
          \  (game_id, player_id, team_id, \
          \   games_played, plate_appearances, at_bats, runs, hits, \
          \   doubles, triples, home_runs, rbi, base_on_balls, \
          \   intentional_walks, strike_outs, stolen_bases, caught_stealing, \
          \   hit_by_pitch, sac_bunts, sac_flies, ground_into_double_play, \
          \   ground_into_triple_play, left_on_base, total_bases, fly_outs, \
          \   ground_outs, catchers_interference, pickoffs) \
          \VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, \
          \        $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, \
          \        $25, $26, $27, $28) \
          \ON CONFLICT (game_id, player_id) DO UPDATE SET \
          \   team_id                 = EXCLUDED.team_id, \
          \   games_played            = EXCLUDED.games_played, \
          \   plate_appearances       = EXCLUDED.plate_appearances, \
          \   at_bats                 = EXCLUDED.at_bats, \
          \   runs                    = EXCLUDED.runs, \
          \   hits                    = EXCLUDED.hits, \
          \   doubles                 = EXCLUDED.doubles, \
          \   triples                 = EXCLUDED.triples, \
          \   home_runs               = EXCLUDED.home_runs, \
          \   rbi                     = EXCLUDED.rbi, \
          \   base_on_balls           = EXCLUDED.base_on_balls, \
          \   intentional_walks       = EXCLUDED.intentional_walks, \
          \   strike_outs             = EXCLUDED.strike_outs, \
          \   stolen_bases            = EXCLUDED.stolen_bases, \
          \   caught_stealing         = EXCLUDED.caught_stealing, \
          \   hit_by_pitch            = EXCLUDED.hit_by_pitch, \
          \   sac_bunts               = EXCLUDED.sac_bunts, \
          \   sac_flies               = EXCLUDED.sac_flies, \
          \   ground_into_double_play = EXCLUDED.ground_into_double_play, \
          \   ground_into_triple_play = EXCLUDED.ground_into_triple_play, \
          \   left_on_base            = EXCLUDED.left_on_base, \
          \   total_bases             = EXCLUDED.total_bases, \
          \   fly_outs                = EXCLUDED.fly_outs, \
          \   ground_outs             = EXCLUDED.ground_outs, \
          \   catchers_interference   = EXCLUDED.catchers_interference, \
          \   pickoffs                = EXCLUDED.pickoffs, \
          \   updated_at              = NOW()"

upsertPitchingStmt :: Stmt.Statement PitchingRow ()
upsertPitchingStmt = Stmt.Statement sql pitchingEncoder D.noResult True
  where
    sql = "INSERT INTO game_player_pitching \
          \  (game_id, player_id, team_id, \
          \   games_played, games_started, games_finished, complete_games, \
          \   shutouts, wins, losses, saves, save_opportunities, holds, \
          \   blown_saves, innings_pitched_outs, batters_faced, \
          \   number_of_pitches, strikes, balls, hits, doubles, triples, \
          \   home_runs, runs, earned_runs, strike_outs, base_on_balls, \
          \   intentional_walks, hit_batsmen, wild_pitches, balks, pickoffs, \
          \   fly_outs, ground_outs, air_outs, inherited_runners, \
          \   inherited_runners_scored, stolen_bases, caught_stealing, \
          \   at_bats, rbi, sac_bunts, sac_flies, catchers_interference, \
          \   passed_ball) \
          \VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, \
          \        $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, \
          \        $25, $26, $27, $28, $29, $30, $31, $32, $33, $34, $35, \
          \        $36, $37, $38, $39, $40, $41, $42, $43, $44, $45) \
          \ON CONFLICT (game_id, player_id) DO UPDATE SET \
          \   team_id                  = EXCLUDED.team_id, \
          \   games_played             = EXCLUDED.games_played, \
          \   games_started            = EXCLUDED.games_started, \
          \   games_finished           = EXCLUDED.games_finished, \
          \   complete_games           = EXCLUDED.complete_games, \
          \   shutouts                 = EXCLUDED.shutouts, \
          \   wins                     = EXCLUDED.wins, \
          \   losses                   = EXCLUDED.losses, \
          \   saves                    = EXCLUDED.saves, \
          \   save_opportunities       = EXCLUDED.save_opportunities, \
          \   holds                    = EXCLUDED.holds, \
          \   blown_saves              = EXCLUDED.blown_saves, \
          \   innings_pitched_outs     = EXCLUDED.innings_pitched_outs, \
          \   batters_faced            = EXCLUDED.batters_faced, \
          \   number_of_pitches        = EXCLUDED.number_of_pitches, \
          \   strikes                  = EXCLUDED.strikes, \
          \   balls                    = EXCLUDED.balls, \
          \   hits                     = EXCLUDED.hits, \
          \   doubles                  = EXCLUDED.doubles, \
          \   triples                  = EXCLUDED.triples, \
          \   home_runs                = EXCLUDED.home_runs, \
          \   runs                     = EXCLUDED.runs, \
          \   earned_runs              = EXCLUDED.earned_runs, \
          \   strike_outs              = EXCLUDED.strike_outs, \
          \   base_on_balls            = EXCLUDED.base_on_balls, \
          \   intentional_walks        = EXCLUDED.intentional_walks, \
          \   hit_batsmen              = EXCLUDED.hit_batsmen, \
          \   wild_pitches             = EXCLUDED.wild_pitches, \
          \   balks                    = EXCLUDED.balks, \
          \   pickoffs                 = EXCLUDED.pickoffs, \
          \   fly_outs                 = EXCLUDED.fly_outs, \
          \   ground_outs              = EXCLUDED.ground_outs, \
          \   air_outs                 = EXCLUDED.air_outs, \
          \   inherited_runners        = EXCLUDED.inherited_runners, \
          \   inherited_runners_scored = EXCLUDED.inherited_runners_scored, \
          \   stolen_bases             = EXCLUDED.stolen_bases, \
          \   caught_stealing          = EXCLUDED.caught_stealing, \
          \   at_bats                  = EXCLUDED.at_bats, \
          \   rbi                      = EXCLUDED.rbi, \
          \   sac_bunts                = EXCLUDED.sac_bunts, \
          \   sac_flies                = EXCLUDED.sac_flies, \
          \   catchers_interference    = EXCLUDED.catchers_interference, \
          \   passed_ball              = EXCLUDED.passed_ball, \
          \   updated_at               = NOW()"

selectBattingForGameStmt :: Stmt.Statement DbGameId (V.Vector BattingRow)
selectBattingForGameStmt =
    Stmt.Statement sql encDbGameId (D.rowVector battingRowDecoder) True
  where
    sql = "SELECT game_id, player_id, team_id, \
          \       games_played, plate_appearances, at_bats, runs, hits, \
          \       doubles, triples, home_runs, rbi, base_on_balls, \
          \       intentional_walks, strike_outs, stolen_bases, \
          \       caught_stealing, hit_by_pitch, sac_bunts, sac_flies, \
          \       ground_into_double_play, ground_into_triple_play, \
          \       left_on_base, total_bases, fly_outs, ground_outs, \
          \       catchers_interference, pickoffs \
          \FROM game_player_batting \
          \WHERE game_id = $1 \
          \ORDER BY player_id"

selectPitchingForGameStmt :: Stmt.Statement DbGameId (V.Vector PitchingRow)
selectPitchingForGameStmt =
    Stmt.Statement sql encDbGameId (D.rowVector pitchingRowDecoder) True
  where
    sql = "SELECT game_id, player_id, team_id, \
          \       games_played, games_started, games_finished, \
          \       complete_games, shutouts, wins, losses, saves, \
          \       save_opportunities, holds, blown_saves, \
          \       innings_pitched_outs, batters_faced, number_of_pitches, \
          \       strikes, balls, hits, doubles, triples, home_runs, runs, \
          \       earned_runs, strike_outs, base_on_balls, intentional_walks, \
          \       hit_batsmen, wild_pitches, balks, pickoffs, fly_outs, \
          \       ground_outs, air_outs, inherited_runners, \
          \       inherited_runners_scored, stolen_bases, caught_stealing, \
          \       at_bats, rbi, sac_bunts, sac_flies, catchers_interference, \
          \       passed_ball \
          \FROM game_player_pitching \
          \WHERE game_id = $1 \
          \ORDER BY player_id"

deleteBattingStmt :: Stmt.Statement DbGameId ()
deleteBattingStmt = Stmt.Statement sql encDbGameId D.noResult True
  where
    sql = "DELETE FROM game_player_batting WHERE game_id = $1"

deletePitchingStmt :: Stmt.Statement DbGameId ()
deletePitchingStmt = Stmt.Statement sql encDbGameId D.noResult True
  where
    sql = "DELETE FROM game_player_pitching WHERE game_id = $1"