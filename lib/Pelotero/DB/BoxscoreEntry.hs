{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pelotero.DB.BoxscoreEntry
  ( BattingRow(..)
  , PitchingRow(..)
  , upsertBattingT
  , upsertPitchingT
  , getBattingForGameT
  , getPitchingForGameT
  , deleteBattingForGameT
  , deletePitchingForGameT
  , upsertBatting
  , upsertPitching
  , getBattingForGame
  , getPitchingForGame
  , deleteBattingForGame
  , deletePitchingForGame
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Int                   (Int32)
import           Data.Time                  (UTCTime)
import           GHC.Generics               (Generic)

import qualified Hasql.Transaction          as Tx

import           Rel8                       ( Column
                                            , Expr
                                            , Name
                                            , Rel8able
                                            , Result
                                            , TableSchema(..)
                                            , (==.)
                                            )
import qualified Rel8                       as R
import qualified Rel8.Expr.Time             as RT

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Rel8Instances ()
import Pelotero.Domain.Id    (DbGameId(..), DbPlayerId(..), DbTeamId(..))

-- ============================================================================
-- game_player_batting
-- ============================================================================

data BattingE f = BattingE
  { _bGameId                 :: Column f DbGameId
  , _bPlayerId               :: Column f DbPlayerId
  , _bTeamId                 :: Column f (Maybe DbTeamId)
  , _bGamesPlayed            :: Column f (Maybe Int32)
  , _bPlateAppearances       :: Column f (Maybe Int32)
  , _bAtBats                 :: Column f (Maybe Int32)
  , _bRuns                   :: Column f (Maybe Int32)
  , _bHits                   :: Column f (Maybe Int32)
  , _bDoubles                :: Column f (Maybe Int32)
  , _bTriples                :: Column f (Maybe Int32)
  , _bHomeRuns               :: Column f (Maybe Int32)
  , _bRbi                    :: Column f (Maybe Int32)
  , _bBaseOnBalls            :: Column f (Maybe Int32)
  , _bIntentionalWalks       :: Column f (Maybe Int32)
  , _bStrikeOuts             :: Column f (Maybe Int32)
  , _bStolenBases            :: Column f (Maybe Int32)
  , _bCaughtStealing         :: Column f (Maybe Int32)
  , _bHitByPitch             :: Column f (Maybe Int32)
  , _bSacBunts               :: Column f (Maybe Int32)
  , _bSacFlies               :: Column f (Maybe Int32)
  , _bGroundIntoDoublePlay   :: Column f (Maybe Int32)
  , _bGroundIntoTriplePlay   :: Column f (Maybe Int32)
  , _bLeftOnBase             :: Column f (Maybe Int32)
  , _bTotalBases             :: Column f (Maybe Int32)
  , _bFlyOuts                :: Column f (Maybe Int32)
  , _bGroundOuts             :: Column f (Maybe Int32)
  , _bCatchersInterference   :: Column f (Maybe Int32)
  , _bPickoffs               :: Column f (Maybe Int32)
  , _bCreatedAt              :: Column f UTCTime
  , _bUpdatedAt              :: Column f UTCTime
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (BattingE f)
deriving stock instance f ~ Result => Eq   (BattingE f)

battingSchema :: TableSchema (BattingE Name)
battingSchema = TableSchema
  { name    = "game_player_batting"
  , columns = BattingE
      { _bGameId                 = "game_id"
      , _bPlayerId               = "player_id"
      , _bTeamId                 = "team_id"
      , _bGamesPlayed            = "games_played"
      , _bPlateAppearances       = "plate_appearances"
      , _bAtBats                 = "at_bats"
      , _bRuns                   = "runs"
      , _bHits                   = "hits"
      , _bDoubles                = "doubles"
      , _bTriples                = "triples"
      , _bHomeRuns               = "home_runs"
      , _bRbi                    = "rbi"
      , _bBaseOnBalls            = "base_on_balls"
      , _bIntentionalWalks       = "intentional_walks"
      , _bStrikeOuts             = "strike_outs"
      , _bStolenBases            = "stolen_bases"
      , _bCaughtStealing         = "caught_stealing"
      , _bHitByPitch             = "hit_by_pitch"
      , _bSacBunts               = "sac_bunts"
      , _bSacFlies               = "sac_flies"
      , _bGroundIntoDoublePlay   = "ground_into_double_play"
      , _bGroundIntoTriplePlay   = "ground_into_triple_play"
      , _bLeftOnBase             = "left_on_base"
      , _bTotalBases             = "total_bases"
      , _bFlyOuts                = "fly_outs"
      , _bGroundOuts             = "ground_outs"
      , _bCatchersInterference   = "catchers_interference"
      , _bPickoffs               = "pickoffs"
      , _bCreatedAt              = "created_at"
      , _bUpdatedAt              = "updated_at"
      }
  }

-- ============================================================================
-- game_player_pitching
-- ============================================================================

data PitchingE f = PitchingE
  { _pGameId                  :: Column f DbGameId
  , _pPlayerId                :: Column f DbPlayerId
  , _pTeamId                  :: Column f (Maybe DbTeamId)
  , _pGamesPlayed             :: Column f (Maybe Int32)
  , _pGamesStarted            :: Column f (Maybe Int32)
  , _pGamesFinished           :: Column f (Maybe Int32)
  , _pCompleteGames           :: Column f (Maybe Int32)
  , _pShutouts                :: Column f (Maybe Int32)
  , _pWins                    :: Column f (Maybe Int32)
  , _pLosses                  :: Column f (Maybe Int32)
  , _pSaves                   :: Column f (Maybe Int32)
  , _pSaveOpportunities       :: Column f (Maybe Int32)
  , _pHolds                   :: Column f (Maybe Int32)
  , _pBlownSaves              :: Column f (Maybe Int32)
  , _pInningsPitchedOuts      :: Column f (Maybe Int32)
  , _pBattersFaced            :: Column f (Maybe Int32)
  , _pNumberOfPitches         :: Column f (Maybe Int32)
  , _pStrikes                 :: Column f (Maybe Int32)
  , _pBalls                   :: Column f (Maybe Int32)
  , _pHits                    :: Column f (Maybe Int32)
  , _pDoubles                 :: Column f (Maybe Int32)
  , _pTriples                 :: Column f (Maybe Int32)
  , _pHomeRuns                :: Column f (Maybe Int32)
  , _pRuns                    :: Column f (Maybe Int32)
  , _pEarnedRuns              :: Column f (Maybe Int32)
  , _pStrikeOuts              :: Column f (Maybe Int32)
  , _pBaseOnBalls             :: Column f (Maybe Int32)
  , _pIntentionalWalks        :: Column f (Maybe Int32)
  , _pHitBatsmen              :: Column f (Maybe Int32)
  , _pWildPitches             :: Column f (Maybe Int32)
  , _pBalks                   :: Column f (Maybe Int32)
  , _pPickoffs                :: Column f (Maybe Int32)
  , _pFlyOuts                 :: Column f (Maybe Int32)
  , _pGroundOuts              :: Column f (Maybe Int32)
  , _pAirOuts                 :: Column f (Maybe Int32)
  , _pInheritedRunners        :: Column f (Maybe Int32)
  , _pInheritedRunnersScored  :: Column f (Maybe Int32)
  , _pStolenBases             :: Column f (Maybe Int32)
  , _pCaughtStealing          :: Column f (Maybe Int32)
  , _pAtBats                  :: Column f (Maybe Int32)
  , _pRbi                     :: Column f (Maybe Int32)
  , _pSacBunts                :: Column f (Maybe Int32)
  , _pSacFlies                :: Column f (Maybe Int32)
  , _pCatchersInterference    :: Column f (Maybe Int32)
  , _pPassedBall              :: Column f (Maybe Int32)
  , _pCreatedAt               :: Column f UTCTime
  , _pUpdatedAt               :: Column f UTCTime
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (PitchingE f)
deriving stock instance f ~ Result => Eq   (PitchingE f)

pitchingSchema :: TableSchema (PitchingE Name)
pitchingSchema = TableSchema
  { name    = "game_player_pitching"
  , columns = PitchingE
      { _pGameId                 = "game_id"
      , _pPlayerId               = "player_id"
      , _pTeamId                 = "team_id"
      , _pGamesPlayed            = "games_played"
      , _pGamesStarted           = "games_started"
      , _pGamesFinished          = "games_finished"
      , _pCompleteGames          = "complete_games"
      , _pShutouts               = "shutouts"
      , _pWins                   = "wins"
      , _pLosses                 = "losses"
      , _pSaves                  = "saves"
      , _pSaveOpportunities      = "save_opportunities"
      , _pHolds                  = "holds"
      , _pBlownSaves             = "blown_saves"
      , _pInningsPitchedOuts     = "innings_pitched_outs"
      , _pBattersFaced           = "batters_faced"
      , _pNumberOfPitches        = "number_of_pitches"
      , _pStrikes                = "strikes"
      , _pBalls                  = "balls"
      , _pHits                   = "hits"
      , _pDoubles                = "doubles"
      , _pTriples                = "triples"
      , _pHomeRuns               = "home_runs"
      , _pRuns                   = "runs"
      , _pEarnedRuns             = "earned_runs"
      , _pStrikeOuts             = "strike_outs"
      , _pBaseOnBalls            = "base_on_balls"
      , _pIntentionalWalks       = "intentional_walks"
      , _pHitBatsmen             = "hit_batsmen"
      , _pWildPitches            = "wild_pitches"
      , _pBalks                  = "balks"
      , _pPickoffs               = "pickoffs"
      , _pFlyOuts                = "fly_outs"
      , _pGroundOuts             = "ground_outs"
      , _pAirOuts                = "air_outs"
      , _pInheritedRunners       = "inherited_runners"
      , _pInheritedRunnersScored = "inherited_runners_scored"
      , _pStolenBases            = "stolen_bases"
      , _pCaughtStealing         = "caught_stealing"
      , _pAtBats                 = "at_bats"
      , _pRbi                    = "rbi"
      , _pSacBunts               = "sac_bunts"
      , _pSacFlies               = "sac_flies"
      , _pCatchersInterference   = "catchers_interference"
      , _pPassedBall             = "passed_ball"
      , _pCreatedAt              = "created_at"
      , _pUpdatedAt              = "updated_at"
      }
  }

-- ============================================================================
-- Public row types (API compatibility with old hasql module)
-- ============================================================================

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

fromBattingResult :: BattingE Result -> BattingRow
fromBattingResult BattingE{..} = BattingRow
  { battingGameId                 = _bGameId
  , battingPlayerId               = _bPlayerId
  , battingTeamId                 = _bTeamId
  , battingGamesPlayed            = _bGamesPlayed
  , battingPlateAppearances       = _bPlateAppearances
  , battingAtBats                 = _bAtBats
  , battingRuns                   = _bRuns
  , battingHits                   = _bHits
  , battingDoubles                = _bDoubles
  , battingTriples                = _bTriples
  , battingHomeRuns               = _bHomeRuns
  , battingRbi                    = _bRbi
  , battingBaseOnBalls            = _bBaseOnBalls
  , battingIntentionalWalks       = _bIntentionalWalks
  , battingStrikeOuts             = _bStrikeOuts
  , battingStolenBases            = _bStolenBases
  , battingCaughtStealing         = _bCaughtStealing
  , battingHitByPitch             = _bHitByPitch
  , battingSacBunts               = _bSacBunts
  , battingSacFlies               = _bSacFlies
  , battingGroundIntoDoublePlay   = _bGroundIntoDoublePlay
  , battingGroundIntoTriplePlay   = _bGroundIntoTriplePlay
  , battingLeftOnBase             = _bLeftOnBase
  , battingTotalBases             = _bTotalBases
  , battingFlyOuts                = _bFlyOuts
  , battingGroundOuts             = _bGroundOuts
  , battingCatchersInterference   = _bCatchersInterference
  , battingPickoffs               = _bPickoffs
  }

fromPitchingResult :: PitchingE Result -> PitchingRow
fromPitchingResult PitchingE{..} = PitchingRow
  { pitchingGameId                 = _pGameId
  , pitchingPlayerId               = _pPlayerId
  , pitchingTeamId                 = _pTeamId
  , pitchingGamesPlayed            = _pGamesPlayed
  , pitchingGamesStarted           = _pGamesStarted
  , pitchingGamesFinished          = _pGamesFinished
  , pitchingCompleteGames          = _pCompleteGames
  , pitchingShutouts               = _pShutouts
  , pitchingWins                   = _pWins
  , pitchingLosses                 = _pLosses
  , pitchingSaves                  = _pSaves
  , pitchingSaveOpportunities      = _pSaveOpportunities
  , pitchingHolds                  = _pHolds
  , pitchingBlownSaves             = _pBlownSaves
  , pitchingInningsPitchedOuts     = _pInningsPitchedOuts
  , pitchingBattersFaced           = _pBattersFaced
  , pitchingNumberOfPitches        = _pNumberOfPitches
  , pitchingStrikes                = _pStrikes
  , pitchingBalls                  = _pBalls
  , pitchingHits                   = _pHits
  , pitchingDoubles                = _pDoubles
  , pitchingTriples                = _pTriples
  , pitchingHomeRuns               = _pHomeRuns
  , pitchingRuns                   = _pRuns
  , pitchingEarnedRuns             = _pEarnedRuns
  , pitchingStrikeOuts             = _pStrikeOuts
  , pitchingBaseOnBalls            = _pBaseOnBalls
  , pitchingIntentionalWalks       = _pIntentionalWalks
  , pitchingHitBatsmen             = _pHitBatsmen
  , pitchingWildPitches            = _pWildPitches
  , pitchingBalks                  = _pBalks
  , pitchingPickoffs               = _pPickoffs
  , pitchingFlyOuts                = _pFlyOuts
  , pitchingGroundOuts             = _pGroundOuts
  , pitchingAirOuts                = _pAirOuts
  , pitchingInheritedRunners       = _pInheritedRunners
  , pitchingInheritedRunnersScored = _pInheritedRunnersScored
  , pitchingStolenBases            = _pStolenBases
  , pitchingCaughtStealing         = _pCaughtStealing
  , pitchingAtBats                 = _pAtBats
  , pitchingRbi                    = _pRbi
  , pitchingSacBunts               = _pSacBunts
  , pitchingSacFlies               = _pSacFlies
  , pitchingCatchersInterference   = _pCatchersInterference
  , pitchingPassedBall             = _pPassedBall
  }

battingRowToExpr :: BattingRow -> BattingE Expr
battingRowToExpr r = BattingE
  { _bGameId                 = R.lit (battingGameId r)
  , _bPlayerId               = R.lit (battingPlayerId r)
  , _bTeamId                 = R.lit (battingTeamId r)
  , _bGamesPlayed            = R.lit (battingGamesPlayed r)
  , _bPlateAppearances       = R.lit (battingPlateAppearances r)
  , _bAtBats                 = R.lit (battingAtBats r)
  , _bRuns                   = R.lit (battingRuns r)
  , _bHits                   = R.lit (battingHits r)
  , _bDoubles                = R.lit (battingDoubles r)
  , _bTriples                = R.lit (battingTriples r)
  , _bHomeRuns               = R.lit (battingHomeRuns r)
  , _bRbi                    = R.lit (battingRbi r)
  , _bBaseOnBalls            = R.lit (battingBaseOnBalls r)
  , _bIntentionalWalks       = R.lit (battingIntentionalWalks r)
  , _bStrikeOuts             = R.lit (battingStrikeOuts r)
  , _bStolenBases            = R.lit (battingStolenBases r)
  , _bCaughtStealing         = R.lit (battingCaughtStealing r)
  , _bHitByPitch             = R.lit (battingHitByPitch r)
  , _bSacBunts               = R.lit (battingSacBunts r)
  , _bSacFlies               = R.lit (battingSacFlies r)
  , _bGroundIntoDoublePlay   = R.lit (battingGroundIntoDoublePlay r)
  , _bGroundIntoTriplePlay   = R.lit (battingGroundIntoTriplePlay r)
  , _bLeftOnBase             = R.lit (battingLeftOnBase r)
  , _bTotalBases             = R.lit (battingTotalBases r)
  , _bFlyOuts                = R.lit (battingFlyOuts r)
  , _bGroundOuts             = R.lit (battingGroundOuts r)
  , _bCatchersInterference   = R.lit (battingCatchersInterference r)
  , _bPickoffs               = R.lit (battingPickoffs r)
  , _bCreatedAt              = R.unsafeDefault
  , _bUpdatedAt              = R.unsafeDefault
  }

pitchingRowToExpr :: PitchingRow -> PitchingE Expr
pitchingRowToExpr r = PitchingE
  { _pGameId                  = R.lit (pitchingGameId r)
  , _pPlayerId                = R.lit (pitchingPlayerId r)
  , _pTeamId                  = R.lit (pitchingTeamId r)
  , _pGamesPlayed             = R.lit (pitchingGamesPlayed r)
  , _pGamesStarted            = R.lit (pitchingGamesStarted r)
  , _pGamesFinished           = R.lit (pitchingGamesFinished r)
  , _pCompleteGames           = R.lit (pitchingCompleteGames r)
  , _pShutouts                = R.lit (pitchingShutouts r)
  , _pWins                    = R.lit (pitchingWins r)
  , _pLosses                  = R.lit (pitchingLosses r)
  , _pSaves                   = R.lit (pitchingSaves r)
  , _pSaveOpportunities       = R.lit (pitchingSaveOpportunities r)
  , _pHolds                   = R.lit (pitchingHolds r)
  , _pBlownSaves              = R.lit (pitchingBlownSaves r)
  , _pInningsPitchedOuts      = R.lit (pitchingInningsPitchedOuts r)
  , _pBattersFaced            = R.lit (pitchingBattersFaced r)
  , _pNumberOfPitches         = R.lit (pitchingNumberOfPitches r)
  , _pStrikes                 = R.lit (pitchingStrikes r)
  , _pBalls                   = R.lit (pitchingBalls r)
  , _pHits                    = R.lit (pitchingHits r)
  , _pDoubles                 = R.lit (pitchingDoubles r)
  , _pTriples                 = R.lit (pitchingTriples r)
  , _pHomeRuns                = R.lit (pitchingHomeRuns r)
  , _pRuns                    = R.lit (pitchingRuns r)
  , _pEarnedRuns              = R.lit (pitchingEarnedRuns r)
  , _pStrikeOuts              = R.lit (pitchingStrikeOuts r)
  , _pBaseOnBalls             = R.lit (pitchingBaseOnBalls r)
  , _pIntentionalWalks        = R.lit (pitchingIntentionalWalks r)
  , _pHitBatsmen              = R.lit (pitchingHitBatsmen r)
  , _pWildPitches             = R.lit (pitchingWildPitches r)
  , _pBalks                   = R.lit (pitchingBalks r)
  , _pPickoffs                = R.lit (pitchingPickoffs r)
  , _pFlyOuts                 = R.lit (pitchingFlyOuts r)
  , _pGroundOuts              = R.lit (pitchingGroundOuts r)
  , _pAirOuts                 = R.lit (pitchingAirOuts r)
  , _pInheritedRunners        = R.lit (pitchingInheritedRunners r)
  , _pInheritedRunnersScored  = R.lit (pitchingInheritedRunnersScored r)
  , _pStolenBases             = R.lit (pitchingStolenBases r)
  , _pCaughtStealing          = R.lit (pitchingCaughtStealing r)
  , _pAtBats                  = R.lit (pitchingAtBats r)
  , _pRbi                     = R.lit (pitchingRbi r)
  , _pSacBunts                = R.lit (pitchingSacBunts r)
  , _pSacFlies                = R.lit (pitchingSacFlies r)
  , _pCatchersInterference    = R.lit (pitchingCatchersInterference r)
  , _pPassedBall              = R.lit (pitchingPassedBall r)
  , _pCreatedAt               = R.unsafeDefault
  , _pUpdatedAt               = R.unsafeDefault
  }

-- ============================================================================
-- Transaction-flavored CRUD: batting
-- ============================================================================

upsertBattingT :: BattingRow -> Tx.Transaction ()
upsertBattingT row = Tx.statement () $ R.run_ $ R.insert R.Insert
  { R.into       = battingSchema
  , R.rows       = R.values [ battingRowToExpr row ]
  , R.onConflict = R.DoUpdate R.Upsert
      { R.index       = \r -> (_bGameId r, _bPlayerId r)
      , R.predicate   = Nothing
      , R.set         = \new old -> BattingE
          { _bGameId                 = _bGameId old
          , _bPlayerId               = _bPlayerId old
          , _bTeamId                 = _bTeamId new
          , _bGamesPlayed            = _bGamesPlayed new
          , _bPlateAppearances       = _bPlateAppearances new
          , _bAtBats                 = _bAtBats new
          , _bRuns                   = _bRuns new
          , _bHits                   = _bHits new
          , _bDoubles                = _bDoubles new
          , _bTriples                = _bTriples new
          , _bHomeRuns               = _bHomeRuns new
          , _bRbi                    = _bRbi new
          , _bBaseOnBalls            = _bBaseOnBalls new
          , _bIntentionalWalks       = _bIntentionalWalks new
          , _bStrikeOuts             = _bStrikeOuts new
          , _bStolenBases            = _bStolenBases new
          , _bCaughtStealing         = _bCaughtStealing new
          , _bHitByPitch             = _bHitByPitch new
          , _bSacBunts               = _bSacBunts new
          , _bSacFlies               = _bSacFlies new
          , _bGroundIntoDoublePlay   = _bGroundIntoDoublePlay new
          , _bGroundIntoTriplePlay   = _bGroundIntoTriplePlay new
          , _bLeftOnBase             = _bLeftOnBase new
          , _bTotalBases             = _bTotalBases new
          , _bFlyOuts                = _bFlyOuts new
          , _bGroundOuts             = _bGroundOuts new
          , _bCatchersInterference   = _bCatchersInterference new
          , _bPickoffs               = _bPickoffs new
          , _bCreatedAt              = _bCreatedAt old
          , _bUpdatedAt              = RT.now
          }
      , R.updateWhere = \_ _ -> R.lit True
      }
  , R.returning  = R.NoReturning
  }

getBattingForGameT :: DbGameId -> Tx.Transaction [BattingRow]
getBattingForGameT gid = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy (_bPlayerId >$< R.asc) $ do
      r <- R.each battingSchema
      R.where_ (_bGameId r ==. R.lit gid)
      pure r
  pure (map fromBattingResult rows)

deleteBattingForGameT :: DbGameId -> Tx.Transaction ()
deleteBattingForGameT gid = Tx.statement () $ R.run_ $ R.delete R.Delete
  { R.from        = battingSchema
  , R.using       = pure ()
  , R.deleteWhere = \_ r -> _bGameId r ==. R.lit gid
  , R.returning   = R.NoReturning
  }

-- ============================================================================
-- Transaction-flavored CRUD: pitching
-- ============================================================================

upsertPitchingT :: PitchingRow -> Tx.Transaction ()
upsertPitchingT row = Tx.statement () $ R.run_ $ R.insert R.Insert
  { R.into       = pitchingSchema
  , R.rows       = R.values [ pitchingRowToExpr row ]
  , R.onConflict = R.DoUpdate R.Upsert
      { R.index       = \r -> (_pGameId r, _pPlayerId r)
      , R.predicate   = Nothing
      , R.set         = \new old -> PitchingE
          { _pGameId                  = _pGameId old
          , _pPlayerId                = _pPlayerId old
          , _pTeamId                  = _pTeamId new
          , _pGamesPlayed             = _pGamesPlayed new
          , _pGamesStarted            = _pGamesStarted new
          , _pGamesFinished           = _pGamesFinished new
          , _pCompleteGames           = _pCompleteGames new
          , _pShutouts                = _pShutouts new
          , _pWins                    = _pWins new
          , _pLosses                  = _pLosses new
          , _pSaves                   = _pSaves new
          , _pSaveOpportunities       = _pSaveOpportunities new
          , _pHolds                   = _pHolds new
          , _pBlownSaves              = _pBlownSaves new
          , _pInningsPitchedOuts      = _pInningsPitchedOuts new
          , _pBattersFaced            = _pBattersFaced new
          , _pNumberOfPitches         = _pNumberOfPitches new
          , _pStrikes                 = _pStrikes new
          , _pBalls                   = _pBalls new
          , _pHits                    = _pHits new
          , _pDoubles                 = _pDoubles new
          , _pTriples                 = _pTriples new
          , _pHomeRuns                = _pHomeRuns new
          , _pRuns                    = _pRuns new
          , _pEarnedRuns              = _pEarnedRuns new
          , _pStrikeOuts              = _pStrikeOuts new
          , _pBaseOnBalls             = _pBaseOnBalls new
          , _pIntentionalWalks        = _pIntentionalWalks new
          , _pHitBatsmen              = _pHitBatsmen new
          , _pWildPitches             = _pWildPitches new
          , _pBalks                   = _pBalks new
          , _pPickoffs                = _pPickoffs new
          , _pFlyOuts                 = _pFlyOuts new
          , _pGroundOuts              = _pGroundOuts new
          , _pAirOuts                 = _pAirOuts new
          , _pInheritedRunners        = _pInheritedRunners new
          , _pInheritedRunnersScored  = _pInheritedRunnersScored new
          , _pStolenBases             = _pStolenBases new
          , _pCaughtStealing          = _pCaughtStealing new
          , _pAtBats                  = _pAtBats new
          , _pRbi                     = _pRbi new
          , _pSacBunts                = _pSacBunts new
          , _pSacFlies                = _pSacFlies new
          , _pCatchersInterference    = _pCatchersInterference new
          , _pPassedBall              = _pPassedBall new
          , _pCreatedAt               = _pCreatedAt old
          , _pUpdatedAt               = RT.now
          }
      , R.updateWhere = \_ _ -> R.lit True
      }
  , R.returning  = R.NoReturning
  }

getPitchingForGameT :: DbGameId -> Tx.Transaction [PitchingRow]
getPitchingForGameT gid = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy (_pPlayerId >$< R.asc) $ do
      r <- R.each pitchingSchema
      R.where_ (_pGameId r ==. R.lit gid)
      pure r
  pure (map fromPitchingResult rows)

deletePitchingForGameT :: DbGameId -> Tx.Transaction ()
deletePitchingForGameT gid = Tx.statement () $ R.run_ $ R.delete R.Delete
  { R.from        = pitchingSchema
  , R.using       = pure ()
  , R.deleteWhere = \_ r -> _pGameId r ==. R.lit gid
  , R.returning   = R.NoReturning
  }

-- ============================================================================
-- Pool-flavored CRUD
-- ============================================================================

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