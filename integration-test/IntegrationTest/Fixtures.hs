{-# LANGUAGE OverloadedStrings #-}

-- | Shared, typed test fixtures for the integration suite.
--
-- Pure builders (mkXxxRow) return a default 'XxxRow' tagged by 'Text'
-- so callers can use record-update syntax to override fields:
--
-- > let cfg = (mkLeagueConfigRow "active-league") { lcStatus = "active" }
--
-- Tx-level helpers (addXxxT) compose a builder with the corresponding
-- repo insert, so the most common shapes are one line:
--
-- > (lcid, ltid) <- addLeagueTeamWithConfigT "smoke"
--
-- Use the *Row builders when you need to inspect or modify the row
-- before insert; use the addXxxT helpers when you just need an id.
module IntegrationTest.Fixtures
  ( -- * Time
    epochUtc
    -- * Pure row builders
  , mkTeamRow
  , mkPlayerRow
  , mkGameRow
  , mkLeagueConfigRow
  , mkLeagueTeamRow
  , zeroBatting
  , zeroPitching
    -- * Scoring presets
  , zeroScoring
  , standardScoring
  , singlesOnlyScoring
    -- * Limit presets
  , emptyRosterLimits
  , emptyLineupLimits
  , standardRosterLimits
  , standardLineupLimits
    -- * Tx-level "build + insert" helpers
  , addLeagueConfigT
  , addLeagueTeamT
  , addLeagueTeamWithConfigT
  , addPlayerT
  , addTeamT
  , addGameT
  , addTeamPairT
  ) where

import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import           Data.Time                (Day, UTCTime(..), fromGregorian, secondsToDiffTime)

import qualified Hasql.Transaction        as Tx

import qualified Pelotero.DB.Game         as Game
import qualified Pelotero.DB.LeagueConfig as LC
import qualified Pelotero.DB.LeagueTeam   as LT
import qualified Pelotero.DB.Player       as P
import qualified Pelotero.DB.Team         as Tm
import           Pelotero.DB.BoxscoreEntry (BattingRow(..), PitchingRow(..))
import           Pelotero.DB.Game          (GameRow(..))
import           Pelotero.DB.LeagueConfig  (LeagueConfigRow(..))
import           Pelotero.DB.LeagueTeam    (LeagueTeamRow(..))
import           Pelotero.DB.Player        (PlayerRow(..))
import           Pelotero.DB.Provider      (ProviderName(..))
import           Pelotero.DB.Team          (TeamRow(..))
import           Pelotero.Domain.Id
                   ( DbGameId
                   , DbLeagueConfigId
                   , DbLeagueTeamId
                   , DbPlayerId
                   , DbTeamId
                   )
import           Pelotero.Domain.Roster
                   ( LineupLimits(..)
                   , RosterLimits(..)
                   , RosterSlot(..)
                   )
import           Pelotero.Domain.Scoring
                   ( BattingMultipliers(..)
                   , LeagueScoring(..)
                   , PitchingMultipliers(..)
                   )

-- | A stable epoch UTCTime used for fields that don't matter to the
-- test under inspection. Tests that care about time should override.
epochUtc :: UTCTime
epochUtc = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

------------------------------------------------------------------------
-- Pure row builders
------------------------------------------------------------------------

mkTeamRow :: Text -> Text -> TeamRow
mkTeamRow name abbr = TeamRow
  { teamRowId                 = Nothing
  , teamRowName               = name
  , teamRowAbbreviation       = abbr
  , teamRowLocationName       = "Anywhere"
  , teamRowLastSyncedProvider = Just ProviderMLB
  , teamRowLastSyncedAt       = Nothing
  }

mkPlayerRow :: Text -> PlayerRow
mkPlayerRow tag = PlayerRow
  { playerRowId                 = Nothing
  , playerRowFirstName          = tag <> "-first"
  , playerRowLastName           = tag <> "-last"
  , playerRowNameSlug           = tag <> "-slug"
  , playerRowPosition           = Nothing
  , playerRowBatSide            = Nothing
  , playerRowPitchHand          = Nothing
  , playerRowActive             = True
  , playerRowCurrentTeamId      = Nothing
  , playerRowLastSyncedProvider = Just ProviderMLB
  , playerRowLastSyncedAt       = Nothing
  }

mkGameRow :: Day -> DbTeamId -> DbTeamId -> GameRow
mkGameRow d at ht = GameRow
  { gameRowId                 = Nothing
  , gameRowGameDate           = d
  , gameRowAwayTeamId         = at
  , gameRowHomeTeamId         = ht
  , gameRowLastSyncedProvider = Just ProviderMLB
  , gameRowLastSyncedAt       = Nothing
  }

mkLeagueConfigRow :: Text -> LeagueConfigRow
mkLeagueConfigRow tag = LeagueConfigRow
  { lcId            = Nothing
  , lcLeagueId      = tag <> "-league"
  , lcCommissioner  = "test-commish"
  , lcStatus        = "draft"
  , lcScoring       = zeroScoring
  , lcRosterLimits  = emptyRosterLimits
  , lcLineupLimits  = emptyLineupLimits
  , lcDraftAuto     = False
  , lcDraftStrategy = "serpentine"
  , lcDraftAutoAt   = Nothing
  , lcScoringStart  = epochUtc
  , lcScoringEnd    = epochUtc
  }

mkLeagueTeamRow :: DbLeagueConfigId -> Text -> LeagueTeamRow
mkLeagueTeamRow lcid tag = LeagueTeamRow
  { ltId             = Nothing
  , ltLeagueConfigId = lcid
  , ltTeamKey        = tag <> "-key"
  , ltName           = tag <> "-name"
  , ltOwner          = tag <> "-owner"
  }

-- | A 'BattingRow' with all stat columns set to 'Nothing'. Use record
-- update syntax to set the stats relevant to your test.
zeroBatting :: DbGameId -> DbPlayerId -> BattingRow
zeroBatting gid pid = BattingRow
  { battingGameId               = gid
  , battingPlayerId             = pid
  , battingTeamId               = Nothing
  , battingGamesPlayed          = Nothing
  , battingPlateAppearances     = Nothing
  , battingAtBats               = Nothing
  , battingRuns                 = Nothing
  , battingHits                 = Nothing
  , battingDoubles              = Nothing
  , battingTriples              = Nothing
  , battingHomeRuns             = Nothing
  , battingRbi                  = Nothing
  , battingBaseOnBalls          = Nothing
  , battingIntentionalWalks     = Nothing
  , battingStrikeOuts           = Nothing
  , battingStolenBases          = Nothing
  , battingCaughtStealing       = Nothing
  , battingHitByPitch           = Nothing
  , battingSacBunts             = Nothing
  , battingSacFlies             = Nothing
  , battingGroundIntoDoublePlay = Nothing
  , battingGroundIntoTriplePlay = Nothing
  , battingLeftOnBase           = Nothing
  , battingTotalBases           = Nothing
  , battingFlyOuts              = Nothing
  , battingGroundOuts           = Nothing
  , battingCatchersInterference = Nothing
  , battingPickoffs             = Nothing
  }

-- | A 'PitchingRow' with all stat columns set to 'Nothing'.
zeroPitching :: DbGameId -> DbPlayerId -> PitchingRow
zeroPitching gid pid = PitchingRow
  { pitchingGameId                 = gid
  , pitchingPlayerId               = pid
  , pitchingTeamId                 = Nothing
  , pitchingGamesPlayed            = Nothing
  , pitchingGamesStarted           = Nothing
  , pitchingGamesFinished          = Nothing
  , pitchingCompleteGames          = Nothing
  , pitchingShutouts               = Nothing
  , pitchingWins                   = Nothing
  , pitchingLosses                 = Nothing
  , pitchingSaves                  = Nothing
  , pitchingSaveOpportunities      = Nothing
  , pitchingHolds                  = Nothing
  , pitchingBlownSaves             = Nothing
  , pitchingInningsPitchedOuts     = Nothing
  , pitchingBattersFaced           = Nothing
  , pitchingNumberOfPitches        = Nothing
  , pitchingStrikes                = Nothing
  , pitchingBalls                  = Nothing
  , pitchingHits                   = Nothing
  , pitchingDoubles                = Nothing
  , pitchingTriples                = Nothing
  , pitchingHomeRuns               = Nothing
  , pitchingRuns                   = Nothing
  , pitchingEarnedRuns             = Nothing
  , pitchingStrikeOuts             = Nothing
  , pitchingBaseOnBalls            = Nothing
  , pitchingIntentionalWalks       = Nothing
  , pitchingHitBatsmen             = Nothing
  , pitchingWildPitches            = Nothing
  , pitchingBalks                  = Nothing
  , pitchingPickoffs               = Nothing
  , pitchingFlyOuts                = Nothing
  , pitchingGroundOuts             = Nothing
  , pitchingAirOuts                = Nothing
  , pitchingInheritedRunners       = Nothing
  , pitchingInheritedRunnersScored = Nothing
  , pitchingStolenBases            = Nothing
  , pitchingCaughtStealing         = Nothing
  , pitchingAtBats                 = Nothing
  , pitchingRbi                    = Nothing
  , pitchingSacBunts               = Nothing
  , pitchingSacFlies               = Nothing
  , pitchingCatchersInterference   = Nothing
  , pitchingPassedBall             = Nothing
  }

------------------------------------------------------------------------
-- Scoring presets
------------------------------------------------------------------------

-- | Every multiplier 0. Use as a base for "scoring doesn't matter" tests.
zeroScoring :: LeagueScoring
zeroScoring = LeagueScoring
  { lsBatting = BattingMultipliers
      { bmSingle = 0, bmDouble = 0, bmTriple = 0, bmHomeRun = 0
      , bmRbi = 0, bmRun = 0, bmBaseOnBalls = 0, bmStolenBase = 0
      , bmHitByPitch = 0, bmStrikeOut = 0, bmCaughtStealing = 0
      }
  , lsPitching = PitchingMultipliers
      { pmWin = 0, pmSave = 0, pmQualityStart = 0, pmInningPitched = 0
      , pmStrikeOut = 0, pmCompleteGame = 0, pmShutout = 0
      , pmBaseOnBalls = 0, pmHitsAllowed = 0, pmEarnedRun = 0
      , pmHitBatsman = 0, pmLoss = 0
      }
  }

-- | A "typical" multiplier set: 1/2/3/4 for hits, 5 for W/SV, etc.
-- Used by ScoreSpec; reuse anywhere you need realistic non-zero scoring.
standardScoring :: LeagueScoring
standardScoring = LeagueScoring
  { lsBatting = BattingMultipliers
      { bmSingle = 1, bmDouble = 2, bmTriple = 3, bmHomeRun = 4
      , bmRbi = 1, bmRun = 1, bmBaseOnBalls = 1, bmStolenBase = 2
      , bmHitByPitch = 1, bmStrikeOut = -1, bmCaughtStealing = -1
      }
  , lsPitching = PitchingMultipliers
      { pmWin = 5, pmSave = 5, pmQualityStart = 4, pmInningPitched = 3
      , pmStrikeOut = 1, pmCompleteGame = 5, pmShutout = 5
      , pmBaseOnBalls = -1, pmHitsAllowed = 0, pmEarnedRun = -1
      , pmHitBatsman = -1, pmLoss = -3
      }
  }

-- | One point per single, everything else zero. Used by SmokeSpec for
-- deterministic point arithmetic.
singlesOnlyScoring :: LeagueScoring
singlesOnlyScoring = LeagueScoring
  { lsBatting = (lsBatting zeroScoring) { bmSingle = 1 }
  , lsPitching = lsPitching zeroScoring
  }

------------------------------------------------------------------------
-- Limit presets
------------------------------------------------------------------------

emptyRosterLimits :: RosterLimits
emptyRosterLimits = RosterLimits Map.empty

emptyLineupLimits :: LineupLimits
emptyLineupLimits = LineupLimits Map.empty

-- | A reasonable filled-out RosterLimits for tests that exercise JSONB
-- round-tripping or lineup validation.
standardRosterLimits :: RosterLimits
standardRosterLimits = RosterLimits $ Map.fromList
  [ (SlotCatcher,         1)
  , (SlotFirstBase,       1)
  , (SlotSecondBase,      1)
  , (SlotThirdBase,       1)
  , (SlotShortstop,       1)
  , (SlotOutfield,        3)
  , (SlotUtility,         1)
  , (SlotStartingPitcher, 2)
  , (SlotReliefPitcher,   2)
  ]

standardLineupLimits :: LineupLimits
standardLineupLimits = LineupLimits $ Map.fromList
  [ (SlotCatcher,         1)
  , (SlotFirstBase,       1)
  , (SlotOutfield,        2)
  , (SlotStartingPitcher, 1)
  ]

------------------------------------------------------------------------
-- Tx-level "build + insert" helpers
--
-- These are the most common shapes. If your test needs to override
-- fields, build a row with mkXxxRow and call the repo insert directly.
------------------------------------------------------------------------

-- | Insert a default-tagged league config; return its surrogate id.
addLeagueConfigT :: Text -> Tx.Transaction DbLeagueConfigId
addLeagueConfigT tag = LC.insertLeagueConfigT (mkLeagueConfigRow tag)

-- | Insert a league team under an existing league config.
addLeagueTeamT :: DbLeagueConfigId -> Text -> Tx.Transaction DbLeagueTeamId
addLeagueTeamT lcid tag = LT.insertLeagueTeamT (mkLeagueTeamRow lcid tag)

-- | Insert a fresh league config and a single team within it.
-- Convenient for tests that only care about (lcid, ltid).
addLeagueTeamWithConfigT
  :: Text -> Tx.Transaction (DbLeagueConfigId, DbLeagueTeamId)
addLeagueTeamWithConfigT tag = do
  lcid <- addLeagueConfigT tag
  ltid <- addLeagueTeamT lcid tag
  pure (lcid, ltid)

-- | Insert a default-tagged player; return its surrogate id.
addPlayerT :: Text -> Tx.Transaction DbPlayerId
addPlayerT tag = P.insertPlayerT (mkPlayerRow tag)

-- | Insert a team with the given name and abbreviation.
addTeamT :: Text -> Text -> Tx.Transaction DbTeamId
addTeamT name abbr = Tm.insertTeamT (mkTeamRow name abbr)

-- | Insert a game between two existing teams on the given date.
addGameT :: Day -> DbTeamId -> DbTeamId -> Tx.Transaction DbGameId
addGameT d at ht = Game.insertGameT (mkGameRow d at ht)

-- | Convenience for tests that need away+home: inserts two teams with
-- the given (name, abbreviation) pairs and returns (awayId, homeId).
addTeamPairT
  :: (Text, Text)  -- ^ away (name, abbr)
  -> (Text, Text)  -- ^ home (name, abbr)
  -> Tx.Transaction (DbTeamId, DbTeamId)
addTeamPairT (an, aa) (hn, ha) = do
  atid <- addTeamT an aa
  htid <- addTeamT hn ha
  pure (atid, htid)