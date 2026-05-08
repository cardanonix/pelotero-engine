{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

module IntegrationTest.ScoreSpec (spec) where

import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import           Data.Time                (UTCTime(..), fromGregorian, secondsToDiffTime)
import           Test.Hspec

import           Effectful                (runEff)
import           Effectful.Error.Static   (runErrorNoCallStack)

import qualified Pelotero.DB.BoxscoreEntry as Box
import qualified Pelotero.DB.Game          as Game
import qualified Pelotero.DB.LeagueConfig  as LC
import qualified Pelotero.DB.LeagueTeam    as LT
import qualified Pelotero.DB.LineupSlot    as LS
import qualified Pelotero.DB.Player        as P
import qualified Pelotero.DB.Team          as Tm
import           Pelotero.DB.BoxscoreEntry (BattingRow(..), PitchingRow(..))
import           Pelotero.DB.Game          (GameRow(..))
import           Pelotero.DB.LeagueConfig  (LeagueConfigRow(..))
import           Pelotero.DB.LeagueTeam    (LeagueTeamRow(..))
import           Pelotero.DB.LineupSlot    (LineupSlotRow(..))
import           Pelotero.DB.Player        (PlayerRow(..))
import           Pelotero.DB.Pool          (DBError)
import           Pelotero.DB.Team          (TeamRow(..))
import           Pelotero.DB.Provider      (ProviderName(..))
import           Pelotero.Domain.Id
                   ( DbGameId
                   , DbLeagueConfigId(..)
                   , DbPlayerId
                   )
import           Pelotero.Domain.Roster    (LineupLimits(..), RosterLimits(..))
import           Pelotero.Domain.Scoring
                   ( BattingMultipliers(..)
                   , LeagueScoring(..)
                   , PitchingMultipliers(..)
                   , Points(..)
                   )

import           Pelotero.Effects.BoxscoreEntry  (runBoxscoreEntryDB)
import           Pelotero.Effects.Database       (runDatabasePool, runTx)
import           Pelotero.Effects.Games          (runGamesDB)
import           Pelotero.Effects.LeagueConfig   (runLeagueConfigDB)
import           Pelotero.Effects.LeagueTeam     (runLeagueTeamDB)
import           Pelotero.Effects.LineupSlot     (runLineupSlotDB)
import           Pelotero.Effects.LineupSnapshot (runLineupSnapshotDB)
import           Pelotero.Effects.Logging        (runLoggingDiscard)
import qualified Pelotero.Lineup.Snapshot        as Snap
import           Pelotero.Score
                   ( LeagueScore(..)
                   , PlayerScore(..)
                   , TeamScore(..)
                   , scoreLeague
                   )

import           IntegrationTest.Setup
                   ( cleanDatabase
                   , runEffectsOrFail
                   , withTestPool
                   )

spec :: Spec
spec = around withTestPool $
  describe "Pelotero.Score.scoreLeague (end-to-end, via lineup snapshots)" $ do

    it "scores a one-team league with one game's batting and pitching" $ \pool -> do
      cleanDatabase pool

      result <- runEffectsOrFail
              . runEff
              . runErrorNoCallStack @DBError
              . runLoggingDiscard
              . runDatabasePool pool
              . runLeagueConfigDB
              . runLeagueTeamDB
              . runLineupSlotDB
              . runLineupSnapshotDB
              . runGamesDB
              . runBoxscoreEntryDB
              $ do
                  lcid <- runTx (LC.insertLeagueConfigT scoringConfig)

                  ltid <- runTx $ LT.insertLeagueTeamT LeagueTeamRow
                    { ltId             = Nothing
                    , ltLeagueConfigId = lcid
                    , ltTeamKey        = "team-one"
                    , ltName           = "Team One"
                    , ltOwner           = "owner-one"
                    }

                  pidBat <- runTx (P.insertPlayerT (mkPlayer "score-bat"))
                  pidPit <- runTx (P.insertPlayerT (mkPlayer "score-pit"))

                  runTx $ LS.addSlotT (LineupSlotRow ltid "outfield"  pidBat)
                  runTx $ LS.addSlotT (LineupSlotRow ltid "s_pitcher" pidPit)

                  atid <- runTx (Tm.insertTeamT (mkTeam "Score Away" "SCA"))
                  htid <- runTx (Tm.insertTeamT (mkTeam "Score Home" "SCH"))
                  gid  <- runTx $ Game.insertGameT GameRow
                    { gameRowId                 = Nothing
                    , gameRowGameDate           = fromGregorian 2025 4 5
                    , gameRowAwayTeamId         = atid
                    , gameRowHomeTeamId         = htid
                    , gameRowLastSyncedProvider = Just ProviderMLB
                    , gameRowLastSyncedAt       = Nothing
                    }

                  runTx $ Box.upsertBattingT (zeroBatting gid pidBat)
                    { battingTeamId   = Just atid
                    , battingHomeRuns = Just 1
                    , battingRbi      = Just 2
                    , battingRuns     = Just 1
                    }

                  runTx $ Box.upsertPitchingT (zeroPitching gid pidPit)
                    { pitchingTeamId             = Just htid
                    , pitchingInningsPitchedOuts = Just 18
                    , pitchingEarnedRuns         = Just 3
                    }

                  -- Snapshot the team's lineup for this game BEFORE scoring.
                  -- Phase B.3: scoring reads from snapshots, not current
                  -- lineup_slot rows.
                  _ <- Snap.snapshotLineupsForTeam ltid gid

                  scoreLeague lcid

      case result of
        Just ls ->
          case lscTeams ls of
            [team] -> do
              tsTotalPoints team        `shouldBe` Points 26
              length (tsPlayers team)   `shouldBe` 2
              sumPlayerTotals team      `shouldBe` Points 26
            _ -> expectationFailure "expected exactly one team in the league"
        Nothing -> expectationFailure "scoreLeague returned Nothing for an existing league"

    it "returns Nothing for a nonexistent league config id" $ \pool -> do
      cleanDatabase pool
      result <- runEffectsOrFail
              . runEff
              . runErrorNoCallStack @DBError
              . runLoggingDiscard
              . runDatabasePool pool
              . runLeagueConfigDB
              . runLeagueTeamDB
              . runLineupSlotDB
              . runLineupSnapshotDB
              . runGamesDB
              . runBoxscoreEntryDB
              $ scoreLeague (DbLeagueConfigId 999999)
      result `shouldBe` Nothing

    it "scores zero when no games (and no snapshots) exist in the period" $ \pool -> do
      cleanDatabase pool
      result <- runEffectsOrFail
              . runEff
              . runErrorNoCallStack @DBError
              . runLoggingDiscard
              . runDatabasePool pool
              . runLeagueConfigDB
              . runLeagueTeamDB
              . runLineupSlotDB
              . runLineupSnapshotDB
              . runGamesDB
              . runBoxscoreEntryDB
              $ do
                  lcid <- runTx (LC.insertLeagueConfigT scoringConfig)
                  _    <- runTx $ LT.insertLeagueTeamT LeagueTeamRow
                            { ltId             = Nothing
                            , ltLeagueConfigId = lcid
                            , ltTeamKey        = "empty"
                            , ltName           = "Empty Team"
                            , ltOwner          = "nobody"
                            }
                  scoreLeague lcid
      case result of
        Just ls -> case lscTeams ls of
          [team] -> do
            tsTotalPoints team `shouldBe` Points 0
            tsPlayers     team `shouldBe` []
          _ -> expectationFailure "expected exactly one team"
        Nothing -> expectationFailure "scoreLeague returned Nothing"


sumPlayerTotals :: TeamScore -> Points
sumPlayerTotals = foldr addP (Points 0) . tsPlayers
  where
    addP ps acc =
      let Points a = psTotalPoints ps
          Points b = acc
      in Points (a + b)

scoringConfig :: LeagueConfigRow
scoringConfig = LeagueConfigRow
  { lcId            = Nothing
  , lcLeagueId      = "score-it-league"
  , lcCommissioner  = "test-commish"
  , lcStatus        = "active"
  , lcScoring       = standardScoring
  , lcRosterLimits  = RosterLimits Map.empty
  , lcLineupLimits  = LineupLimits Map.empty
  , lcDraftAuto     = False
  , lcDraftStrategy = "serpentine"
  , lcDraftAutoAt   = Nothing
  , lcScoringStart  = utc (fromGregorian 2025 4 1)
  , lcScoringEnd    = utc (fromGregorian 2025 4 30)
  }
  where
    utc d = UTCTime d (secondsToDiffTime 0)

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

mkTeam :: Text -> Text -> TeamRow
mkTeam name abbr =
  TeamRow Nothing name abbr "Anywhere" (Just ProviderMLB) Nothing

mkPlayer :: Text -> PlayerRow
mkPlayer tag = PlayerRow
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

zeroBatting :: DbGameId -> DbPlayerId -> BattingRow
zeroBatting gid pid = BattingRow
  { battingGameId = gid, battingPlayerId = pid, battingTeamId = Nothing
  , battingGamesPlayed = Nothing, battingPlateAppearances = Nothing
  , battingAtBats = Nothing, battingRuns = Nothing, battingHits = Nothing
  , battingDoubles = Nothing, battingTriples = Nothing
  , battingHomeRuns = Nothing, battingRbi = Nothing
  , battingBaseOnBalls = Nothing, battingIntentionalWalks = Nothing
  , battingStrikeOuts = Nothing, battingStolenBases = Nothing
  , battingCaughtStealing = Nothing, battingHitByPitch = Nothing
  , battingSacBunts = Nothing, battingSacFlies = Nothing
  , battingGroundIntoDoublePlay = Nothing
  , battingGroundIntoTriplePlay = Nothing
  , battingLeftOnBase = Nothing, battingTotalBases = Nothing
  , battingFlyOuts = Nothing, battingGroundOuts = Nothing
  , battingCatchersInterference = Nothing, battingPickoffs = Nothing
  }

zeroPitching :: DbGameId -> DbPlayerId -> PitchingRow
zeroPitching gid pid = PitchingRow
  { pitchingGameId = gid, pitchingPlayerId = pid, pitchingTeamId = Nothing
  , pitchingGamesPlayed = Nothing, pitchingGamesStarted = Nothing
  , pitchingGamesFinished = Nothing, pitchingCompleteGames = Nothing
  , pitchingShutouts = Nothing, pitchingWins = Nothing
  , pitchingLosses = Nothing, pitchingSaves = Nothing
  , pitchingSaveOpportunities = Nothing, pitchingHolds = Nothing
  , pitchingBlownSaves = Nothing, pitchingInningsPitchedOuts = Nothing
  , pitchingBattersFaced = Nothing, pitchingNumberOfPitches = Nothing
  , pitchingStrikes = Nothing, pitchingBalls = Nothing
  , pitchingHits = Nothing, pitchingDoubles = Nothing
  , pitchingTriples = Nothing, pitchingHomeRuns = Nothing
  , pitchingRuns = Nothing, pitchingEarnedRuns = Nothing
  , pitchingStrikeOuts = Nothing, pitchingBaseOnBalls = Nothing
  , pitchingIntentionalWalks = Nothing, pitchingHitBatsmen = Nothing
  , pitchingWildPitches = Nothing, pitchingBalks = Nothing
  , pitchingPickoffs = Nothing, pitchingFlyOuts = Nothing
  , pitchingGroundOuts = Nothing, pitchingAirOuts = Nothing
  , pitchingInheritedRunners = Nothing
  , pitchingInheritedRunnersScored = Nothing
  , pitchingStolenBases = Nothing, pitchingCaughtStealing = Nothing
  , pitchingAtBats = Nothing, pitchingRbi = Nothing
  , pitchingSacBunts = Nothing, pitchingSacFlies = Nothing
  , pitchingCatchersInterference = Nothing, pitchingPassedBall = Nothing
  }