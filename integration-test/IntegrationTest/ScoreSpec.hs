{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

module IntegrationTest.ScoreSpec (spec) where

import qualified Data.Map.Strict          as Map
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
import           Pelotero.DB.LeagueConfig  (LeagueConfigRow(..))
import           Pelotero.DB.LeagueTeam    (LeagueTeamRow(..))
import           Pelotero.DB.LineupSlot    (LineupSlotRow(..))
import           Pelotero.DB.Pool          (DBError, Pool)
import           Pelotero.Domain.Id        (DbLeagueConfigId(..))
import           Pelotero.Domain.Roster    (LineupLimits(..), RosterLimits(..))
import           Pelotero.Domain.Scoring   (Points(..))

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

import           IntegrationTest.Fixtures
                   ( mkGameRow
                   , mkLeagueConfigRow
                   , mkPlayerRow
                   , mkTeamRow
                   , standardScoring
                   , zeroBatting
                   , zeroPitching
                   )
import           IntegrationTest.Setup
                   ( cleanDatabase
                   , runEffectsOrFail
                   )

spec :: SpecWith Pool
spec = describe "Pelotero.Score.scoreLeague (end-to-end, via lineup snapshots)" $ do

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
                  , ltOwner          = "owner-one"
                  }

                pidBat <- runTx (P.insertPlayerT (mkPlayerRow "score-bat"))
                pidPit <- runTx (P.insertPlayerT (mkPlayerRow "score-pit"))

                runTx $ LS.addSlotT (LineupSlotRow ltid "outfield"  pidBat)
                runTx $ LS.addSlotT (LineupSlotRow ltid "s_pitcher" pidPit)

                atid <- runTx (Tm.insertTeamT (mkTeamRow "Score Away" "SCA"))
                htid <- runTx (Tm.insertTeamT (mkTeamRow "Score Home" "SCH"))
                gid  <- runTx $ Game.insertGameT
                          (mkGameRow (fromGregorian 2025 4 5) atid htid)

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
scoringConfig = (mkLeagueConfigRow "score-it")
  { lcStatus       = "active"
  , lcScoring      = standardScoring
  , lcScoringStart = utc (fromGregorian 2025 4 1)
  , lcScoringEnd   = utc (fromGregorian 2025 4 30)
  }
  where
    utc d = UTCTime d (secondsToDiffTime 0)