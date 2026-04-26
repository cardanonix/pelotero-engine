-- | Round-trip tests for "Pelotero.DB.BoxscoreEntry".
module IntegrationTest.BoxscoreEntryRepoSpec (spec) where

import Data.Time (fromGregorian)
import Test.Hspec

import qualified Pelotero.DB.BoxscoreEntry as Box
import qualified Pelotero.DB.Game          as Game
import qualified Pelotero.DB.Player        as Player
import qualified Pelotero.DB.Team          as Team
import           Pelotero.DB.BoxscoreEntry (BattingRow(..), PitchingRow(..))
import           Pelotero.DB.Game          (GameRow(..))
import           Pelotero.DB.Player        (PlayerRow(..))
import           Pelotero.DB.Team          (TeamRow(..))
import           Pelotero.DB.Provider      (ProviderName(..))

import IntegrationTest.Setup

spec :: Spec
spec = around withTestPool $
  describe "Pelotero.DB.BoxscoreEntry" $ do

    it "round-trips a batting row" $ \pool -> do
      result <- runRolledBack pool $ do
        atid <- Team.insertTeamT (mkTeam "Box Away" "BXA")
        htid <- Team.insertTeamT (mkTeam "Box Home" "BXH")
        gid  <- Game.insertGameT GameRow
          { gameRowId = Nothing
          , gameRowGameDate = fromGregorian 2025 4 1
          , gameRowAwayTeamId = atid
          , gameRowHomeTeamId = htid
          , gameRowLastSyncedProvider = Just ProviderMLB
          , gameRowLastSyncedAt = Nothing
          }
        pid <- Player.insertPlayerT (mkPlayer "Box" "Batter")
        let batting = (zeroBatting gid pid)
              { battingTeamId    = Just atid
              , battingAtBats    = Just 4
              , battingHits      = Just 2
              , battingHomeRuns  = Just 1
              , battingRbi       = Just 3
              }
        Box.upsertBattingT batting
        Box.getBattingForGameT gid
      case result of
        [b] -> do
          battingHits     b `shouldBe` Just 2
          battingHomeRuns b `shouldBe` Just 1
          battingRbi      b `shouldBe` Just 3
        _ -> expectationFailure $ "expected exactly one batting row; got " ++ show (length result)

    it "round-trips a pitching row with innings stored as outs" $ \pool -> do
      result <- runRolledBack pool $ do
        atid <- Team.insertTeamT (mkTeam "Pitch Away" "PTA")
        htid <- Team.insertTeamT (mkTeam "Pitch Home" "PTH")
        gid  <- Game.insertGameT GameRow
          { gameRowId = Nothing
          , gameRowGameDate = fromGregorian 2025 4 2
          , gameRowAwayTeamId = atid
          , gameRowHomeTeamId = htid
          , gameRowLastSyncedProvider = Just ProviderMLB
          , gameRowLastSyncedAt = Nothing
          }
        pid <- Player.insertPlayerT (mkPlayer "Pitch" "Pitcher")
        -- 6 2/3 innings = 20 outs
        let pitching = (zeroPitching gid pid)
              { pitchingTeamId             = Just htid
              , pitchingInningsPitchedOuts = Just 20
              , pitchingStrikeOuts         = Just 8
              , pitchingEarnedRuns         = Just 2
              }
        Box.upsertPitchingT pitching
        Box.getPitchingForGameT gid
      case result of
        [p] -> do
          pitchingInningsPitchedOuts p `shouldBe` Just 20
          pitchingStrikeOuts         p `shouldBe` Just 8
          pitchingEarnedRuns         p `shouldBe` Just 2
        _ -> expectationFailure $ "expected exactly one pitching row; got " ++ show (length result)

    it "upsert overwrites existing batting on (game_id, player_id) conflict" $ \pool -> do
      result <- runRolledBack pool $ do
        atid <- Team.insertTeamT (mkTeam "Up Away" "UPA")
        htid <- Team.insertTeamT (mkTeam "Up Home" "UPH")
        gid  <- Game.insertGameT GameRow
          { gameRowId = Nothing
          , gameRowGameDate = fromGregorian 2025 4 3
          , gameRowAwayTeamId = atid
          , gameRowHomeTeamId = htid
          , gameRowLastSyncedProvider = Just ProviderMLB
          , gameRowLastSyncedAt = Nothing
          }
        pid <- Player.insertPlayerT (mkPlayer "Twice" "Inserted")
        Box.upsertBattingT (zeroBatting gid pid) { battingAtBats = Just 1 }  -- ignored, see below
        let initial = (zeroBatting gid pid) { battingTeamId = Just atid, battingHits = Just 1 }
            updated = (zeroBatting gid pid) { battingTeamId = Just atid, battingHits = Just 5 }
        Box.upsertBattingT initial
        Box.upsertBattingT updated
        Box.getBattingForGameT gid
      case result of
        [b] -> battingHits b `shouldBe` Just 5
        _ -> expectationFailure "expected one batting row after two upserts"

    it "deleteBattingForGameT removes only that game's rows" $ \pool -> do
      result <- runRolledBack pool $ do
        atid <- Team.insertTeamT (mkTeam "Del Away" "DLA")
        htid <- Team.insertTeamT (mkTeam "Del Home" "DLH")
        gid1 <- Game.insertGameT GameRow
          { gameRowId = Nothing
          , gameRowGameDate = fromGregorian 2025 4 4
          , gameRowAwayTeamId = atid
          , gameRowHomeTeamId = htid
          , gameRowLastSyncedProvider = Just ProviderMLB
          , gameRowLastSyncedAt = Nothing
          }
        gid2 <- Game.insertGameT GameRow
          { gameRowId = Nothing
          , gameRowGameDate = fromGregorian 2025 4 5
          , gameRowAwayTeamId = atid
          , gameRowHomeTeamId = htid
          , gameRowLastSyncedProvider = Just ProviderMLB
          , gameRowLastSyncedAt = Nothing
          }
        pid <- Player.insertPlayerT (mkPlayer "Cross" "Game")
        Box.upsertBattingT (zeroBatting gid1 pid) { battingHits = Just 2 }
        Box.upsertBattingT (zeroBatting gid2 pid) { battingHits = Just 3 }
        Box.deleteBattingForGameT gid1
        rows1 <- Box.getBattingForGameT gid1
        rows2 <- Box.getBattingForGameT gid2
        pure (length rows1, length rows2)
      result `shouldBe` (0, 1)

  where
    mkTeam name abbr = TeamRow Nothing name abbr "Anywhere"
                               (Just ProviderMLB) Nothing

    mkPlayer first last_ = PlayerRow
      { playerRowId = Nothing
      , playerRowFirstName = first
      , playerRowLastName  = last_
      , playerRowNameSlug  = first <> "-" <> last_
      , playerRowPosition  = Nothing
      , playerRowBatSide   = Nothing
      , playerRowPitchHand = Nothing
      , playerRowActive    = True
      , playerRowCurrentTeamId = Nothing
      , playerRowLastSyncedProvider = Just ProviderMLB
      , playerRowLastSyncedAt = Nothing
      }

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