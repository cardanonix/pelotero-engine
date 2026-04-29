module IntegrationTest.LeagueTeamRepoSpec (spec) where

import           Data.List                (sort)
import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import           Data.Time                (UTCTime(..), fromGregorian, secondsToDiffTime)
import           Test.Hspec

import qualified Hasql.Transaction        as Tx

import qualified Pelotero.DB.LeagueConfig as LC
import qualified Pelotero.DB.LeagueTeam   as LT
import           Pelotero.DB.LeagueConfig (LeagueConfigRow(..))
import           Pelotero.DB.LeagueTeam   (LeagueTeamRow(..))
import           Pelotero.Domain.Id       (DbLeagueConfigId)
import           Pelotero.Domain.Roster   (LineupLimits(..), RosterLimits(..))
import           Pelotero.Domain.Scoring
  ( BattingMultipliers(..), LeagueScoring(..), PitchingMultipliers(..) )

import IntegrationTest.Setup

spec :: Spec
spec = around withTestPool $
  describe "Pelotero.DB.LeagueTeam" $ do

    it "round-trips an inserted league team" $ \pool -> do
      mGot <- runRolledBack pool $ do
        lcid <- mkLeagueConfig "lt-rt"
        ltid <- LT.insertLeagueTeamT (mkTeam lcid "alpha")
        LT.getByIdT ltid
      case mGot of
        Just got -> do
          ltTeamKey got `shouldBe` "alpha-key"
          ltName got    `shouldBe` "alpha-name"
          ltOwner got   `shouldBe` "alpha-owner"
        Nothing -> expectationFailure "round-trip read returned Nothing"

    it "lookupByKey finds a team by (league_config_id, team_key)" $ \pool -> do
      mGot <- runRolledBack pool $ do
        lcid <- mkLeagueConfig "lt-key"
        _    <- LT.insertLeagueTeamT (mkTeam lcid "beta")
        LT.lookupByKeyT lcid "beta-key"
      case mGot of
        Just got -> ltTeamKey got `shouldBe` "beta-key"
        Nothing  -> expectationFailure "expected to find by key"

    it "lookupByKey returns Nothing for an unknown key" $ \pool -> do
      mGot <- runRolledBack pool $ do
        lcid <- mkLeagueConfig "lt-mk"
        LT.lookupByKeyT lcid "no-such-key"
      mGot `shouldBe` Nothing

    it "getForLeague returns all teams for the given league" $ \pool -> do
      names <- runRolledBack pool $ do
        lcid <- mkLeagueConfig "lt-many"
        _    <- LT.insertLeagueTeamT (mkTeam lcid "alpha")
        _    <- LT.insertLeagueTeamT (mkTeam lcid "beta")
        _    <- LT.insertLeagueTeamT (mkTeam lcid "gamma")
        rows <- LT.getForLeagueT lcid
        pure (sort (map ltName rows))
      names `shouldBe` ["alpha-name", "beta-name", "gamma-name"]

    it "getForLeague does not bleed across leagues" $ \pool -> do
      counts <- runRolledBack pool $ do
        lcid1 <- mkLeagueConfig "lt-iso-1"
        lcid2 <- mkLeagueConfig "lt-iso-2"
        _ <- LT.insertLeagueTeamT (mkTeam lcid1 "alpha")
        _ <- LT.insertLeagueTeamT (mkTeam lcid1 "beta")
        _ <- LT.insertLeagueTeamT (mkTeam lcid2 "gamma")
        a <- LT.getForLeagueT lcid1
        b <- LT.getForLeagueT lcid2
        pure (length a, length b)
      counts `shouldBe` (2, 1)

    it "updateLeagueTeam overwrites the row" $ \pool -> do
      mGot <- runRolledBack pool $ do
        lcid <- mkLeagueConfig "lt-upd"
        ltid <- LT.insertLeagueTeamT (mkTeam lcid "delta")
        let updated = (mkTeam lcid "delta") { ltName = "renamed", ltOwner = "new-owner" }
        LT.updateLeagueTeamT ltid updated
        LT.getByIdT ltid
      case mGot of
        Just got -> do
          ltName got  `shouldBe` "renamed"
          ltOwner got `shouldBe` "new-owner"
        Nothing -> expectationFailure "expected to find updated team"

    it "deleteT removes the named team" $ \pool -> do
      result <- runRolledBack pool $ do
        lcid <- mkLeagueConfig "lt-del"
        ltid <- LT.insertLeagueTeamT (mkTeam lcid "doomed")
        LT.deleteT ltid
        LT.getByIdT ltid
      result `shouldBe` Nothing

mkLeagueConfig :: Text -> Tx.Transaction DbLeagueConfigId
mkLeagueConfig tag = LC.insertLeagueConfigT $ LeagueConfigRow
  { lcId            = Nothing
  , lcLeagueId      = tag <> "-league"
  , lcCommissioner  = "test-commish"
  , lcStatus        = "draft"
  , lcScoring       = zeroScoring
  , lcRosterLimits  = RosterLimits Map.empty
  , lcLineupLimits  = LineupLimits Map.empty
  , lcDraftAuto     = False
  , lcDraftStrategy = "serpentine"
  , lcDraftAutoAt   = Nothing
  , lcScoringStart  = epoch
  , lcScoringEnd    = epoch
  }
  where
    epoch = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

mkTeam :: DbLeagueConfigId -> Text -> LeagueTeamRow
mkTeam lcid tag = LeagueTeamRow
  { ltId             = Nothing
  , ltLeagueConfigId = lcid
  , ltTeamKey        = tag <> "-key"
  , ltName           = tag <> "-name"
  , ltOwner          = tag <> "-owner"
  }

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