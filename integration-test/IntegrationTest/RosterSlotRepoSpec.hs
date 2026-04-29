module IntegrationTest.RosterSlotRepoSpec (spec) where

import           Data.List                (sort)
import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import           Data.Time                (UTCTime(..), fromGregorian, secondsToDiffTime)
import           Test.Hspec

import qualified Hasql.Transaction        as Tx

import qualified Pelotero.DB.LeagueConfig as LC
import qualified Pelotero.DB.LeagueTeam   as LT
import qualified Pelotero.DB.Player       as P
import qualified Pelotero.DB.RosterSlot   as RS
import           Pelotero.DB.LeagueConfig (LeagueConfigRow(..))
import           Pelotero.DB.LeagueTeam   (LeagueTeamRow(..))
import           Pelotero.DB.Player       (PlayerRow(..))
import           Pelotero.DB.Provider     (ProviderName(..))
import           Pelotero.DB.RosterSlot   (RosterSlotRow(..))
import           Pelotero.Domain.Id       (DbLeagueTeamId, DbPlayerId)
import           Pelotero.Domain.Roster   (LineupLimits(..), RosterLimits(..))
import           Pelotero.Domain.Scoring
  ( BattingMultipliers(..), LeagueScoring(..), PitchingMultipliers(..) )

import IntegrationTest.Setup

spec :: Spec
spec = around withTestPool $
  describe "Pelotero.DB.RosterSlot" $ do

    it "round-trips an inserted slot" $ \pool -> do
      rows <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "rs-rt"
        pid  <- mkPlayer     "rs-rt-player"
        RS.addSlotT (RosterSlotRow ltid "catcher" pid)
        RS.getSlotsForTeamT ltid
      length rows                        `shouldBe` 1
      map rsSlot rows                    `shouldBe` ["catcher"]

    it "addSlot is an upsert: re-adding the same player at a new slot moves them" $ \pool -> do
      rows <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "rs-up"
        pid  <- mkPlayer     "rs-up-player"
        RS.addSlotT (RosterSlotRow ltid "catcher" pid)
        RS.addSlotT (RosterSlotRow ltid "first"   pid)
        RS.getSlotsForTeamT ltid
      length rows                        `shouldBe` 1
      map rsSlot rows                    `shouldBe` ["first"]

    it "removeSlot removes the named player and leaves others" $ \pool -> do
      (rows, expectedSurvivor) <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "rs-rm"
        pidA <- mkPlayer     "rs-rm-a"
        pidB <- mkPlayer     "rs-rm-b"
        RS.addSlotT (RosterSlotRow ltid "outfield" pidA)
        RS.addSlotT (RosterSlotRow ltid "outfield" pidB)
        RS.removeSlotT ltid pidA
        rs <- RS.getSlotsForTeamT ltid
        pure (rs, pidB)
      length rows                        `shouldBe` 1
      map rsPlayerId rows                `shouldBe` [expectedSurvivor]

    it "clearTeamRoster removes only the target team's rows" $ \pool -> do
      (n1, n2) <- runRolledBack pool $ do
        ltid1 <- mkLeagueTeam "rs-cl-1"
        ltid2 <- mkLeagueTeam "rs-cl-2"
        pid   <- mkPlayer     "rs-cl-player"
        RS.addSlotT (RosterSlotRow ltid1 "catcher" pid)
        RS.addSlotT (RosterSlotRow ltid2 "catcher" pid)
        RS.clearTeamRosterT ltid1
        rs1 <- RS.getSlotsForTeamT ltid1
        rs2 <- RS.getSlotsForTeamT ltid2
        pure (length rs1, length rs2)
      n1                                 `shouldBe` 0
      n2                                 `shouldBe` 1

    it "replaceTeamRoster atomically replaces the full set" $ \pool -> do
      slots <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "rs-rep"
        pidA <- mkPlayer     "rs-rep-a"
        pidB <- mkPlayer     "rs-rep-b"
        pidC <- mkPlayer     "rs-rep-c"
        RS.addSlotT (RosterSlotRow ltid "catcher" pidA)
        RS.addSlotT (RosterSlotRow ltid "first"   pidB)
        RS.replaceTeamRosterT ltid
          [ RosterSlotRow ltid "shortstop" pidC
          , RosterSlotRow ltid "outfield"  pidA
          ]
        map rsSlot <$> RS.getSlotsForTeamT ltid
      sort slots                         `shouldBe` ["outfield", "shortstop"]

    it "countBySlot returns the right count, including zero" $ \pool -> do
      (nOutfield, nCatcher, nMissing) <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "rs-ct"
        pidA <- mkPlayer     "rs-ct-a"
        pidB <- mkPlayer     "rs-ct-b"
        pidC <- mkPlayer     "rs-ct-c"
        RS.addSlotT (RosterSlotRow ltid "outfield" pidA)
        RS.addSlotT (RosterSlotRow ltid "outfield" pidB)
        RS.addSlotT (RosterSlotRow ltid "catcher"  pidC)
        a <- RS.countBySlotT ltid "outfield"
        b <- RS.countBySlotT ltid "catcher"
        c <- RS.countBySlotT ltid "third"
        pure (a, b, c)
      nOutfield                          `shouldBe` 2
      nCatcher                           `shouldBe` 1
      nMissing                           `shouldBe` 0

-- ----------------------------------------------------------------------------
-- Fixtures.
-- Each call constructs a fresh league_config + league_team or player row
-- and returns its id. All cleanup happens via runRolledBack's tx.condemn.
-- ----------------------------------------------------------------------------

mkLeagueTeam :: Text -> Tx.Transaction DbLeagueTeamId
mkLeagueTeam tag = do
  lcid <- LC.insertLeagueConfigT (mkLeagueConfig tag)
  LT.insertLeagueTeamT $ LeagueTeamRow
    { ltId             = Nothing
    , ltLeagueConfigId = lcid
    , ltTeamKey        = tag <> "-key"
    , ltName           = tag <> "-name"
    , ltOwner          = tag <> "-owner"
    }

mkPlayer :: Text -> Tx.Transaction DbPlayerId
mkPlayer tag = P.insertPlayerT $ PlayerRow
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

mkLeagueConfig :: Text -> LeagueConfigRow
mkLeagueConfig tag = LeagueConfigRow
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