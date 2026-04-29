module IntegrationTest.LineupSlotRepoSpec (spec) where

import           Data.List                (sort)
import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import           Data.Time                (UTCTime(..), fromGregorian, secondsToDiffTime)
import           Test.Hspec

import qualified Hasql.Transaction        as Tx

import qualified Pelotero.DB.LeagueConfig as LC
import qualified Pelotero.DB.LeagueTeam   as LT
import qualified Pelotero.DB.LineupSlot   as LS
import qualified Pelotero.DB.Player       as P
import           Pelotero.DB.LeagueConfig (LeagueConfigRow(..))
import           Pelotero.DB.LeagueTeam   (LeagueTeamRow(..))
import           Pelotero.DB.LineupSlot   (LineupSlotRow(..))
import           Pelotero.DB.Player       (PlayerRow(..))
import           Pelotero.DB.Provider     (ProviderName(..))
import           Pelotero.Domain.Id       (DbLeagueTeamId, DbPlayerId)
import           Pelotero.Domain.Roster   (LineupLimits(..), RosterLimits(..))
import           Pelotero.Domain.Scoring
  ( BattingMultipliers(..), LeagueScoring(..), PitchingMultipliers(..) )

import IntegrationTest.Setup

spec :: Spec
spec = around withTestPool $
  describe "Pelotero.DB.LineupSlot" $ do

    it "round-trips an inserted slot" $ \pool -> do
      rows <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "ls-rt"
        pid  <- mkPlayer     "ls-rt-player"
        LS.addSlotT (LineupSlotRow ltid "catcher" pid)
        LS.getSlotsForTeamT ltid
      length rows                        `shouldBe` 1
      map lsSlot rows                    `shouldBe` ["catcher"]

    it "addSlot is an upsert: re-adding the same player at a new slot moves them" $ \pool -> do
      rows <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "ls-up"
        pid  <- mkPlayer     "ls-up-player"
        LS.addSlotT (LineupSlotRow ltid "catcher" pid)
        LS.addSlotT (LineupSlotRow ltid "first"   pid)
        LS.getSlotsForTeamT ltid
      length rows                        `shouldBe` 1
      map lsSlot rows                    `shouldBe` ["first"]

    it "removeSlot removes the named player and leaves others" $ \pool -> do
      (rows, expectedSurvivor) <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "ls-rm"
        pidA <- mkPlayer     "ls-rm-a"
        pidB <- mkPlayer     "ls-rm-b"
        LS.addSlotT (LineupSlotRow ltid "outfield" pidA)
        LS.addSlotT (LineupSlotRow ltid "outfield" pidB)
        LS.removeSlotT ltid pidA
        rs <- LS.getSlotsForTeamT ltid
        pure (rs, pidB)
      length rows                        `shouldBe` 1
      map lsPlayerId rows                `shouldBe` [expectedSurvivor]

    it "clearTeamLineup removes only the target team's rows" $ \pool -> do
      (n1, n2) <- runRolledBack pool $ do
        ltid1 <- mkLeagueTeam "ls-cl-1"
        ltid2 <- mkLeagueTeam "ls-cl-2"
        pid   <- mkPlayer     "ls-cl-player"
        LS.addSlotT (LineupSlotRow ltid1 "catcher" pid)
        LS.addSlotT (LineupSlotRow ltid2 "catcher" pid)
        LS.clearTeamLineupT ltid1
        rs1 <- LS.getSlotsForTeamT ltid1
        rs2 <- LS.getSlotsForTeamT ltid2
        pure (length rs1, length rs2)
      n1                                 `shouldBe` 0
      n2                                 `shouldBe` 1

    it "replaceTeamLineup atomically replaces the full set" $ \pool -> do
      slots <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "ls-rep"
        pidA <- mkPlayer     "ls-rep-a"
        pidB <- mkPlayer     "ls-rep-b"
        pidC <- mkPlayer     "ls-rep-c"
        LS.addSlotT (LineupSlotRow ltid "catcher" pidA)
        LS.addSlotT (LineupSlotRow ltid "first"   pidB)
        LS.replaceTeamLineupT ltid
          [ LineupSlotRow ltid "shortstop" pidC
          , LineupSlotRow ltid "outfield"  pidA
          ]
        map lsSlot <$> LS.getSlotsForTeamT ltid
      sort slots                         `shouldBe` ["outfield", "shortstop"]

mkLeagueTeam :: Text -> Tx.Transaction DbLeagueTeamId
mkLeagueTeam tag = do
  lcid <- LC.insertLeagueConfigT $ LeagueConfigRow
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
  LT.insertLeagueTeamT $ LeagueTeamRow
    { ltId             = Nothing
    , ltLeagueConfigId = lcid
    , ltTeamKey        = tag <> "-key"
    , ltName           = tag <> "-name"
    , ltOwner          = tag <> "-owner"
    }
  where
    epoch = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

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