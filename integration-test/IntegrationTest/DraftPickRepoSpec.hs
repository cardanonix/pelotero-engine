module IntegrationTest.DraftPickRepoSpec (spec) where

import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import           Data.Time                (UTCTime(..), fromGregorian, secondsToDiffTime)
import           Test.Hspec

import qualified Hasql.Transaction        as Tx

import qualified Pelotero.DB.DraftPick    as DP
import qualified Pelotero.DB.LeagueConfig as LC
import qualified Pelotero.DB.LeagueTeam   as LT
import qualified Pelotero.DB.Player       as P
import           Pelotero.DB.DraftPick    (DraftPickRow(..))
import           Pelotero.DB.LeagueConfig (LeagueConfigRow(..))
import           Pelotero.DB.LeagueTeam   (LeagueTeamRow(..))
import           Pelotero.DB.Player       (PlayerRow(..))
import           Pelotero.DB.Provider     (ProviderName(..))
import           Pelotero.Domain.Id
  (DbLeagueConfigId, DbLeagueTeamId, DbPlayerId)
import           Pelotero.Domain.Roster   (LineupLimits(..), RosterLimits(..))
import           Pelotero.Domain.Scoring
  ( BattingMultipliers(..), LeagueScoring(..), PitchingMultipliers(..) )

import IntegrationTest.Setup

spec :: Spec
spec = around withTestPool $
  describe "Pelotero.DB.DraftPick" $ do

    it "round-trips a recorded pick" $ \pool -> do
      picks <- runRolledBack pool $ do
        (lcid, ltid, pid) <- mkContext "dp-rt"
        _ <- DP.recordPickT (mkPick lcid ltid pid 1)
        DP.getPicksForLeagueT lcid
      length picks               `shouldBe` 1
      map dpPickNumber picks     `shouldBe` [1]

    it "getPicksForLeague orders by pick_number" $ \pool -> do
      picks <- runRolledBack pool $ do
        (lcid, ltid, _) <- mkContext "dp-ord"
        pidA <- mkPlayer "dp-ord-a"
        pidB <- mkPlayer "dp-ord-b"
        pidC <- mkPlayer "dp-ord-c"
        _ <- DP.recordPickT (mkPick lcid ltid pidB 2)
        _ <- DP.recordPickT (mkPick lcid ltid pidC 3)
        _ <- DP.recordPickT (mkPick lcid ltid pidA 1)
        DP.getPicksForLeagueT lcid
      map dpPickNumber picks `shouldBe` [1, 2, 3]

    it "getPickCount returns the correct count, including zero" $ \pool -> do
      (nWith, nEmpty) <- runRolledBack pool $ do
        (lcid1, ltid, _) <- mkContext "dp-ct-1"
        lcid2 <- mkLeagueConfig "dp-ct-2"
        pidA <- mkPlayer "dp-ct-a"
        pidB <- mkPlayer "dp-ct-b"
        _ <- DP.recordPickT (mkPick lcid1 ltid pidA 1)
        _ <- DP.recordPickT (mkPick lcid1 ltid pidB 2)
        a <- DP.getPickCountT lcid1
        b <- DP.getPickCountT lcid2
        pure (a, b)
      nWith  `shouldBe` 2
      nEmpty `shouldBe` 0

    it "picks do not bleed across leagues" $ \pool -> do
      (n1, n2) <- runRolledBack pool $ do
        (lcid1, ltid1, _) <- mkContext "dp-iso-1"
        (lcid2, ltid2, _) <- mkContext "dp-iso-2"
        pidA <- mkPlayer "dp-iso-a"
        pidB <- mkPlayer "dp-iso-b"
        pidC <- mkPlayer "dp-iso-c"
        _ <- DP.recordPickT (mkPick lcid1 ltid1 pidA 1)
        _ <- DP.recordPickT (mkPick lcid1 ltid1 pidB 2)
        _ <- DP.recordPickT (mkPick lcid2 ltid2 pidC 1)
        a <- DP.getPicksForLeagueT lcid1
        b <- DP.getPicksForLeagueT lcid2
        pure (length a, length b)
      n1 `shouldBe` 2
      n2 `shouldBe` 1

mkContext :: Text -> Tx.Transaction (DbLeagueConfigId, DbLeagueTeamId, DbPlayerId)
mkContext tag = do
  lcid <- mkLeagueConfig tag
  ltid <- LT.insertLeagueTeamT $ LeagueTeamRow
    { ltId             = Nothing
    , ltLeagueConfigId = lcid
    , ltTeamKey        = tag <> "-key"
    , ltName           = tag <> "-name"
    , ltOwner          = tag <> "-owner"
    }
  pid <- mkPlayer (tag <> "-player")
  pure (lcid, ltid, pid)

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

mkPick :: DbLeagueConfigId -> DbLeagueTeamId -> DbPlayerId -> Int -> DraftPickRow
mkPick lcid ltid pid n = DraftPickRow
  { dpId             = Nothing
  , dpLeagueConfigId = lcid
  , dpPickNumber     = fromIntegral n
  , dpLeagueTeamId   = ltid
  , dpPlayerId       = pid
  , dpPickedAt       = Nothing
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