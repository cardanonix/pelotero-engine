module IntegrationTest.PlayerRankingRepoSpec (spec) where

import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime(..), fromGregorian, secondsToDiffTime)
import           Test.Hspec

import qualified Hasql.Transaction          as Tx

import qualified Pelotero.DB.LeagueConfig   as LC
import qualified Pelotero.DB.LeagueTeam     as LT
import qualified Pelotero.DB.Player         as P
import qualified Pelotero.DB.PlayerRanking  as PR
import           Pelotero.DB.LeagueConfig   (LeagueConfigRow(..))
import           Pelotero.DB.LeagueTeam     (LeagueTeamRow(..))
import           Pelotero.DB.Player         (PlayerRow(..))
import           Pelotero.DB.PlayerRanking  (PlayerRankingRow(..))
import           Pelotero.DB.Provider       (ProviderName(..))
import           Pelotero.Domain.Id         (DbLeagueTeamId, DbPlayerId)
import           Pelotero.Domain.Roster     (LineupLimits(..), RosterLimits(..))
import           Pelotero.Domain.Scoring
  ( BattingMultipliers(..), LeagueScoring(..), PitchingMultipliers(..) )

import IntegrationTest.Setup

spec :: Spec
spec = around withTestPool $
  describe "Pelotero.DB.PlayerRanking" $ do

    it "replaceRankings + getRankingsForTeam round-trips and orders by rank_slot" $ \pool -> do
      rows <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "pr-rt"
        pidA <- mkPlayer     "pr-rt-a"
        pidB <- mkPlayer     "pr-rt-b"
        pidC <- mkPlayer     "pr-rt-c"
        PR.replaceRankingsT ltid
          [ PlayerRankingRow ltid pidB 2
          , PlayerRankingRow ltid pidA 1
          , PlayerRankingRow ltid pidC 3
          ]
        PR.getRankingsForTeamT ltid
      map prRankSlot rows `shouldBe` [1, 2, 3]

    it "replaceRankings is destructive: the new set replaces the old set entirely" $ \pool -> do
      rows <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "pr-rep"
        pidA <- mkPlayer     "pr-rep-a"
        pidB <- mkPlayer     "pr-rep-b"
        pidC <- mkPlayer     "pr-rep-c"
        PR.replaceRankingsT ltid
          [ PlayerRankingRow ltid pidA 1
          , PlayerRankingRow ltid pidB 2
          ]
        PR.replaceRankingsT ltid
          [ PlayerRankingRow ltid pidC 1
          ]
        PR.getRankingsForTeamT ltid
      length rows         `shouldBe` 1
      map prRankSlot rows `shouldBe` [1]

    it "clearRankings empties the team's rankings" $ \pool -> do
      n <- runRolledBack pool $ do
        ltid <- mkLeagueTeam "pr-clr"
        pid  <- mkPlayer     "pr-clr-a"
        PR.replaceRankingsT ltid [ PlayerRankingRow ltid pid 1 ]
        PR.clearRankingsT ltid
        rs <- PR.getRankingsForTeamT ltid
        pure (length rs)
      n `shouldBe` 0

    it "getRankingCount returns the correct count" $ \pool -> do
      (nWithRows, nEmpty) <- runRolledBack pool $ do
        ltid1 <- mkLeagueTeam "pr-ct-1"
        ltid2 <- mkLeagueTeam "pr-ct-2"
        pidA  <- mkPlayer     "pr-ct-a"
        pidB  <- mkPlayer     "pr-ct-b"
        PR.replaceRankingsT ltid1
          [ PlayerRankingRow ltid1 pidA 1
          , PlayerRankingRow ltid1 pidB 2
          ]
        a <- PR.getRankingCountT ltid1
        b <- PR.getRankingCountT ltid2
        pure (a, b)
      nWithRows `shouldBe` 2
      nEmpty    `shouldBe` 0

    it "rankings do not bleed across teams" $ \pool -> do
      (n1, n2) <- runRolledBack pool $ do
        ltid1 <- mkLeagueTeam "pr-iso-1"
        ltid2 <- mkLeagueTeam "pr-iso-2"
        pidA  <- mkPlayer     "pr-iso-a"
        pidB  <- mkPlayer     "pr-iso-b"
        pidC  <- mkPlayer     "pr-iso-c"
        PR.replaceRankingsT ltid1
          [ PlayerRankingRow ltid1 pidA 1
          , PlayerRankingRow ltid1 pidB 2
          ]
        PR.replaceRankingsT ltid2
          [ PlayerRankingRow ltid2 pidC 1
          ]
        a <- PR.getRankingsForTeamT ltid1
        b <- PR.getRankingsForTeamT ltid2
        pure (length a, length b)
      n1 `shouldBe` 2
      n2 `shouldBe` 1

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