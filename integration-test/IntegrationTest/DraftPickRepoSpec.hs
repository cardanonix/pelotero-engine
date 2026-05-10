{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest.DraftPickRepoSpec (spec) where

import           Data.Text                (Text)
import           Test.Hspec

import qualified Hasql.Transaction        as Tx

import qualified Pelotero.DB.DraftPick    as DP
import           Pelotero.DB.DraftPick    (DraftPickRow(..))
import           Pelotero.DB.Pool         (Pool)
import           Pelotero.Domain.Id
                   ( DbLeagueConfigId
                   , DbLeagueTeamId
                   , DbPlayerId
                   )

import           IntegrationTest.Fixtures
                   ( addLeagueConfigT
                   , addLeagueTeamWithConfigT
                   , addPlayerT
                   )
import           IntegrationTest.Setup    (runRolledBack)

spec :: SpecWith Pool
spec = describe "Pelotero.DB.DraftPick" $ do

  it "round-trips a recorded pick" $ \pool -> do
    picks <- runRolledBack pool $ do
      (lcid, ltid, pid) <- mkContext "dp-rt"
      _ <- DP.recordPickT (mkPick lcid ltid pid 1)
      DP.getPicksForLeagueT lcid
    length picks           `shouldBe` 1
    map dpPickNumber picks `shouldBe` [1]

  it "getPicksForLeague orders by pick_number" $ \pool -> do
    picks <- runRolledBack pool $ do
      (lcid, ltid, _) <- mkContext "dp-ord"
      pidA <- addPlayerT "dp-ord-a"
      pidB <- addPlayerT "dp-ord-b"
      pidC <- addPlayerT "dp-ord-c"
      _ <- DP.recordPickT (mkPick lcid ltid pidB 2)
      _ <- DP.recordPickT (mkPick lcid ltid pidC 3)
      _ <- DP.recordPickT (mkPick lcid ltid pidA 1)
      DP.getPicksForLeagueT lcid
    map dpPickNumber picks `shouldBe` [1, 2, 3]

  it "getPickCount returns the correct count, including zero" $ \pool -> do
    (nWith, nEmpty) <- runRolledBack pool $ do
      (lcid1, ltid, _) <- mkContext "dp-ct-1"
      lcid2 <- addLeagueConfigT "dp-ct-2"
      pidA <- addPlayerT "dp-ct-a"
      pidB <- addPlayerT "dp-ct-b"
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
      pidA <- addPlayerT "dp-iso-a"
      pidB <- addPlayerT "dp-iso-b"
      pidC <- addPlayerT "dp-iso-c"
      _ <- DP.recordPickT (mkPick lcid1 ltid1 pidA 1)
      _ <- DP.recordPickT (mkPick lcid1 ltid1 pidB 2)
      _ <- DP.recordPickT (mkPick lcid2 ltid2 pidC 1)
      a <- DP.getPicksForLeagueT lcid1
      b <- DP.getPicksForLeagueT lcid2
      pure (length a, length b)
    n1 `shouldBe` 2
    n2 `shouldBe` 1

mkContext
  :: Text
  -> Tx.Transaction (DbLeagueConfigId, DbLeagueTeamId, DbPlayerId)
mkContext tag = do
  (lcid, ltid) <- addLeagueTeamWithConfigT tag
  pid          <- addPlayerT (tag <> "-player")
  pure (lcid, ltid, pid)

mkPick
  :: DbLeagueConfigId
  -> DbLeagueTeamId
  -> DbPlayerId
  -> Int
  -> DraftPickRow
mkPick lcid ltid pid n = DraftPickRow
  { dpId             = Nothing
  , dpLeagueConfigId = lcid
  , dpPickNumber     = fromIntegral n
  , dpLeagueTeamId   = ltid
  , dpPlayerId       = pid
  , dpPickedAt       = Nothing
  }