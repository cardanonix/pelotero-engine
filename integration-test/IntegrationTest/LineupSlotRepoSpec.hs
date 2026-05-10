{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest.LineupSlotRepoSpec (spec) where

import           Data.List                (sort)
import           Data.Text                (Text)
import           Test.Hspec

import qualified Hasql.Transaction        as Tx

import qualified Pelotero.DB.LineupSlot   as LS
import           Pelotero.DB.LineupSlot   (LineupSlotRow(..))
import           Pelotero.DB.Pool         (Pool)
import           Pelotero.Domain.Id       (DbLeagueTeamId)

import           IntegrationTest.Fixtures
                   ( addLeagueTeamWithConfigT
                   , addPlayerT
                   )
import           IntegrationTest.Setup    (runRolledBack)

spec :: SpecWith Pool
spec = describe "Pelotero.DB.LineupSlot" $ do

  it "round-trips an inserted slot" $ \pool -> do
    rows <- runRolledBack pool $ do
      ltid <- mkLeagueTeam "ls-rt"
      pid  <- addPlayerT  "ls-rt-player"
      LS.addSlotT (LineupSlotRow ltid "catcher" pid)
      LS.getSlotsForTeamT ltid
    length rows     `shouldBe` 1
    map lsSlot rows `shouldBe` ["catcher"]

  it "addSlot is an upsert: re-adding the same player at a new slot moves them" $ \pool -> do
    rows <- runRolledBack pool $ do
      ltid <- mkLeagueTeam "ls-up"
      pid  <- addPlayerT  "ls-up-player"
      LS.addSlotT (LineupSlotRow ltid "catcher" pid)
      LS.addSlotT (LineupSlotRow ltid "first"   pid)
      LS.getSlotsForTeamT ltid
    length rows     `shouldBe` 1
    map lsSlot rows `shouldBe` ["first"]

  it "removeSlot removes the named player and leaves others" $ \pool -> do
    (rows, expectedSurvivor) <- runRolledBack pool $ do
      ltid <- mkLeagueTeam "ls-rm"
      pidA <- addPlayerT  "ls-rm-a"
      pidB <- addPlayerT  "ls-rm-b"
      LS.addSlotT (LineupSlotRow ltid "outfield" pidA)
      LS.addSlotT (LineupSlotRow ltid "outfield" pidB)
      LS.removeSlotT ltid pidA
      rs <- LS.getSlotsForTeamT ltid
      pure (rs, pidB)
    length rows         `shouldBe` 1
    map lsPlayerId rows `shouldBe` [expectedSurvivor]

  it "clearTeamLineup removes only the target team's rows" $ \pool -> do
    (n1, n2) <- runRolledBack pool $ do
      ltid1 <- mkLeagueTeam "ls-cl-1"
      ltid2 <- mkLeagueTeam "ls-cl-2"
      pid   <- addPlayerT  "ls-cl-player"
      LS.addSlotT (LineupSlotRow ltid1 "catcher" pid)
      LS.addSlotT (LineupSlotRow ltid2 "catcher" pid)
      LS.clearTeamLineupT ltid1
      rs1 <- LS.getSlotsForTeamT ltid1
      rs2 <- LS.getSlotsForTeamT ltid2
      pure (length rs1, length rs2)
    n1 `shouldBe` 0
    n2 `shouldBe` 1

  it "replaceTeamLineup atomically replaces the full set" $ \pool -> do
    slots <- runRolledBack pool $ do
      ltid <- mkLeagueTeam "ls-rep"
      pidA <- addPlayerT  "ls-rep-a"
      pidB <- addPlayerT  "ls-rep-b"
      pidC <- addPlayerT  "ls-rep-c"
      LS.addSlotT (LineupSlotRow ltid "catcher" pidA)
      LS.addSlotT (LineupSlotRow ltid "first"   pidB)
      LS.replaceTeamLineupT ltid
        [ LineupSlotRow ltid "shortstop" pidC
        , LineupSlotRow ltid "outfield"  pidA
        ]
      map lsSlot <$> LS.getSlotsForTeamT ltid
    sort slots `shouldBe` ["outfield", "shortstop"]

mkLeagueTeam :: Text -> Tx.Transaction DbLeagueTeamId
mkLeagueTeam tag = snd <$> addLeagueTeamWithConfigT tag