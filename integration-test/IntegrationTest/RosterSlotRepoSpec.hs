{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest.RosterSlotRepoSpec (spec) where

import           Data.List                (sort)
import           Data.Text                (Text)
import           Test.Hspec

import qualified Hasql.Transaction        as Tx

import qualified Pelotero.DB.RosterSlot   as RS
import           Pelotero.DB.Pool         (Pool)
import           Pelotero.DB.RosterSlot   (RosterSlotRow(..))
import           Pelotero.Domain.Id       (DbLeagueTeamId)

import           IntegrationTest.Fixtures
                   ( addLeagueTeamWithConfigT
                   , addPlayerT
                   )
import           IntegrationTest.Setup    (runRolledBack)

spec :: SpecWith Pool
spec = describe "Pelotero.DB.RosterSlot" $ do

  it "round-trips an inserted slot" $ \pool -> do
    rows <- runRolledBack pool $ do
      ltid <- mkLeagueTeam "rs-rt"
      pid  <- addPlayerT  "rs-rt-player"
      RS.addSlotT (RosterSlotRow ltid "catcher" pid)
      RS.getSlotsForTeamT ltid
    length rows     `shouldBe` 1
    map rsSlot rows `shouldBe` ["catcher"]

  it "addSlot is an upsert: re-adding the same player at a new slot moves them" $ \pool -> do
    rows <- runRolledBack pool $ do
      ltid <- mkLeagueTeam "rs-up"
      pid  <- addPlayerT  "rs-up-player"
      RS.addSlotT (RosterSlotRow ltid "catcher" pid)
      RS.addSlotT (RosterSlotRow ltid "first"   pid)
      RS.getSlotsForTeamT ltid
    length rows     `shouldBe` 1
    map rsSlot rows `shouldBe` ["first"]

  it "removeSlot removes the named player and leaves others" $ \pool -> do
    (rows, expectedSurvivor) <- runRolledBack pool $ do
      ltid <- mkLeagueTeam "rs-rm"
      pidA <- addPlayerT  "rs-rm-a"
      pidB <- addPlayerT  "rs-rm-b"
      RS.addSlotT (RosterSlotRow ltid "outfield" pidA)
      RS.addSlotT (RosterSlotRow ltid "outfield" pidB)
      RS.removeSlotT ltid pidA
      rs <- RS.getSlotsForTeamT ltid
      pure (rs, pidB)
    length rows         `shouldBe` 1
    map rsPlayerId rows `shouldBe` [expectedSurvivor]

  it "clearTeamRoster removes only the target team's rows" $ \pool -> do
    (n1, n2) <- runRolledBack pool $ do
      ltid1 <- mkLeagueTeam "rs-cl-1"
      ltid2 <- mkLeagueTeam "rs-cl-2"
      pid   <- addPlayerT  "rs-cl-player"
      RS.addSlotT (RosterSlotRow ltid1 "catcher" pid)
      RS.addSlotT (RosterSlotRow ltid2 "catcher" pid)
      RS.clearTeamRosterT ltid1
      rs1 <- RS.getSlotsForTeamT ltid1
      rs2 <- RS.getSlotsForTeamT ltid2
      pure (length rs1, length rs2)
    n1 `shouldBe` 0
    n2 `shouldBe` 1

  it "replaceTeamRoster atomically replaces the full set" $ \pool -> do
    slots <- runRolledBack pool $ do
      ltid <- mkLeagueTeam "rs-rep"
      pidA <- addPlayerT  "rs-rep-a"
      pidB <- addPlayerT  "rs-rep-b"
      pidC <- addPlayerT  "rs-rep-c"
      RS.addSlotT (RosterSlotRow ltid "catcher" pidA)
      RS.addSlotT (RosterSlotRow ltid "first"   pidB)
      RS.replaceTeamRosterT ltid
        [ RosterSlotRow ltid "shortstop" pidC
        , RosterSlotRow ltid "outfield"  pidA
        ]
      map rsSlot <$> RS.getSlotsForTeamT ltid
    sort slots `shouldBe` ["outfield", "shortstop"]

  it "countBySlot returns the right count, including zero" $ \pool -> do
    (nOutfield, nCatcher, nMissing) <- runRolledBack pool $ do
      ltid <- mkLeagueTeam "rs-ct"
      pidA <- addPlayerT  "rs-ct-a"
      pidB <- addPlayerT  "rs-ct-b"
      pidC <- addPlayerT  "rs-ct-c"
      RS.addSlotT (RosterSlotRow ltid "outfield" pidA)
      RS.addSlotT (RosterSlotRow ltid "outfield" pidB)
      RS.addSlotT (RosterSlotRow ltid "catcher"  pidC)
      a <- RS.countBySlotT ltid "outfield"
      b <- RS.countBySlotT ltid "catcher"
      c <- RS.countBySlotT ltid "third"
      pure (a, b, c)
    nOutfield `shouldBe` 2
    nCatcher  `shouldBe` 1
    nMissing  `shouldBe` 0

mkLeagueTeam :: Text -> Tx.Transaction DbLeagueTeamId
mkLeagueTeam tag = snd <$> addLeagueTeamWithConfigT tag