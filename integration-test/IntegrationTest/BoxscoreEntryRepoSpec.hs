{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest.BoxscoreEntryRepoSpec (spec) where

import           Data.Time                (fromGregorian)
import           Test.Hspec

import qualified Pelotero.DB.BoxscoreEntry as Box
import qualified Pelotero.DB.Game          as Game
import qualified Pelotero.DB.Player        as Player
import qualified Pelotero.DB.Team          as Team
import           Pelotero.DB.BoxscoreEntry (BattingRow(..), PitchingRow(..))
import           Pelotero.DB.Pool          (Pool)

import           IntegrationTest.Fixtures
                   ( mkGameRow
                   , mkPlayerRow
                   , mkTeamRow
                   , zeroBatting
                   , zeroPitching
                   )
import           IntegrationTest.Setup     (runRolledBack)

spec :: SpecWith Pool
spec = describe "Pelotero.DB.BoxscoreEntry" $ do

  it "round-trips a batting row" $ \pool -> do
    result <- runRolledBack pool $ do
      atid <- Team.insertTeamT (mkTeamRow "Box Away" "BXA")
      htid <- Team.insertTeamT (mkTeamRow "Box Home" "BXH")
      gid  <- Game.insertGameT (mkGameRow (fromGregorian 2025 4 1) atid htid)
      pid  <- Player.insertPlayerT (mkPlayerRow "box-batter")
      let batting = (zeroBatting gid pid)
            { battingTeamId   = Just atid
            , battingAtBats   = Just 4
            , battingHits     = Just 2
            , battingHomeRuns = Just 1
            , battingRbi      = Just 3
            }
      Box.upsertBattingT batting
      Box.getBattingForGameT gid
    case result of
      [b] -> do
        battingHits     b `shouldBe` Just 2
        battingHomeRuns b `shouldBe` Just 1
        battingRbi      b `shouldBe` Just 3
      _ -> expectationFailure $
             "expected exactly one batting row; got " ++ show (length result)

  it "round-trips a pitching row with innings stored as outs" $ \pool -> do
    result <- runRolledBack pool $ do
      atid <- Team.insertTeamT (mkTeamRow "Pitch Away" "PTA")
      htid <- Team.insertTeamT (mkTeamRow "Pitch Home" "PTH")
      gid  <- Game.insertGameT (mkGameRow (fromGregorian 2025 4 2) atid htid)
      pid  <- Player.insertPlayerT (mkPlayerRow "box-pitcher")
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
      _ -> expectationFailure $
             "expected exactly one pitching row; got " ++ show (length result)

  it "upsert overwrites existing batting on (game_id, player_id) conflict" $ \pool -> do
    result <- runRolledBack pool $ do
      atid <- Team.insertTeamT (mkTeamRow "Up Away" "UPA")
      htid <- Team.insertTeamT (mkTeamRow "Up Home" "UPH")
      gid  <- Game.insertGameT (mkGameRow (fromGregorian 2025 4 3) atid htid)
      pid  <- Player.insertPlayerT (mkPlayerRow "twice-inserted")
      let initial = (zeroBatting gid pid) { battingTeamId = Just atid, battingHits = Just 1 }
          updated = (zeroBatting gid pid) { battingTeamId = Just atid, battingHits = Just 5 }
      Box.upsertBattingT initial
      Box.upsertBattingT updated
      Box.getBattingForGameT gid
    case result of
      [b] -> battingHits b `shouldBe` Just 5
      _   -> expectationFailure "expected one batting row after two upserts"

  it "deleteBattingForGameT removes only that game's rows" $ \pool -> do
    result <- runRolledBack pool $ do
      atid <- Team.insertTeamT (mkTeamRow "Del Away" "DLA")
      htid <- Team.insertTeamT (mkTeamRow "Del Home" "DLH")
      gid1 <- Game.insertGameT (mkGameRow (fromGregorian 2025 4 4) atid htid)
      gid2 <- Game.insertGameT (mkGameRow (fromGregorian 2025 4 5) atid htid)
      pid  <- Player.insertPlayerT (mkPlayerRow "cross-game")
      Box.upsertBattingT (zeroBatting gid1 pid) { battingHits = Just 2 }
      Box.upsertBattingT (zeroBatting gid2 pid) { battingHits = Just 3 }
      Box.deleteBattingForGameT gid1
      rows1 <- Box.getBattingForGameT gid1
      rows2 <- Box.getBattingForGameT gid2
      pure (length rows1, length rows2)
    result `shouldBe` (0, 1)