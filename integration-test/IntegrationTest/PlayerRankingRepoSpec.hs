{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest.PlayerRankingRepoSpec (spec) where

import           Data.Text                  (Text)
import           Test.Hspec

import qualified Hasql.Transaction          as Tx

import qualified Pelotero.DB.PlayerRanking  as PR
import           Pelotero.DB.PlayerRanking  (PlayerRankingRow(..))
import           Pelotero.DB.Pool           (Pool)
import           Pelotero.Domain.Id         (DbLeagueTeamId)

import           IntegrationTest.Fixtures
                   ( addLeagueTeamWithConfigT
                   , addPlayerT
                   )
import           IntegrationTest.Setup      (runRolledBack)

spec :: SpecWith Pool
spec = describe "Pelotero.DB.PlayerRanking" $ do

  it "replaceRankings + getRankingsForTeam round-trips and orders by rank_slot" $ \pool -> do
    rows <- runRolledBack pool $ do
      ltid <- mkLeagueTeam "pr-rt"
      pidA <- addPlayerT  "pr-rt-a"
      pidB <- addPlayerT  "pr-rt-b"
      pidC <- addPlayerT  "pr-rt-c"
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
      pidA <- addPlayerT  "pr-rep-a"
      pidB <- addPlayerT  "pr-rep-b"
      pidC <- addPlayerT  "pr-rep-c"
      PR.replaceRankingsT ltid
        [ PlayerRankingRow ltid pidA 1
        , PlayerRankingRow ltid pidB 2
        ]
      PR.replaceRankingsT ltid
        [ PlayerRankingRow ltid pidC 1 ]
      PR.getRankingsForTeamT ltid
    length rows         `shouldBe` 1
    map prRankSlot rows `shouldBe` [1]

  it "clearRankings empties the team's rankings" $ \pool -> do
    n <- runRolledBack pool $ do
      ltid <- mkLeagueTeam "pr-clr"
      pid  <- addPlayerT  "pr-clr-a"
      PR.replaceRankingsT ltid [ PlayerRankingRow ltid pid 1 ]
      PR.clearRankingsT ltid
      rs <- PR.getRankingsForTeamT ltid
      pure (length rs)
    n `shouldBe` 0

  it "getRankingCount returns the correct count" $ \pool -> do
    (nWithRows, nEmpty) <- runRolledBack pool $ do
      ltid1 <- mkLeagueTeam "pr-ct-1"
      ltid2 <- mkLeagueTeam "pr-ct-2"
      pidA  <- addPlayerT  "pr-ct-a"
      pidB  <- addPlayerT  "pr-ct-b"
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
      pidA  <- addPlayerT  "pr-iso-a"
      pidB  <- addPlayerT  "pr-iso-b"
      pidC  <- addPlayerT  "pr-iso-c"
      PR.replaceRankingsT ltid1
        [ PlayerRankingRow ltid1 pidA 1
        , PlayerRankingRow ltid1 pidB 2
        ]
      PR.replaceRankingsT ltid2
        [ PlayerRankingRow ltid2 pidC 1 ]
      a <- PR.getRankingsForTeamT ltid1
      b <- PR.getRankingsForTeamT ltid2
      pure (length a, length b)
    n1 `shouldBe` 2
    n2 `shouldBe` 1

mkLeagueTeam :: Text -> Tx.Transaction DbLeagueTeamId
mkLeagueTeam tag = snd <$> addLeagueTeamWithConfigT tag