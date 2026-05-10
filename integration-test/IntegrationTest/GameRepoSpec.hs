{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest.GameRepoSpec (spec) where

import           Data.Time                (fromGregorian)
import           Test.Hspec

import qualified Pelotero.DB.Game         as Game
import qualified Pelotero.DB.Team         as Team
import           Pelotero.DB.Game         (GameRow(..))
import           Pelotero.DB.Pool         (Pool)
import           Pelotero.DB.Provider     (ProviderName(..))

import           IntegrationTest.Fixtures (mkGameRow, mkTeamRow)
import           IntegrationTest.Setup    (runRolledBack)

spec :: SpecWith Pool
spec = describe "Pelotero.DB.Game" $ do

  it "round-trips an inserted game" $ \pool -> do
    let awayTeam = mkTeamRow "Away Team" "AWY"
        homeTeam = mkTeamRow "Home Team" "HOM"
    result <- runRolledBack pool $ do
      atid <- Team.insertTeamT awayTeam
      htid <- Team.insertTeamT homeTeam
      gid  <- Game.insertGameT (mkGameRow (fromGregorian 2025 4 12) atid htid)
      got  <- Game.getByIdT gid
      pure (atid, htid, got)
    case result of
      (atid, htid, Just g) -> do
        gameRowGameDate   g `shouldBe` fromGregorian 2025 4 12
        gameRowAwayTeamId g `shouldBe` atid
        gameRowHomeTeamId g `shouldBe` htid
      (_, _, Nothing) ->
        expectationFailure "round-trip read returned Nothing"

  it "links and looks up a game external id" $ \pool -> do
    let extId = "game-link-test-id"
    result <- runRolledBack pool $ do
      atid <- Team.insertTeamT (mkTeamRow "Away One" "AW1")
      htid <- Team.insertTeamT (mkTeamRow "Home One" "HM1")
      gid  <- Game.insertGameT (mkGameRow (fromGregorian 2025 5 1) atid htid)
      Game.linkExternalIdT gid ProviderMLB extId
      found <- Game.lookupByExternalIdT ProviderMLB extId
      pure (gid, found)
    let (gid, found) = result
    found `shouldBe` Just gid

  it "getByDate returns games on the requested date" $ \pool -> do
    let targetDate = fromGregorian 2025 6 15
        otherDate  = fromGregorian 2025 6 16
    gamesOnTarget <- runRolledBack pool $ do
      atid1 <- Team.insertTeamT (mkTeamRow "ByDate Away 1" "BD1")
      htid1 <- Team.insertTeamT (mkTeamRow "ByDate Home 1" "BD2")
      atid2 <- Team.insertTeamT (mkTeamRow "ByDate Away 2" "BD3")
      htid2 <- Team.insertTeamT (mkTeamRow "ByDate Home 2" "BD4")
      _ <- Game.insertGameT (mkGameRow targetDate atid1 htid1)
      _ <- Game.insertGameT (mkGameRow targetDate atid2 htid2)
      _ <- Game.insertGameT (mkGameRow otherDate  atid1 htid2)
      Game.getByDateT targetDate
    length gamesOnTarget `shouldBe` 2
    all (\g -> gameRowGameDate g == targetDate) gamesOnTarget `shouldBe` True

  it "upsertByExternalIdT inserts then updates" $ \pool -> do
    let extId = "game-upsert-test-id"
    result <- runRolledBack pool $ do
      atid1 <- Team.insertTeamT (mkTeamRow "Upsert Away 1" "UA1")
      htid1 <- Team.insertTeamT (mkTeamRow "Upsert Home 1" "UH1")
      atid2 <- Team.insertTeamT (mkTeamRow "Upsert Away 2" "UA2")
      htid2 <- Team.insertTeamT (mkTeamRow "Upsert Home 2" "UH2")
      let initial = mkGameRow (fromGregorian 2025 7 1) atid1 htid1
          updated = initial
            { gameRowGameDate   = fromGregorian 2025 7 2
            , gameRowAwayTeamId = atid2
            , gameRowHomeTeamId = htid2
            }
      gid1   <- Game.upsertByExternalIdT ProviderMLB extId initial
      gid2   <- Game.upsertByExternalIdT ProviderMLB extId updated
      mAfter <- Game.getByIdT gid2
      pure (gid1 == gid2, mAfter)
    case result of
      (sameId, Just g) -> do
        sameId             `shouldBe` True
        gameRowGameDate g  `shouldBe` fromGregorian 2025 7 2
      (_, Nothing) ->
        expectationFailure "expected the upserted game"