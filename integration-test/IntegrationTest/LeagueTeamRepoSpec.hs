{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest.LeagueTeamRepoSpec (spec) where

import           Data.List                (sort)
import           Test.Hspec

import qualified Pelotero.DB.LeagueTeam   as LT
import           Pelotero.DB.LeagueTeam   (LeagueTeamRow(..))
import           Pelotero.DB.Pool         (Pool)

import           IntegrationTest.Fixtures
                   ( addLeagueConfigT
                   , mkLeagueTeamRow
                   )
import           IntegrationTest.Setup    (runRolledBack)

spec :: SpecWith Pool
spec = describe "Pelotero.DB.LeagueTeam" $ do

  it "round-trips an inserted league team" $ \pool -> do
    mGot <- runRolledBack pool $ do
      lcid <- addLeagueConfigT "lt-rt"
      ltid <- LT.insertLeagueTeamT (mkLeagueTeamRow lcid "alpha")
      LT.getByIdT ltid
    case mGot of
      Just got -> do
        LT.lltTeamKey got `shouldBe` "alpha-key"
        LT.lltName    got `shouldBe` "alpha-name"
        LT.lltOwner   got `shouldBe` "alpha-owner"
      Nothing -> expectationFailure "round-trip read returned Nothing"

  it "lookupByKey finds a team by (league_config_id, team_key)" $ \pool -> do
    mGot <- runRolledBack pool $ do
      lcid <- addLeagueConfigT "lt-key"
      _    <- LT.insertLeagueTeamT (mkLeagueTeamRow lcid "beta")
      LT.lookupByKeyT lcid "beta-key"
    case mGot of
      Just got -> LT.lltTeamKey got `shouldBe` "beta-key"
      Nothing  -> expectationFailure "expected to find by key"

  it "lookupByKey returns Nothing for an unknown key" $ \pool -> do
    mGot <- runRolledBack pool $ do
      lcid <- addLeagueConfigT "lt-mk"
      LT.lookupByKeyT lcid "no-such-key"
    mGot `shouldBe` Nothing

  it "getForLeague returns all teams for the given league" $ \pool -> do
    names <- runRolledBack pool $ do
      lcid <- addLeagueConfigT "lt-many"
      _    <- LT.insertLeagueTeamT (mkLeagueTeamRow lcid "alpha")
      _    <- LT.insertLeagueTeamT (mkLeagueTeamRow lcid "beta")
      _    <- LT.insertLeagueTeamT (mkLeagueTeamRow lcid "gamma")
      rows <- LT.getForLeagueT lcid
      pure (sort (map LT.lltName rows))
    names `shouldBe` ["alpha-name", "beta-name", "gamma-name"]

  it "getForLeague does not bleed across leagues" $ \pool -> do
    counts <- runRolledBack pool $ do
      lcid1 <- addLeagueConfigT "lt-iso-1"
      lcid2 <- addLeagueConfigT "lt-iso-2"
      _ <- LT.insertLeagueTeamT (mkLeagueTeamRow lcid1 "alpha")
      _ <- LT.insertLeagueTeamT (mkLeagueTeamRow lcid1 "beta")
      _ <- LT.insertLeagueTeamT (mkLeagueTeamRow lcid2 "gamma")
      a <- LT.getForLeagueT lcid1
      b <- LT.getForLeagueT lcid2
      pure (length a, length b)
    counts `shouldBe` (2, 1)

  it "updateLeagueTeam overwrites the row" $ \pool -> do
    mGot <- runRolledBack pool $ do
      lcid <- addLeagueConfigT "lt-upd"
      ltid <- LT.insertLeagueTeamT (mkLeagueTeamRow lcid "delta")
      let updated = (mkLeagueTeamRow lcid "delta")
            { ltName  = "renamed"
            , ltOwner = "new-owner"
            }
      LT.updateLeagueTeamT ltid updated
      LT.getByIdT ltid
    case mGot of
      Just got -> do
        LT.lltName  got `shouldBe` "renamed"
        LT.lltOwner got `shouldBe` "new-owner"
      Nothing -> expectationFailure "expected to find updated team"

  it "deleteT removes the named team" $ \pool -> do
    result <- runRolledBack pool $ do
      lcid <- addLeagueConfigT "lt-del"
      ltid <- LT.insertLeagueTeamT (mkLeagueTeamRow lcid "doomed")
      LT.deleteT ltid
      LT.getByIdT ltid
    result `shouldBe` Nothing