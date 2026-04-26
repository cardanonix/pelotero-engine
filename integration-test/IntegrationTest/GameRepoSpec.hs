-- | Round-trip tests for "Pelotero.DB.Game".
module IntegrationTest.GameRepoSpec (spec) where

import Data.Time (fromGregorian)
import Test.Hspec

import qualified Pelotero.DB.Game     as Game
import qualified Pelotero.DB.Team     as Team
import           Pelotero.DB.Game     (GameRow(..))
import           Pelotero.DB.Team     (TeamRow(..))
import           Pelotero.DB.Provider (ProviderName(..))

import IntegrationTest.Setup

spec :: Spec
spec = around withTestPool $
  describe "Pelotero.DB.Game" $ do

    it "round-trips an inserted game" $ \pool -> do
      let awayTeam = TeamRow Nothing "Away Team" "AWY" "Awayton"
                             (Just ProviderMLB) Nothing
          homeTeam = TeamRow Nothing "Home Team" "HOM" "Homeville"
                             (Just ProviderMLB) Nothing
          mkGame at ht = GameRow
            { gameRowId = Nothing
            , gameRowGameDate = fromGregorian 2025 4 12
            , gameRowAwayTeamId = at
            , gameRowHomeTeamId = ht
            , gameRowLastSyncedProvider = Just ProviderMLB
            , gameRowLastSyncedAt = Nothing
            }
      result <- runRolledBack pool $ do
        atid <- Team.insertTeamT awayTeam
        htid <- Team.insertTeamT homeTeam
        gid  <- Game.insertGameT (mkGame atid htid)
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
      let mkTeam name abbr = TeamRow Nothing name abbr "Anywhere"
                                     (Just ProviderMLB) Nothing
          extId = "game-link-test-id"
      result <- runRolledBack pool $ do
        atid <- Team.insertTeamT (mkTeam "Away One" "AW1")
        htid <- Team.insertTeamT (mkTeam "Home One" "HM1")
        gid  <- Game.insertGameT GameRow
          { gameRowId = Nothing
          , gameRowGameDate = fromGregorian 2025 5 1
          , gameRowAwayTeamId = atid
          , gameRowHomeTeamId = htid
          , gameRowLastSyncedProvider = Just ProviderMLB
          , gameRowLastSyncedAt = Nothing
          }
        Game.linkExternalIdT gid ProviderMLB extId
        found <- Game.lookupByExternalIdT ProviderMLB extId
        pure (gid, found)
      let (gid, found) = result
      found `shouldBe` Just gid

    it "getByDate returns games on the requested date" $ \pool -> do
      let mkTeam name abbr = TeamRow Nothing name abbr "Anywhere"
                                     (Just ProviderMLB) Nothing
          targetDate = fromGregorian 2025 6 15
          otherDate  = fromGregorian 2025 6 16
          mkGame d at ht = GameRow
            { gameRowId = Nothing
            , gameRowGameDate = d
            , gameRowAwayTeamId = at
            , gameRowHomeTeamId = ht
            , gameRowLastSyncedProvider = Just ProviderMLB
            , gameRowLastSyncedAt = Nothing
            }
      gamesOnTarget <- runRolledBack pool $ do
        atid1 <- Team.insertTeamT (mkTeam "ByDate Away 1" "BD1")
        htid1 <- Team.insertTeamT (mkTeam "ByDate Home 1" "BD2")
        atid2 <- Team.insertTeamT (mkTeam "ByDate Away 2" "BD3")
        htid2 <- Team.insertTeamT (mkTeam "ByDate Home 2" "BD4")
        _ <- Game.insertGameT (mkGame targetDate atid1 htid1)
        _ <- Game.insertGameT (mkGame targetDate atid2 htid2)
        _ <- Game.insertGameT (mkGame otherDate  atid1 htid2)
        Game.getByDateT targetDate
      length gamesOnTarget `shouldBe` 2
      all (\g -> gameRowGameDate g == targetDate) gamesOnTarget `shouldBe` True

    it "upsertByExternalIdT inserts then updates" $ \pool -> do
      let mkTeam name abbr = TeamRow Nothing name abbr "Anywhere"
                                     (Just ProviderMLB) Nothing
          extId = "game-upsert-test-id"
      result <- runRolledBack pool $ do
        atid1 <- Team.insertTeamT (mkTeam "Upsert Away 1" "UA1")
        htid1 <- Team.insertTeamT (mkTeam "Upsert Home 1" "UH1")
        atid2 <- Team.insertTeamT (mkTeam "Upsert Away 2" "UA2")
        htid2 <- Team.insertTeamT (mkTeam "Upsert Home 2" "UH2")
        let initial = GameRow
              { gameRowId = Nothing
              , gameRowGameDate = fromGregorian 2025 7 1
              , gameRowAwayTeamId = atid1
              , gameRowHomeTeamId = htid1
              , gameRowLastSyncedProvider = Just ProviderMLB
              , gameRowLastSyncedAt = Nothing
              }
            updated = initial
              { gameRowGameDate   = fromGregorian 2025 7 2
              , gameRowAwayTeamId = atid2
              , gameRowHomeTeamId = htid2
              }
        gid1  <- Game.upsertByExternalIdT ProviderMLB extId initial
        gid2  <- Game.upsertByExternalIdT ProviderMLB extId updated
        after <- Game.getByIdT gid2
        pure (gid1 == gid2, after)
      case result of
        (sameId, Just g) -> do
          sameId             `shouldBe` True
          gameRowGameDate g  `shouldBe` fromGregorian 2025 7 2
        (_, Nothing) ->
          expectationFailure "expected the upserted game"