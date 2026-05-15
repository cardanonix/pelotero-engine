module Pelotero.Effects.PlayersSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Effectful (runEff)

import Pelotero.DB.Player   (PlayerRow(..), LoadedPlayerRow(..))
import Pelotero.DB.Provider (ProviderName(..))
import Pelotero.Domain.Id   (DbPlayerId(..))
import Pelotero.Effects.Players

spec :: Spec
spec = describe "Pelotero.Effects.Players (in-memory)" $ do

  it "round-trips an upserted player" $ do
    let row = mkRow "Test" "Player"
    result <- runEff $ runPlayersInMemory $ do
      pid <- upsertPlayerByExternalId ProviderMLB "ext-1" row
      getPlayerById pid
    case result of
      Just got -> do
        lprFirstName got `shouldBe` "Test"
        lprLastName  got `shouldBe` "Player"
      Nothing  ->
        expectationFailure "expected to find the upserted player"

  it "upsert returns the same id on the second call with the same external id" $ do
    let row1 = mkRow "Initial" "Name"
        row2 = mkRow "Updated" "Name"
    (pid1, pid2) <- runEff $ runPlayersInMemory $ do
      a <- upsertPlayerByExternalId ProviderMLB "ext-2" row1
      b <- upsertPlayerByExternalId ProviderMLB "ext-2" row2
      pure (a, b)
    pid1 `shouldBe` pid2

  it "upsert overwrites the row data on second call" $ do
    let row1 = mkRow "Initial" "Name"
        row2 = mkRow "Updated" "Name"
    result <- runEff $ runPlayersInMemory $ do
      _   <- upsertPlayerByExternalId ProviderMLB "ext-3" row1
      pid <- upsertPlayerByExternalId ProviderMLB "ext-3" row2
      getPlayerById pid
    case result of
      Just got -> lprFirstName got `shouldBe` "Updated"
      Nothing  -> expectationFailure "expected to find the upserted player"

  it "getActivePlayers filters out inactive ones" $ do
    let active   = mkRow "Active"   "One"
        active2  = mkRow "Active"   "Two"
        inactive = (mkRow "Inactive" "One") { playerRowActive = False }
    rows <- runEff $ runPlayersInMemory $ do
      _ <- upsertPlayerByExternalId ProviderMLB "a1" active
      _ <- upsertPlayerByExternalId ProviderMLB "a2" active2
      _ <- upsertPlayerByExternalId ProviderMLB "i1" inactive
      getActivePlayers
    length rows `shouldBe` 2
    all lprActive rows `shouldBe` True

  it "getPlayerById returns Nothing for unknown ids" $ do
    result <- runEff $ runPlayersInMemory $ do
      _   <- upsertPlayerByExternalId ProviderMLB "only" (mkRow "Only" "Player")
      getPlayerById (DbPlayerId 99999)
    result `shouldBe` Nothing

mkRow :: T.Text -> T.Text -> PlayerRow
mkRow first last_ = PlayerRow
  { playerRowId                 = Nothing
  , playerRowFirstName          = first
  , playerRowLastName           = last_
  , playerRowNameSlug           = first <> "-" <> last_
  , playerRowPosition           = Nothing
  , playerRowBatSide            = Nothing
  , playerRowPitchHand          = Nothing
  , playerRowActive             = True
  , playerRowCurrentTeamId      = Nothing
  , playerRowLastSyncedProvider = Just ProviderMLB
  , playerRowLastSyncedAt       = Nothing
  }