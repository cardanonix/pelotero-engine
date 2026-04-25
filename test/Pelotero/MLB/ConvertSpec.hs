-- test/Pelotero/MLB/ConvertSpec.hs
module Pelotero.MLB.ConvertSpec (spec) where

import Data.Aeson (FromJSON, eitherDecodeStrict)
import qualified Data.ByteString as BS
import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  , shouldContain
  , shouldSatisfy, expectationFailure
  )

import Pelotero.Domain.Game (Game(..), GameSchedule(..))
import Pelotero.Domain.Id (GameId(..), PlayerId(..), TeamId(..))
import Pelotero.Domain.Player (Player(..))
import Pelotero.Domain.Position (Position(..))
import Pelotero.MLB.Convert
  ( BoxscoreEntry(..)
  , ConvertWarning(..)
  , convertBoxscore
  , convertPlayers
  , convertSchedule
  )
import Pelotero.MLB.Wire.Boxscore ()
import Pelotero.MLB.Wire.Player   ()
import Pelotero.MLB.Wire.Schedule ()

spec :: Spec
spec = do
  describe "convertPlayers" $ do
    it "converts a clean roster sample with no warnings" $ do
      env <- decodeFixture "test/fixtures/players-clean.json"
      let (warns, players) = convertPlayers env
      warns `shouldBe` []
      length players `shouldBe` 2
      map playerId players       `shouldBe` [PlayerId 660271, PlayerId 545361]
      map playerPosition players `shouldBe` [Just Pitcher, Just CenterField]
      map playerActive players   `shouldBe` [True, True]

    it "drops invalid IDs and warns about unknown positions" $ do
      env <- decodeFixture "test/fixtures/players-dirty.json"
      let (warns, players)               = convertPlayers env
          isUnknownPos UnknownPosition{} = True
          isUnknownPos _                 = False
      warns `shouldContain` [InvalidPlayerId 0]
      warns `shouldSatisfy` any isUnknownPos
      case players of
        [p] -> playerPosition p `shouldBe` Nothing
        _   -> expectationFailure $ "expected exactly one player, got " <> show (length players)

  describe "convertSchedule" $ do
    it "flattens dates and games" $ do
      env <- decodeFixture "test/fixtures/schedule.json"
      let (warns, GameSchedule games) = convertSchedule env
      warns                  `shouldBe` []
      length games           `shouldBe` 2
      map gameId games       `shouldBe` [GameId 778001, GameId 778002]
      map gameAwayTeam games `shouldBe` [TeamId 117, TeamId 110]
      map gameHomeTeam games `shouldBe` [TeamId 121, TeamId 145]

  describe "convertBoxscore" $ do
    it "produces one entry per appearance, tagged with the game id" $ do
      bs <- decodeFixture "test/fixtures/boxscore.json"
      let gid              = GameId 778001
          (warns, entries) = convertBoxscore gid bs
      warns                                 `shouldBe` []
      length entries                        `shouldBe` 2
      all ((== gid) . boxGameId) entries    `shouldBe` True

decodeFixture :: FromJSON a => FilePath -> IO a
decodeFixture path = do
  bs <- BS.readFile path
  case eitherDecodeStrict bs of
    Right v  -> pure v
    Left err -> error ("fixture " <> path <> ": " <> err)