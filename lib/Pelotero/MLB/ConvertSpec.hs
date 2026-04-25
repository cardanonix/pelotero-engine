module Pelotero.MLB.ConvertSpec (spec) where

import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString as BS
import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  , shouldContain
  , shouldSatisfy
  )

import Pelotero.Domain.Game (GameSchedule(..), Game(..))
import Pelotero.Domain.Id (GameId(..), PlayerId(..), TeamId(..))
import Pelotero.Domain.Player (Player(..))
import Pelotero.Domain.Position (Position(..))
import Pelotero.MLB.Convert
  ( ConvertWarning(..)
  , convertBoxscore
  , convertPlayers
  , convertSchedule
  )
import qualified Pelotero.MLB.Wire.Boxscore as WB
import qualified Pelotero.MLB.Wire.Player as WP
import qualified Pelotero.MLB.Wire.Schedule as WS

spec :: Spec
spec = do
  describe "convertPlayers" $ do
    it "converts a clean roster sample with no warnings" $ do
      env <- decodeFixture "test/fixtures/players-clean.json"
      let (warns, players) = convertPlayers env
      warns `shouldBe` []
      length players `shouldBe` 2
      map playerId players `shouldBe` [PlayerId 660271, PlayerId 545361]
      map playerPosition players
        `shouldBe` [Just Pitcher, Just CenterField]
      map playerActive players `shouldBe` [True, True]

    it "drops invalid IDs and warns about unknown positions" $ do
      env <- decodeFixture "test/fixtures/players-dirty.json"
      let (warns, players) = convertPlayers env
      length players `shouldBe` 1                             -- the 0-id is dropped
      warns `shouldContain` [InvalidPlayerId 0]
      warns `shouldSatisfy` any isUnknownPos
      playerPosition (head players) `shouldBe` Nothing        -- "TWP" doesn't parse
      where
        isUnknownPos UnknownPosition{} = True
        isUnknownPos _                 = False

  describe "convertSchedule" $ do
    it "flattens dates and games" $ do
      env <- decodeFixture "test/fixtures/schedule.json"
      let (warns, GameSchedule games) = convertSchedule env
      warns `shouldBe` []
      length games `shouldBe` 2
      map gameId games `shouldBe` [GameId 778001, GameId 778002]
      map gameAwayTeam games `shouldBe` [TeamId 117, TeamId 110]
      map gameHomeTeam games `shouldBe` [TeamId 121, TeamId 145]

  describe "convertBoxscore" $ do
    it "produces one entry per appearance" $ do
      bs <- decodeFixture "test/fixtures/boxscore.json"
      let (warns, entries) = convertBoxscore bs
      warns `shouldBe` []
      length entries `shouldBe` 2

decodeFixture :: forall a. (Eq a, Show a, FromJSONFixture a) => FilePath -> IO a
decodeFixture path = do
  bs <- BS.readFile path
  case decodeFix bs of
    Right v  -> pure v
    Left err -> error ("fixture " <> path <> ": " <> err)

-- | Tiny class so we can decode each fixture into the right wire type. Avoids
-- a separate top-level `decode` per call site without leaking the decode
-- function names into every test case.
class FromJSONFixture a where
  decodeFix :: BS.ByteString -> Either String a

instance FromJSONFixture WP.WirePlayerEnvelope where
  decodeFix = eitherDecodeStrict

instance FromJSONFixture WS.WireScheduleEnvelope where
  decodeFix = eitherDecodeStrict

instance FromJSONFixture WB.WireBoxscore where
  decodeFix = eitherDecodeStrict