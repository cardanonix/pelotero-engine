module Pelotero.Domain.PositionSpec (spec) where

import Hedgehog (Gen, forAll, property, tripping, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Pelotero.Domain.Position
  ( Position(..)
  , isInfielder
  , isOutfielder
  , isPitcher
  , parsePosition
  , renderPosition
  )

spec :: Spec
spec = do
  describe "parsePosition / renderPosition" $ do
    it "round-trips for every Position" $ hedgehog $ property $ do
      p <- forAll genPosition
      tripping p renderPosition parsePosition

    it "accepts both numeric and scorer codes for one fixed point" $ do
      parsePosition "5"  `shouldBe` Just ThirdBase
      parsePosition "3B" `shouldBe` Just ThirdBase
      parsePosition "10" `shouldBe` Just DesignatedHitter
      parsePosition "DH" `shouldBe` Just DesignatedHitter

    it "rejects unknown codes" $ do
      parsePosition ""    `shouldBe` Nothing
      parsePosition "TWP" `shouldBe` Nothing
      parsePosition "11"  `shouldBe` Nothing

  describe "classification predicates" $ do
    it "Pitcher is a pitcher and nothing else" $ do
      isPitcher Pitcher       `shouldBe` True
      isInfielder Pitcher     `shouldBe` False
      isOutfielder Pitcher    `shouldBe` False

    it "Catcher is an infielder" $
      isInfielder Catcher `shouldBe` True

    it "outfielders are outfielders, not infielders" $ do
      isOutfielder LeftField   `shouldBe` True
      isOutfielder CenterField `shouldBe` True
      isOutfielder RightField  `shouldBe` True
      isInfielder LeftField    `shouldBe` False

    it "DH is none of the three" $ do
      isPitcher    DesignatedHitter `shouldBe` False
      isInfielder  DesignatedHitter `shouldBe` False
      isOutfielder DesignatedHitter `shouldBe` False

genPosition :: Gen Position
genPosition = Gen.element [minBound .. maxBound]