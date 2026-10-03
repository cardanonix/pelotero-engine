module Pelotero.Effects.RandomSpec (spec) where

import           Data.List               (sort)
import           Effectful               (runPureEff)
import           Hedgehog                (assert, forAll, (===))
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range
import           Test.Hspec              (Spec, describe, it, shouldBe, shouldNotBe)
import           Test.Hspec.Hedgehog     (hedgehog)

import           Pelotero.Effects.Random

spec :: Spec
spec = describe "Pelotero.Effects.Random" $ do

  it "shuffle returns a permutation of its input" $
    hedgehog $ do
      seed <- forAll (Gen.int Range.linearBounded)
      xs   <- forAll (Gen.list (Range.linear 0 200) (Gen.int (Range.linear 0 50)))
      let ys = runPureEff (runRandomSeeded seed (shuffle xs))
      sort ys === sort xs

  it "the same seed gives the same results" $
    hedgehog $ do
      seed <- forAll (Gen.int Range.linearBounded)
      let run = runPureEff $ runRandomSeeded seed $ do
            a <- shuffle [1 .. 50 :: Int]
            b <- uniformInt (0, 1000)
            c <- shuffle "pelotero"
            pure (a, b, c)
      run === run

  it "successive shuffles in one run differ from each other" $ do
    let (a, b) = runPureEff $ runRandomSeeded 7 $
          (,) <$> shuffle [1 .. 100 :: Int] <*> shuffle [1 .. 100 :: Int]
    a `shouldNotBe` b

  it "different seeds give different shuffles of a long list" $
    runPureEff (runRandomSeeded 1 (shuffle [1 .. 100 :: Int]))
      `shouldNotBe` runPureEff (runRandomSeeded 2 (shuffle [1 .. 100 :: Int]))

  it "uniformInt stays inside its inclusive range" $
    hedgehog $ do
      seed <- forAll (Gen.int Range.linearBounded)
      lo   <- forAll (Gen.int (Range.linear (-100) 100))
      span' <- forAll (Gen.int (Range.linear 0 100))
      let n = runPureEff (runRandomSeeded seed (uniformInt (lo, lo + span')))
      assert (n >= lo && n <= lo + span')

  it "shuffle of an empty list is empty" $
    runPureEff (runRandomSeeded 0 (shuffle ([] :: [Int]))) `shouldBe` []
