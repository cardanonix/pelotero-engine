module Pelotero.Domain.DraftSpec (spec) where

import qualified Data.Text as T
import Test.Hspec (Spec, describe, it, shouldBe)

import Pelotero.Domain.Draft
  ( DraftOrderStrategy(..)
  , extendRankingsWithUnranked
  , generateDraftOrder
  )
import Pelotero.Domain.Id
  ( DraftPickNumber(..)
  , FantasyTeamId(..)
  , PlayerId(..)
  )

spec :: Spec
spec = do
  describe "generateDraftOrder (serpentine)" $ do
    it "alternates direction round-by-round" $ do
      let a = FantasyTeamId "1"
          b = FantasyTeamId "2"
          c = FantasyTeamId "3"
          d = FantasyTeamId "4"
          order = generateDraftOrder SerpentineOrder 8 [a, b, c, d]
      map fst order `shouldBe` [a, b, c, d, d, c, b, a]
      map snd order `shouldBe` map DraftPickNumber [1 .. 8]

    it "truncates partial trailing rounds" $ do
      let teams = map (FantasyTeamId . T.pack . show) [1 :: Int .. 3]
      length (generateDraftOrder SerpentineOrder 7 teams) `shouldBe` 6

    it "returns nothing for empty teams" $
      generateDraftOrder SerpentineOrder 10 [] `shouldBe` []

  describe "extendRankingsWithUnranked" $ do
    it "appends universe-but-not-ranked, preserving universe order" $ do
      let ranked   = [PlayerId 3, PlayerId 1]
          universe = [PlayerId 1, PlayerId 2, PlayerId 3, PlayerId 4]
      extendRankingsWithUnranked ranked universe
        `shouldBe` [PlayerId 3, PlayerId 1, PlayerId 2, PlayerId 4]

    it "is a no-op when ranked already covers the universe" $ do
      let universe = [PlayerId 1, PlayerId 2]
          ranked   = [PlayerId 2, PlayerId 1]
      extendRankingsWithUnranked ranked universe `shouldBe` ranked