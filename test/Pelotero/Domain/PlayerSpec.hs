{-# LANGUAGE OverloadedStrings #-}

module Pelotero.Domain.PlayerSpec (spec) where

import qualified Data.Text             as T
import           Test.Hspec            (Spec, describe, it, shouldBe)

import           Pelotero.Domain.Player
                     ( Handedness (..)
                     , handChar
                     , parseHandedness
                     , renderHandedness
                     )

spec :: Spec
spec = describe "Pelotero.Domain.Player" $ do

  describe "handChar" $ do
    it "LeftHanded  -> 'L'" $ handChar LeftHanded  `shouldBe` 'L'
    it "RightHanded -> 'R'" $ handChar RightHanded `shouldBe` 'R'
    it "Switch      -> 'S'" $ handChar Switch      `shouldBe` 'S'

  describe "handChar agrees with renderHandedness for every constructor" $
    -- If a new constructor is added, this exhaustive list breaks at
    -- compile time via -Wincomplete-uni-patterns elsewhere; here we
    -- pin down the agreement so a divergence between handChar and
    -- renderHandedness can never silently appear.
    mapM_
      (\h -> it (show h) $
         T.singleton (handChar h) `shouldBe` renderHandedness h)
      [minBound .. maxBound :: Handedness]

  describe "parseHandedness . renderHandedness === Just" $
    mapM_
      (\h -> it (show h) $
         parseHandedness (renderHandedness h) `shouldBe` Just h)
      [minBound .. maxBound :: Handedness]