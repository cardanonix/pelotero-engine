module Pelotero.Domain.RosterSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldSatisfy)

import Pelotero.Domain.Id (PlayerId(..))
import Pelotero.Domain.Roster
  ( Roster
  , RosterError(..)
  , RosterLimits(..)
  , addToRoster
  , countAt
  , emptyRoster
  , rosterAt
  , rosterContains
  , rosterPlayers
  , removeFromRoster
  , RosterSlot(..)
  , validateRoster
  )

spec :: Spec
spec = do
  describe "emptyRoster" $ do
    it "has all slots present and empty" $ do
      let r = emptyRoster
      countAt SlotCatcher r         `shouldBe` 0
      countAt SlotStartingPitcher r `shouldBe` 0
      length (rosterPlayers r)      `shouldBe` 0

  describe "addToRoster / removeFromRoster" $ do
    it "appends in order and removes the first occurrence" $ do
      let pa = PlayerId 1
          pb = PlayerId 2
          pc = PlayerId 3
          r  = addToRoster SlotOutfield pc
             . addToRoster SlotOutfield pb
             . addToRoster SlotOutfield pa
             $ emptyRoster
      take 3 (rosterPlayersAt SlotOutfield r) `shouldBe` [pa, pb, pc]
      let r' = removeFromRoster SlotOutfield pb r
      take 2 (rosterPlayersAt SlotOutfield r') `shouldBe` [pa, pc]

    it "rosterContains finds added players and not absent ones" $ do
      let pid = PlayerId 42
          r   = addToRoster SlotShortstop pid emptyRoster
      rosterContains pid emptyRoster `shouldBe` False
      rosterContains pid r           `shouldBe` True

  describe "validateRoster" $ do
    let limits = RosterLimits . Map.fromList $
          [ (SlotCatcher, 1)
          , (SlotFirstBase, 1)
          , (SlotSecondBase, 1)
          , (SlotThirdBase, 1)
          , (SlotShortstop, 1)
          , (SlotOutfield, 3)
          , (SlotUtility, 1)
          , (SlotStartingPitcher, 2)
          , (SlotReliefPitcher, 2)
          ]

    it "accepts a roster within limits with no duplicates" $ do
      let r = addToRoster SlotCatcher (PlayerId 1) emptyRoster
      validateRoster limits r `shouldBe` []

    it "reports too-many at a slot" $ do
      let r = addToRoster SlotCatcher (PlayerId 2)
            . addToRoster SlotCatcher (PlayerId 1)
            $ emptyRoster
      validateRoster limits r `shouldContain` [RosterTooManyAt SlotCatcher 2 1]

    it "reports duplicate players across slots" $ do
      let pid = PlayerId 7
          r   = addToRoster SlotUtility pid
              . addToRoster SlotShortstop pid
              $ emptyRoster
      validateRoster limits r `shouldSatisfy`
        elem (RosterDuplicatePlayer pid)

rosterPlayersAt :: RosterSlot -> Roster -> [PlayerId]
rosterPlayersAt slot = foldMap pure . rosterAt slot