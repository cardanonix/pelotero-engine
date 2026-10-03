module Pelotero.Domain.EligibilitySpec (spec) where

import           Data.Foldable               (toList)
import qualified Data.Map.Strict             as Map
import           Hedgehog                    (Gen, assert, forAll, (===))
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Test.Hspec                  (Spec, describe, it, shouldBe)
import           Test.Hspec.Hedgehog         (hedgehog)

import           Pelotero.Domain.Eligibility
import           Pelotero.Domain.Position    (Position (..), isPitcher)
import           Pelotero.Domain.Roster
                     ( RosterLimits (..)
                     , RosterSlot (..)
                     , allRosterSlots
                     , isPitcherSlot
                     , rosterLimitFor
                     , totalRosterSize
                     )

spec :: Spec
spec = describe "Pelotero.Domain.Eligibility" $ do

  describe "eligibleSlots" $ do
    it "sends pitchers to pitcher slots only and batters to batter slots only" $
      sequence_
        [ all isPitcherSlot (eligibleSlots pos) `shouldBe` isPitcher pos
        | pos <- [minBound .. maxBound]
        ]

    it "lets every batter play utility and no pitcher" $
      sequence_
        [ (pos `isEligibleFor` SlotUtility) `shouldBe` not (isPitcher pos)
        | pos <- [minBound .. maxBound]
        ]

    it "maps all three outfield positions to the outfield slot first" $
      map (take 1 . toList . eligibleSlots) [LeftField, CenterField, RightField]
        `shouldBe` replicate 3 [SlotOutfield]

    it "covers every roster slot with at least one position" $
      sequence_
        [ any (`isEligibleFor` slot) [minBound .. maxBound] `shouldBe` True
        | slot <- allRosterSlots
        ]

  describe "openSlotFor" $ do
    it "returns an eligible slot that still has room" $
      hedgehog $ do
        limits <- forAll genLimits
        counts <- forAll (genCountsWithin limits)
        pos    <- forAll Gen.enumBounded
        case openSlotFor limits counts pos of
          Nothing   -> pure ()
          Just slot -> do
            assert (pos `isEligibleFor` slot)
            assert (slotCount slot counts < rosterLimitFor slot limits)

    it "returns Nothing exactly when every eligible slot is full" $
      hedgehog $ do
        limits <- forAll genLimits
        counts <- forAll (genCountsWithin limits)
        pos    <- forAll Gen.enumBounded
        let allFull = all
              (\slot -> slotCount slot counts >= rosterLimitFor slot limits)
              (eligibleSlots pos)
        (openSlotFor limits counts pos == Nothing) === allFull

    it "prefers the position's own slot over utility" $ do
      let limits = RosterLimits (Map.fromList [(SlotCatcher, 1), (SlotUtility, 1)])
          first  = openSlotFor limits emptySlotCounts Catcher
          second = openSlotFor limits (occupySlot SlotCatcher emptySlotCounts) Catcher
          third  = openSlotFor limits
                     (occupySlot SlotUtility (occupySlot SlotCatcher emptySlotCounts))
                     Catcher
      (first, second, third) `shouldBe` (Just SlotCatcher, Just SlotUtility, Nothing)

    it "keeps a roster within its limits however players arrive" $
      hedgehog $ do
        limits    <- forAll genLimits
        positions <- forAll (Gen.list (Range.linear 0 60) Gen.enumBounded)
        let place counts pos = maybe counts (`occupySlot` counts) (openSlotFor limits counts pos)
            final            = foldl place emptySlotCounts positions
        assert (withinLimits limits final)
        assert (totalOccupied final <= totalRosterSize limits)

genLimits :: Gen RosterLimits
genLimits = do
  counts <- traverse (const (Gen.int (Range.linear 0 3))) allRosterSlots
  pure (RosterLimits (Map.fromList (zip allRosterSlots counts)))

-- | Slot counts that never exceed the given limits.
genCountsWithin :: RosterLimits -> Gen SlotCounts
genCountsWithin limits = do
  taken <- traverse (\slot -> Gen.int (Range.linear 0 (rosterLimitFor slot limits))) allRosterSlots
  pure $ foldr
    (\(slot, n) counts -> iterate (occupySlot slot) counts !! n)
    emptySlotCounts
    (zip allRosterSlots taken)
