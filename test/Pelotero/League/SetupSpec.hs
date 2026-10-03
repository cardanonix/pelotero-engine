module Pelotero.League.SetupSpec (spec) where

import           Data.List               (isSubsequenceOf)
import qualified Data.Map.Strict         as Map
import           Data.Text               (Text)
import           Data.Time               (fromGregorian)
import           Hedgehog                (Gen, assert, forAll, (===))
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range
import           Test.Hspec              (Spec, describe, it, shouldBe)
import           Test.Hspec.Hedgehog     (hedgehog)

import           Pelotero.Domain.Draft   (DraftOrderStrategy (..))
import           Pelotero.Domain.Roster
                     ( LineupLimits (..)
                     , RosterLimits (..)
                     , RosterSlot (..)
                     , allRosterSlots
                     , lineupLimitFor
                     , totalLineupSize
                     , totalRosterSize
                     )
import           Pelotero.League.Setup

spec :: Spec
spec = describe "Pelotero.League.Setup" $ do

  describe "standard presets" $ do
    it "the standard league is well formed" $
      validateNewLeague baseLeague `shouldBe` []

    it "the standard roster holds 25 and the standard lineup 15" $ do
      totalRosterSize standardRosterLimits `shouldBe` 25
      totalLineupSize standardLineupLimits `shouldBe` 15

  describe "validateNewLeague" $ do
    it "requires at least two teams" $
      validateNewLeague baseLeague { nlTeams = take 1 (nlTeams baseLeague) }
        `shouldBe` [LeagueNeedsTwoTeams 1]

    it "rejects duplicate team keys" $
      validateNewLeague baseLeague { nlTeams = [newTeam "a", newTeam "b", newTeam "a"] }
        `shouldBe` [DuplicateTeamKey "a"]

    it "rejects a scoring period that ends before it starts" $
      validateNewLeague baseLeague
        { nlScoringStart = fromGregorian 2025 4 7
        , nlScoringEnd   = fromGregorian 2025 4 1
        }
        `shouldBe` [ScoringPeriodInverted (fromGregorian 2025 4 7) (fromGregorian 2025 4 1)]

    it "accepts a one-day scoring period" $
      validateNewLeague baseLeague { nlScoringEnd = nlScoringStart baseLeague }
        `shouldBe` []

    it "rejects a lineup limit above the roster limit for the same slot" $
      validateNewLeague baseLeague
        { nlRosterLimits = RosterLimits (Map.fromList [(SlotCatcher, 1)])
        , nlLineupLimits = LineupLimits (Map.fromList [(SlotCatcher, 2)])
        }
        `shouldBe` [LineupExceedsRoster SlotCatcher 2 1]

    it "rejects a negative roster limit" $
      validateNewLeague baseLeague
        { nlRosterLimits = RosterLimits (Map.fromList [(SlotCatcher, -1)])
        , nlLineupLimits = LineupLimits Map.empty
        }
        `shouldBe` [NegativeRosterLimit SlotCatcher (-1)]

  describe "chooseLineup" $ do
    it "never exceeds a lineup limit" $
      hedgehog $ do
        limits <- forAll genLineupLimits
        roster <- forAll genRoster
        let lineup = chooseLineup limits roster
        assert $ and
          [ length (filter ((== slot) . fst) lineup) <= lineupLimitFor slot limits
          | slot <- allRosterSlots
          ]

    it "returns a sub-list of the roster in the same order" $
      hedgehog $ do
        limits <- forAll genLineupLimits
        roster <- forAll genRoster
        assert (chooseLineup limits roster `isSubsequenceOf` roster)

    it "fills each slot as far as the roster allows" $
      hedgehog $ do
        limits <- forAll genLineupLimits
        roster <- forAll genRoster
        let lineup = chooseLineup limits roster
        sequence_
          [ length (filter ((== slot) . fst) lineup)
              === min (lineupLimitFor slot limits) (length (filter ((== slot) . fst) roster))
          | slot <- allRosterSlots
          ]

    it "keeps the earliest entries of a slot" $
      chooseLineup
        (LineupLimits (Map.fromList [(SlotOutfield, 2)]))
        [(SlotOutfield, 'a'), (SlotCatcher, 'b'), (SlotOutfield, 'c'), (SlotOutfield, 'd')]
        `shouldBe` [(SlotOutfield, 'a'), (SlotOutfield, 'c')]

baseLeague :: NewLeague
baseLeague = NewLeague
  { nlLeagueId     = "setup-spec"
  , nlCommissioner = "spec"
  , nlScoring      = standardScoring
  , nlRosterLimits = standardRosterLimits
  , nlLineupLimits = standardLineupLimits
  , nlStrategy     = SerpentineOrder
  , nlScoringStart = fromGregorian 2025 4 1
  , nlScoringEnd   = fromGregorian 2025 4 7
  , nlTeams        = [newTeam "t1", newTeam "t2"]
  }

newTeam :: Text -> NewTeam
newTeam key = NewTeam
  { ntKey   = key
  , ntName  = "name-" <> key
  , ntOwner = "owner-" <> key
  }

genLineupLimits :: Gen LineupLimits
genLineupLimits = do
  counts <- traverse (const (Gen.int (Range.linear 0 3))) allRosterSlots
  pure (LineupLimits (Map.fromList (zip allRosterSlots counts)))

genRoster :: Gen [(RosterSlot, Int)]
genRoster = do
  slots <- Gen.list (Range.linear 0 30) Gen.enumBounded
  pure (zip slots [1 ..])
