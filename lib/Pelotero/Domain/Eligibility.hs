-- |
-- Module      : Pelotero.Domain.Eligibility
-- Description : Which roster slots a position may fill, and where a new player lands.
--
-- A roster is described during a draft by how many players it holds in
-- each slot ('SlotCounts'). 'openSlotFor' is the single placement rule
-- used everywhere a player joins a roster: try the position's eligible
-- slots in preference order and take the first one that still has room.
--
-- Every batter has at most one position-specific slot plus 'SlotUtility',
-- and the specific slot is always tried first. Taking the specific slot
-- first never costs a team anything, because 'SlotUtility' accepts every
-- batter that the specific slot accepts. Pitchers go to
-- 'SlotStartingPitcher' first and then 'SlotReliefPitcher'; the MLB
-- roster feed reports both kinds as position @P@, so the engine cannot
-- tell them apart until a richer data source exists.
module Pelotero.Domain.Eligibility
  ( -- * Position to slot mapping
    eligibleSlots
  , isEligibleFor
    -- * Slot occupancy
  , SlotCounts
  , emptySlotCounts
  , slotCount
  , totalOccupied
  , occupySlot
  , slotCountsToList
    -- * Placement
  , openSlotFor
  , withinLimits
  ) where

import           Data.Foldable           (find)
import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map

import           Pelotero.Domain.Position (Position (..))
import           Pelotero.Domain.Roster
                     ( RosterLimits
                     , RosterSlot (..)
                     , rosterLimitFor
                     )

-- | The slots a position may occupy, most specific first.
eligibleSlots :: Position -> NonEmpty RosterSlot
eligibleSlots = \case
  Pitcher          -> SlotStartingPitcher :| [SlotReliefPitcher]
  Catcher          -> SlotCatcher         :| [SlotUtility]
  FirstBase        -> SlotFirstBase       :| [SlotUtility]
  SecondBase       -> SlotSecondBase      :| [SlotUtility]
  ThirdBase        -> SlotThirdBase       :| [SlotUtility]
  Shortstop        -> SlotShortstop       :| [SlotUtility]
  LeftField        -> SlotOutfield        :| [SlotUtility]
  CenterField      -> SlotOutfield        :| [SlotUtility]
  RightField       -> SlotOutfield        :| [SlotUtility]
  DesignatedHitter -> SlotUtility         :| []

isEligibleFor :: Position -> RosterSlot -> Bool
isEligibleFor pos slot = slot `elem` eligibleSlots pos

-- | How many players a roster currently holds in each slot. The
-- constructor is not exported: counts start at zero and only grow by
-- one through 'occupySlot', so a negative count cannot be built.
newtype SlotCounts = SlotCounts (Map RosterSlot Int)
  deriving stock (Show, Eq)

emptySlotCounts :: SlotCounts
emptySlotCounts = SlotCounts Map.empty

slotCount :: RosterSlot -> SlotCounts -> Int
slotCount slot (SlotCounts m) = Map.findWithDefault 0 slot m

totalOccupied :: SlotCounts -> Int
totalOccupied (SlotCounts m) = sum (Map.elems m)

occupySlot :: RosterSlot -> SlotCounts -> SlotCounts
occupySlot slot (SlotCounts m) = SlotCounts (Map.insertWith (+) slot 1 m)

-- | Occupied slots only, in 'RosterSlot' order.
slotCountsToList :: SlotCounts -> [(RosterSlot, Int)]
slotCountsToList (SlotCounts m) = Map.toAscList m

-- | The slot a player of this position takes on a roster with these
-- counts, or 'Nothing' when every eligible slot is full.
openSlotFor :: RosterLimits -> SlotCounts -> Position -> Maybe RosterSlot
openSlotFor limits counts = find hasRoom . eligibleSlots
  where
    hasRoom slot = slotCount slot counts < rosterLimitFor slot limits

-- | Whether no slot holds more players than the limits allow.
withinLimits :: RosterLimits -> SlotCounts -> Bool
withinLimits limits counts =
  all (\(slot, n) -> n <= rosterLimitFor slot limits) (slotCountsToList counts)
