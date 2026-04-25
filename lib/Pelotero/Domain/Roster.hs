-- lib/Pelotero/Domain/Roster.hs
-- | Fantasy roster and lineup representation. The key design decision: a
-- @Map RosterSlot (Seq PlayerId)@ instead of one record field per slot. This
-- collapses what used to be a 9-arm case expression on every operation into
-- a single 'Map.adjust' or 'Map.lookup', and makes adding new slot types
-- (Bench, IL, DH-only) a one-line constructor change.
--
-- Why 'Seq' and not 'Set'? Lineups have order — leagues care which outfielder
-- bats third. 'Seq' preserves insertion order and is the right shape for
-- "list of players at this slot" with O(1) cons/snoc and O(log n) lookup.
-- Duplicate detection is explicit ('hasDuplicates'); it's not free, but it's
-- also rare enough that a 'Set' would over-pay structurally.
module Pelotero.Domain.Roster
  ( -- * Slot types
    RosterSlot(..)
  , allRosterSlots
  , parseRosterSlot
  , renderRosterSlot
  , isPitcherSlot
  , isBatterSlot
    -- * Rosters and lineups
  , Roster(..)
  , Lineup(..)
  , emptyRoster
  , emptyLineup
  , rosterAt
  , lineupAt
  , addToRoster
  , addToLineup
  , removeFromRoster
  , removeFromLineup
  , rosterPlayers
  , lineupPlayers
  , rosterContains
  , lineupContains
  , countAt
  , countAtLineup
    -- * Limits
  , RosterLimits(..)
  , LineupLimits(..)
  , rosterLimitFor
  , lineupLimitFor
  , totalRosterSize
  , totalLineupSize
    -- * Validation
  , RosterError(..)
  , LineupError(..)
  , validateRoster
  , validateLineup
  ) where

import Data.Foldable (toList)
import Data.List (group, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Maybe (mapMaybe)

import Pelotero.Domain.Id (PlayerId)

--------------------------------------------------------------------------------
-- Slot types

-- | A roster/lineup slot. We split pitchers into starting and relief because
-- most fantasy formats do, and we keep 'Utility' as its own slot rather than
-- conflating it with a flexible position marker — utility players are a
-- specific role, not a wildcard.
--
-- Adding a new slot here (Bench, IL, DH) is the only place it needs to be
-- listed. 'allRosterSlots' is derived via 'Bounded'/'Enum'.
data RosterSlot
  = SlotCatcher
  | SlotFirstBase
  | SlotSecondBase
  | SlotThirdBase
  | SlotShortstop
  | SlotOutfield
  | SlotUtility
  | SlotStartingPitcher
  | SlotReliefPitcher
  deriving stock (Show, Eq, Ord, Enum, Bounded)

allRosterSlots :: [RosterSlot]
allRosterSlots = [minBound .. maxBound]

-- | Parse a slot from one of the legacy text codes (\"catcher\", \"first\",
-- \"s_pitcher\", ...). Used at the JSON boundary; pure code should never need
-- this.
parseRosterSlot :: Text -> Maybe RosterSlot
parseRosterSlot = \case
  "catcher"   -> Just SlotCatcher
  "first"     -> Just SlotFirstBase
  "second"    -> Just SlotSecondBase
  "third"     -> Just SlotThirdBase
  "shortstop" -> Just SlotShortstop
  "outfield"  -> Just SlotOutfield
  "utility"   -> Just SlotUtility
  "s_pitcher" -> Just SlotStartingPitcher
  "r_pitcher" -> Just SlotReliefPitcher
  _           -> Nothing

renderRosterSlot :: RosterSlot -> Text
renderRosterSlot = \case
  SlotCatcher         -> "catcher"
  SlotFirstBase       -> "first"
  SlotSecondBase      -> "second"
  SlotThirdBase       -> "third"
  SlotShortstop       -> "shortstop"
  SlotOutfield        -> "outfield"
  SlotUtility         -> "utility"
  SlotStartingPitcher -> "s_pitcher"
  SlotReliefPitcher   -> "r_pitcher"

isPitcherSlot :: RosterSlot -> Bool
isPitcherSlot = \case
  SlotStartingPitcher -> True
  SlotReliefPitcher   -> True
  _                   -> False

isBatterSlot :: RosterSlot -> Bool
isBatterSlot = not . isPitcherSlot

--------------------------------------------------------------------------------
-- Rosters and lineups

-- | A team's full draft roster. Distinct from 'Lineup' — a team carries more
-- players on the roster than they can field in any one game. A roster
-- always has every 'RosterSlot' as a key (possibly with an empty 'Seq');
-- this invariant lets callers do 'rosterAt' without a 'Maybe' wrapper.
newtype Roster = Roster { unRoster :: Map RosterSlot (Seq PlayerId) }
  deriving stock (Show, Eq)

-- | A team's active lineup for a scoring period. Same shape as 'Roster' but
-- semantically distinct, so we keep the types separate to prevent accidental
-- swaps. Adding/removing in either case goes through the same helpers
-- parameterised over the underlying map.
newtype Lineup = Lineup { unLineup :: Map RosterSlot (Seq PlayerId) }
  deriving stock (Show, Eq)

-- | A roster with all slots present and empty. Use this rather than
-- 'Roster Map.empty' so the all-slots-present invariant holds from the start.
emptyRoster :: Roster
emptyRoster = Roster $ Map.fromList [(s, Seq.empty) | s <- allRosterSlots]

emptyLineup :: Lineup
emptyLineup = Lineup $ Map.fromList [(s, Seq.empty) | s <- allRosterSlots]

-- | Players at a given slot. Returns 'Seq.empty' if the slot is unpopulated;
-- never returns a missing-key 'Maybe' because 'emptyRoster' / 'addToRoster'
-- maintain the invariant that all slots exist as keys.
rosterAt :: RosterSlot -> Roster -> Seq PlayerId
rosterAt slot (Roster m) = Map.findWithDefault Seq.empty slot m

lineupAt :: RosterSlot -> Lineup -> Seq PlayerId
lineupAt slot (Lineup m) = Map.findWithDefault Seq.empty slot m

addToRoster :: RosterSlot -> PlayerId -> Roster -> Roster
addToRoster slot pid (Roster m) =
  Roster (Map.insertWith (\_new old -> old Seq.|> pid) slot (Seq.singleton pid) m)

addToLineup :: RosterSlot -> PlayerId -> Lineup -> Lineup
addToLineup slot pid (Lineup m) =
  Lineup (Map.insertWith (\_new old -> old Seq.|> pid) slot (Seq.singleton pid) m)

-- | Remove the first occurrence of a player from a slot. No-op if the player
-- isn't there. Removing only the first occurrence is intentional: if a player
-- somehow ended up in the same slot twice, removing both at once would mask
-- the bug.
removeFromRoster :: RosterSlot -> PlayerId -> Roster -> Roster
removeFromRoster slot pid (Roster m) = Roster (Map.adjust (seqRemoveFirst pid) slot m)

removeFromLineup :: RosterSlot -> PlayerId -> Lineup -> Lineup
removeFromLineup slot pid (Lineup m) = Lineup (Map.adjust (seqRemoveFirst pid) slot m)

seqRemoveFirst :: Eq a => a -> Seq a -> Seq a
seqRemoveFirst x s = case Seq.breakl (== x) s of
  (before, rest) -> case Seq.viewl rest of
    Seq.EmptyL    -> before
    _ Seq.:< after -> before <> after

rosterPlayers :: Roster -> [PlayerId]
rosterPlayers = concatMap toList . Map.elems . unRoster

lineupPlayers :: Lineup -> [PlayerId]
lineupPlayers = concatMap toList . Map.elems . unLineup

rosterContains :: PlayerId -> Roster -> Bool
rosterContains pid = elem pid . rosterPlayers

lineupContains :: PlayerId -> Lineup -> Bool
lineupContains pid = elem pid . lineupPlayers

countAt :: RosterSlot -> Roster -> Int
countAt slot = Seq.length . rosterAt slot

countAtLineup :: RosterSlot -> Lineup -> Int
countAtLineup slot = Seq.length . lineupAt slot

--------------------------------------------------------------------------------
-- Limits

-- | Roster size limits per slot. Like 'Roster' itself, this is keyed on
-- 'RosterSlot' so adding a new slot doesn't ripple through field names.
newtype RosterLimits = RosterLimits { unRosterLimits :: Map RosterSlot Int }
  deriving stock (Show, Eq)

newtype LineupLimits = LineupLimits { unLineupLimits :: Map RosterSlot Int }
  deriving stock (Show, Eq)

rosterLimitFor :: RosterSlot -> RosterLimits -> Int
rosterLimitFor slot (RosterLimits m) = Map.findWithDefault 0 slot m

lineupLimitFor :: RosterSlot -> LineupLimits -> Int
lineupLimitFor slot (LineupLimits m) = Map.findWithDefault 0 slot m

totalRosterSize :: RosterLimits -> Int
totalRosterSize = sum . Map.elems . unRosterLimits

totalLineupSize :: LineupLimits -> Int
totalLineupSize = sum . Map.elems . unLineupLimits

--------------------------------------------------------------------------------
-- Validation

-- | Things that can be wrong with a 'Roster'. Slot-specific rather than a
-- bag of strings, so callers can match on cause.
data RosterError
  = -- | Too many players at this slot. @TooManyAt slot actual limit@.
    RosterTooManyAt !RosterSlot !Int !Int
  | -- | Player listed more than once across the roster.
    RosterDuplicatePlayer !PlayerId
  deriving stock (Show, Eq)

data LineupError
  = LineupTooManyAt !RosterSlot !Int !Int
  | LineupDuplicatePlayer !PlayerId
  deriving stock (Show, Eq)

-- | Validate a roster against its limits. Returns the empty list on success,
-- a list of every distinct problem otherwise. We intentionally return all
-- problems instead of stopping at the first; users want to fix all the
-- mistakes in one pass, not play whack-a-mole.
validateRoster :: RosterLimits -> Roster -> [RosterError]
validateRoster limits roster = sizeProblems <> dupProblems
  where
    sizeProblems =
      [ RosterTooManyAt slot actual lim
      | slot <- allRosterSlots
      , let actual = countAt slot roster
            lim    = rosterLimitFor slot limits
      , actual > lim
      ]
    dupProblems = map RosterDuplicatePlayer (duplicates (rosterPlayers roster))

validateLineup :: LineupLimits -> Lineup -> [LineupError]
validateLineup limits lineup = sizeProblems <> dupProblems
  where
    sizeProblems =
      [ LineupTooManyAt slot actual lim
      | slot <- allRosterSlots
      , let actual = countAtLineup slot lineup
            lim    = lineupLimitFor slot limits
      , actual > lim
      ]
    dupProblems = map LineupDuplicatePlayer (duplicates (lineupPlayers lineup))

duplicates :: Ord a => [a] -> [a]
duplicates = mapMaybe firstOfRepeat . group . sort
  where
    firstOfRepeat (x : _ : _) = Just x
    firstOfRepeat _           = Nothing