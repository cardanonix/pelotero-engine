-- lib/Pelotero/Domain/Roster.hs

module Pelotero.Domain.Roster
  (
    RosterSlot(..)
  , allRosterSlots
  , parseRosterSlot
  , renderRosterSlot
  , isPitcherSlot
  , isBatterSlot

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

  , RosterLimits(..)
  , LineupLimits(..)
  , rosterLimitFor
  , lineupLimitFor
  , totalRosterSize
  , totalLineupSize

  , RosterError(..)
  , LineupError(..)
  , validateRoster
  , validateLineup
  ) where

import           Data.Aeson         (FromJSON(..), ToJSON(..))
import qualified Data.Aeson.Types   as Aeson
import           Data.Foldable      (toList)
import           Data.List          (group, sort)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (mapMaybe)
import           Data.Sequence      (Seq)
import qualified Data.Sequence      as Seq
import           Data.Text          (Text)
import qualified Data.Text          as T

import Pelotero.Domain.Id (PlayerId)

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

newtype Roster = Roster { unRoster :: Map RosterSlot (Seq PlayerId) }
  deriving stock (Show, Eq)

newtype Lineup = Lineup { unLineup :: Map RosterSlot (Seq PlayerId) }
  deriving stock (Show, Eq)

emptyRoster :: Roster
emptyRoster = Roster $ Map.fromList [(s, Seq.empty) | s <- allRosterSlots]

emptyLineup :: Lineup
emptyLineup = Lineup $ Map.fromList [(s, Seq.empty) | s <- allRosterSlots]

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

data RosterError
  = RosterTooManyAt !RosterSlot !Int !Int
  | RosterDuplicatePlayer !PlayerId
  deriving stock (Show, Eq)

data LineupError
  = LineupTooManyAt !RosterSlot !Int !Int
  | LineupDuplicatePlayer !PlayerId
  deriving stock (Show, Eq)

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

instance ToJSON RosterLimits where
  toJSON (RosterLimits m) = toJSON (Map.mapKeys renderRosterSlot m)

instance FromJSON RosterLimits where
  parseJSON v = do
    raw    <- parseJSON v :: Aeson.Parser (Map.Map Text Int)
    parsed <- Map.fromList <$> traverse parsePair (Map.toList raw)
    pure (RosterLimits parsed)
    where
      parsePair (k, n) = case parseRosterSlot k of
        Just slot -> pure (slot, n)
        Nothing   -> fail ("RosterLimits: unknown slot key " <> T.unpack k)

instance ToJSON LineupLimits where
  toJSON (LineupLimits m) = toJSON (Map.mapKeys renderRosterSlot m)

instance FromJSON LineupLimits where
  parseJSON v = do
    raw    <- parseJSON v :: Aeson.Parser (Map.Map Text Int)
    parsed <- Map.fromList <$> traverse parsePair (Map.toList raw)
    pure (LineupLimits parsed)
    where
      parsePair (k, n) = case parseRosterSlot k of
        Just slot -> pure (slot, n)
        Nothing   -> fail ("LineupLimits: unknown slot key " <> T.unpack k)