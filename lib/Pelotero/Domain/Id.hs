-- | Strongly-typed identifiers. Newtypes only — no smart constructors at this
-- layer. Validation (rejecting nonsense like @PlayerId 0@) happens at the wire
-- boundary in "Pelotero.MLB.Convert".
module Pelotero.Domain.Id
  ( PlayerId(..)
  , TeamId(..)
  , GameId(..)
  , SeasonYear(..)
  ) where

-- | MLB player ID. The MLB Stats API uses integers (e.g. 660271 for Shohei
-- Ohtani). We keep that representation.
newtype PlayerId = PlayerId { unPlayerId :: Int }
  deriving stock (Show, Eq, Ord)

-- | MLB team ID. Distinct from a fantasy league's team identifier (which we'll
-- introduce later as a different type). The MLB API uses small integers
-- (e.g. 117 for Houston Astros).
newtype TeamId = TeamId { unTeamId :: Int }
  deriving stock (Show, Eq, Ord)

-- | MLB game ID, sometimes called "gamePk" upstream.
newtype GameId = GameId { unGameId :: Int }
  deriving stock (Show, Eq, Ord)

-- | A baseball season, identified by year (e.g. @SeasonYear 2025@).
newtype SeasonYear = SeasonYear { unSeasonYear :: Int }
  deriving stock (Show, Eq, Ord)