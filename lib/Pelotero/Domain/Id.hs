-- | Strongly-typed identifiers. Newtypes only — no smart constructors at this
-- layer. Validation (rejecting nonsense like @PlayerId 0@) happens at the wire
-- boundary in "Pelotero.MLB.Convert", or for fantasy-side IDs at the time of
-- creation.
module Pelotero.Domain.Id
  ( -- * MLB identifiers
    PlayerId(..)
  , TeamId(..)
  , GameId(..)
  , SeasonYear(..)
    -- * Fantasy identifiers
  , LeagueId(..)
  , FantasyTeamId(..)
    -- * Draft identifiers
  , DraftPickNumber(..)
  , RankSlot(..)
  ) where

import Data.Text (Text)

-- | MLB player ID (e.g. 660271 for Shohei Ohtani).
newtype PlayerId = PlayerId { unPlayerId :: Int }
  deriving stock (Show, Eq, Ord)

-- | MLB team ID (e.g. 117 for Houston Astros). Distinct from a fantasy
-- league's team identifier — see 'FantasyTeamId'.
newtype TeamId = TeamId { unTeamId :: Int }
  deriving stock (Show, Eq, Ord)

-- | MLB game ID, sometimes called \"gamePk\" upstream.
newtype GameId = GameId { unGameId :: Int }
  deriving stock (Show, Eq, Ord)

-- | A baseball season, identified by year (e.g. @SeasonYear 2025@).
newtype SeasonYear = SeasonYear { unSeasonYear :: Int }
  deriving stock (Show, Eq, Ord)

-- | A fantasy league identifier. Opaque 'Text' rather than 'Int' because
-- fantasy leagues are user-created and we want non-guessable, non-collidable
-- identifiers (UUIDs, ULIDs, slugs — caller's choice). The legacy code used
-- a SHA256-derived hex string; that fits here unchanged.
newtype LeagueId = LeagueId { unLeagueId :: Text }
  deriving stock (Show, Eq, Ord)

-- | A fantasy team within a league. Opaque 'Text' for the same reason as
-- 'LeagueId': two different leagues can each have a team called \"Team 1\";
-- only the underlying ID is unique. The legacy code generated these as
-- random SHA256 hex strings; we'll likely move to ULIDs but that's an
-- implementation detail this type doesn't care about.
newtype FantasyTeamId = FantasyTeamId { unFantasyTeamId :: Text }
  deriving stock (Show, Eq, Ord)

-- | A pick's ordinal position within a draft (1, 2, 3, ...). Distinct from
-- both round number and overall position so we can change order strategies
-- (snake, linear, etc.) without renumbering anything.
newtype DraftPickNumber = DraftPickNumber { unDraftPickNumber :: Int }
  deriving stock (Show, Eq, Ord)

-- | A player's rank within a single fantasy team's pre-draft preference
-- list. Lower is better (rank 1 = top pick).
newtype RankSlot = RankSlot { unRankSlot :: Int }
  deriving stock (Show, Eq, Ord)