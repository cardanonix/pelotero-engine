-- | Strongly-typed identifiers. Newtypes only — no smart constructors at this
-- layer. Validation (rejecting nonsense like @PlayerId 0@) happens at the wire
-- boundary in "Pelotero.MLB.Convert", or for fantasy-side IDs at creation.
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
    -- * Database surrogate identifiers
  , DbPlayerId(..)
  , DbTeamId(..)
  , DbGameId(..)
  , DbLeagueConfigId(..)
  , DbLeagueTeamId(..)
  , DbDraftPickId(..)
  ) where

import Data.Int  (Int64)
import Data.Text (Text)

newtype PlayerId = PlayerId { unPlayerId :: Int }
  deriving stock (Show, Eq, Ord)

newtype TeamId = TeamId { unTeamId :: Int }
  deriving stock (Show, Eq, Ord)

newtype GameId = GameId { unGameId :: Int }
  deriving stock (Show, Eq, Ord)

newtype SeasonYear = SeasonYear { unSeasonYear :: Int }
  deriving stock (Show, Eq, Ord)

newtype LeagueId = LeagueId { unLeagueId :: Text }
  deriving stock (Show, Eq, Ord)

newtype FantasyTeamId = FantasyTeamId { unFantasyTeamId :: Text }
  deriving stock (Show, Eq, Ord)

newtype DraftPickNumber = DraftPickNumber { unDraftPickNumber :: Int }
  deriving stock (Show, Eq, Ord)

newtype RankSlot = RankSlot { unRankSlot :: Int }
  deriving stock (Show, Eq, Ord)

-- | Surrogate primary key for @player.id@. Distinct from the upstream
-- 'PlayerId' (which carries the MLB-assigned identifier that lives in
-- @player_external_id@). The two should never be confused; if you find
-- yourself coercing between them, you're crossing a layer boundary that
-- needs an explicit lookup, not a cast.
newtype DbPlayerId = DbPlayerId { unDbPlayerId :: Int64 }
  deriving stock (Show, Eq, Ord)

-- | Surrogate primary key for @team.id@.
newtype DbTeamId = DbTeamId { unDbTeamId :: Int64 }
  deriving stock (Show, Eq, Ord)

-- | Surrogate primary key for @game.id@.
newtype DbGameId = DbGameId { unDbGameId :: Int64 }
  deriving stock (Show, Eq, Ord)

-- | Surrogate primary key for @league_config.id@.
newtype DbLeagueConfigId = DbLeagueConfigId { unDbLeagueConfigId :: Int64 }
  deriving stock (Show, Eq, Ord)

-- | Surrogate primary key for @league_team.id@.
newtype DbLeagueTeamId = DbLeagueTeamId { unDbLeagueTeamId :: Int64 }
  deriving stock (Show, Eq, Ord)

-- | Surrogate primary key for @draft_pick.id@.
newtype DbDraftPickId = DbDraftPickId { unDbDraftPickId :: Int64 }
  deriving stock (Show, Eq, Ord)