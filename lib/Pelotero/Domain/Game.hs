-- | Domain representation of a scheduled or completed MLB game.
module Pelotero.Domain.Game
  ( Game(..)
  , GameSchedule(..)
  ) where

import Data.Time.Calendar (Day)

import Pelotero.Domain.Id (GameId, TeamId)

-- | A single scheduled game. We keep only what's needed to drive stat
-- collection: when, who, and (eventually) whether it's final.
data Game = Game
  { gameId       :: GameId
  , gameDate     :: Day
  , gameAwayTeam :: TeamId
  , gameHomeTeam :: TeamId
  }
  deriving stock (Show, Eq)

-- | A day's slate of games. The empty slate (off-day, all-star break) is
-- represented by an empty list — not 'Nothing' — so callers don't have to
-- handle "scheduled but no games".
newtype GameSchedule = GameSchedule
  { unGameSchedule :: [Game] }
  deriving stock (Show, Eq)