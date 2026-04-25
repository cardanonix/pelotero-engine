-- | A complete fantasy league configuration, plus the team type that lives
-- inside it. This is the integration point that ties scoring, roster limits,
-- and draft parameters together; everything else in @Pelotero.Domain@ is a
-- piece of this whole.
module Pelotero.Domain.League
  ( -- * Leagues
    League(..)
  , LeagueStatus(..)
  , parseLeagueStatus
  , renderLeagueStatus
    -- * Teams
  , FantasyTeam(..)
    -- * Scoring period
  , ScoringPeriod(..)
    -- * Draft parameters
  , DraftParameters(..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)

import Pelotero.Domain.Draft (DraftOrderStrategy)
import Pelotero.Domain.Id
  ( FantasyTeamId
  , LeagueId
  , PlayerId
  )
import Pelotero.Domain.Roster
  ( LineupLimits
  , Lineup
  , Roster
  , RosterLimits
  )
import Pelotero.Domain.Scoring (LeagueScoring)

--------------------------------------------------------------------------------
-- Leagues

-- | Where a league is in its lifecycle. Drives UI affordances (can you edit
-- the lineup yet?) and validation (e.g. lineup edits rejected after period
-- close). Sum type rather than 'Text' for the same reasons as
-- 'DraftOrderStrategy'.
data LeagueStatus
  = LeagueDraft       -- ^ pre-draft / drafting
  | LeagueActive      -- ^ regular play
  | LeagueClosed      -- ^ scoring period ended
  deriving stock (Show, Eq, Ord, Enum, Bounded)

parseLeagueStatus :: Text -> Maybe LeagueStatus
parseLeagueStatus = \case
  "draft"  -> Just LeagueDraft
  "active" -> Just LeagueActive
  "closed" -> Just LeagueClosed
  _        -> Nothing

renderLeagueStatus :: LeagueStatus -> Text
renderLeagueStatus = \case
  LeagueDraft  -> "draft"
  LeagueActive -> "active"
  LeagueClosed -> "closed"

-- | A complete league configuration. The only stateful field is 'leagueStatus';
-- everything else is the immutable definition of what playing in this league
-- means. Per-team rosters and lineups live in 'FantasyTeam', not here.
data League = League
  { leagueId           :: !LeagueId
  , leagueCommissioner :: !Text
  , leagueStatus       :: !LeagueStatus
  , leagueScoring      :: !LeagueScoring
  , leagueRosterLimits :: !RosterLimits
  , leagueLineupLimits :: !LineupLimits
  , leagueDraftParams  :: !DraftParameters
  , leagueScoringPeriod :: !ScoringPeriod
  , leagueTeams        :: ![FantasyTeamId]
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Teams

-- | A single fantasy team within a league. Carries its current roster and
-- lineup. Mutating either of those goes through the helpers in
-- "Pelotero.Domain.Roster"; this record is just the bag.
data FantasyTeam = FantasyTeam
  { ftId      :: !FantasyTeamId
  , ftLeague  :: !LeagueId
  , ftName    :: !Text
  , ftOwner   :: !Text
  , ftRoster  :: !Roster
  , ftLineup  :: !Lineup
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Scoring period

-- | The window during which boxscore stats count toward this league. We use
-- 'UTCTime' rather than 'Day' because real leagues care about local game
-- start times relative to lineup-lock cutoffs, and the wire layer already
-- gives us instants.
data ScoringPeriod = ScoringPeriod
  { spStart :: !UTCTime
  , spEnd   :: !UTCTime
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Draft parameters

-- | Configuration for a league's draft. Whether to auto-draft, when, in what
-- order, and what each team's roster looks like at the end. The roster
-- limits used for the draft are the same as the league's 'leagueRosterLimits'
-- — we don't model "different limits during draft" because no real league we
-- support does that.
data DraftParameters = DraftParameters
  { dpAutoDraft   :: !Bool
  , dpStrategy    :: !DraftOrderStrategy
  , dpAutoDraftAt :: !UTCTime
  }
  deriving stock (Show, Eq)