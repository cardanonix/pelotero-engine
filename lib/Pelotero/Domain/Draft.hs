-- | Draft data model. Order strategies, picks, rankings, and the pure
-- function that generates a draft order from a strategy + team list. The
-- state machine that runs an actual draft (transitioning between
-- WaitingToStart / Drafting / Complete) is Phase 5 work — it depends on the
-- effects layer because each pick is a database write.
module Pelotero.Domain.Draft
  ( -- * Order strategies
    DraftOrderStrategy(..)
  , parseDraftOrderStrategy
  , renderDraftOrderStrategy
  , generateDraftOrder
    -- * Picks
  , DraftPick(..)
    -- * Rankings
  , Ranking(..)
  , PlayerRanking(..)
  , rankingPlayerIds
  , extendRankingsWithUnranked
  ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Pelotero.Domain.Id
  ( DraftPickNumber(..)
  , FantasyTeamId
  , PlayerId
  , RankSlot
  )

--------------------------------------------------------------------------------
-- Order strategies

-- | How to interleave teams across draft rounds. Sum type rather than the
-- legacy 'Text' dispatch — adding a new strategy is a constructor + a case
-- in 'generateDraftOrder', and the compiler tells you about everywhere
-- that needs updating.
data DraftOrderStrategy
  = -- | Snake/serpentine: round 1 forward, round 2 reverse, round 3 forward...
    SerpentineOrder
  | -- | Forward, mid-shifted, reverse, mid-shifted-reverse, repeating.
    -- (The legacy code's "experimental_snake".)
    ExperimentalSnakeOrder
  deriving stock (Show, Eq, Ord, Enum, Bounded)

parseDraftOrderStrategy :: Text -> Maybe DraftOrderStrategy
parseDraftOrderStrategy = \case
  "serpentine"         -> Just SerpentineOrder
  "experimental_snake" -> Just ExperimentalSnakeOrder
  _                    -> Nothing

renderDraftOrderStrategy :: DraftOrderStrategy -> Text
renderDraftOrderStrategy = \case
  SerpentineOrder        -> "serpentine"
  ExperimentalSnakeOrder -> "experimental_snake"

-- | Generate the full pick-by-pick draft order. Pure: same inputs always
-- produce the same output, which means draft tests don't need a randomness
-- effect. Whatever shuffling the *team order* needs (e.g. a coin flip for
-- who picks first) happens upstream and is passed in already-shuffled.
--
-- The result is the team that picks at each position, paired with that
-- pick's ordinal number (1-indexed). If 'totalPicks' isn't a multiple of
-- 'length teams', the trailing partial round is truncated rather than
-- partially filled.
generateDraftOrder
  :: DraftOrderStrategy
  -> Int                       -- ^ total picks across the entire draft
  -> [FantasyTeamId]           -- ^ teams in initial order
  -> [(FantasyTeamId, DraftPickNumber)]
generateDraftOrder _        _    []    = []
generateDraftOrder strategy total teams =
  let perRound = length teams
      rounds   = total `div` perRound
      ordered  = case strategy of
        SerpentineOrder        -> serpentine rounds teams
        ExperimentalSnakeOrder -> experimentalSnake rounds teams
  in zipWith (\t i -> (t, DraftPickNumber i)) ordered [1 ..]

serpentine :: Int -> [a] -> [a]
serpentine rounds teams =
  concat $ take rounds $ cycle [teams, reverse teams]

-- | Forward → mid-shift → reverse → mid-shift-reverse, on repeat.
-- The "mid-shift" is the team list rotated by half its length.
experimentalSnake :: Int -> [a] -> [a]
experimentalSnake rounds teams =
  let n     = length teams
      half  = n `div` 2
      shift = take n . drop half . cycle
      pat   = [teams, shift teams, reverse teams, shift (reverse teams)]
  in concat $ take rounds $ cycle pat

--------------------------------------------------------------------------------
-- Picks

-- | A single pick made during a draft. 'DraftPickNumber' is the ordinal
-- position in the overall draft order; the team and player are who and what.
-- 'pickedAt' captures when the pick was made for replay/audit.
data DraftPick = DraftPick
  { pickNumber :: !DraftPickNumber
  , pickTeam   :: !FantasyTeamId
  , pickPlayer :: !PlayerId
  , pickedAt   :: !UTCTime
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Rankings

-- | A team's pre-draft player preference list. Whole-list semantics: the
-- list is the ranking, the order is the preference, no global rank state
-- exists outside it. 'PlayerRanking' carries a 'RankSlot' as well so that
-- a partially-completed ranking (rank 1, 2, 5, ...) can still be represented
-- without forcing a contiguous numbering.
data Ranking = Ranking
  { rankingTeam       :: !FantasyTeamId
  , rankingChecksum   :: !Text
  , rankingUpdatedAt  :: !UTCTime
  , rankingEntries    :: ![PlayerRanking]
  }
  deriving stock (Show, Eq)

data PlayerRanking = PlayerRanking
  { prPlayer :: !PlayerId
  , prRank   :: !RankSlot
  }
  deriving stock (Show, Eq)

-- | Just the ranked players, in rank order. Doesn't sort — assumes the
-- 'Ranking' was constructed in rank order, which is the convention the
-- legacy code already enforces.
rankingPlayerIds :: Ranking -> [PlayerId]
rankingPlayerIds = map prPlayer . rankingEntries

-- | Append unranked players to the end of a ranked list, in the order they
-- appear in the universe argument. Used by the auto-drafter when a team's
-- ranking doesn't cover the full pool — they get their ranked picks in
-- order, then fall back to whatever's left.
extendRankingsWithUnranked
  :: [PlayerId]   -- ^ ranked, in rank order
  -> [PlayerId]   -- ^ universe of all draftable players
  -> [PlayerId]
extendRankingsWithUnranked ranked universe =
  ranked ++ filter (`notElem` ranked) universe