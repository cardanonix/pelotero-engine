-- |
-- Module      : Pelotero.Matchup
-- Description : Head-to-head results from team scores.
--
-- Pure. A 'Matchup' always carries the outcome computed from its two
-- scores by 'decideMatchup', and the constructor is only reachable
-- through that function, so a matchup whose outcome disagrees with its
-- scores cannot be built.
module Pelotero.Matchup
  ( Outcome (..)
  , Matchup
  , matchupHome
  , matchupAway
  , matchupOutcome
  , decideMatchup
  , matchupWinner
  , pairOff
  , matchupsFor
  ) where

import           Pelotero.Score (TeamScore (..))

data Outcome
  = HomeWins
  | AwayWins
  | Tied
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Matchup = Matchup
  { matchupHome    :: !TeamScore
  , matchupAway    :: !TeamScore
  , matchupOutcome :: !Outcome
  }
  deriving stock (Show, Eq)

decideMatchup :: TeamScore -> TeamScore -> Matchup
decideMatchup home away = Matchup home away outcome
  where
    outcome = case compare (tsTotalPoints home) (tsTotalPoints away) of
      GT -> HomeWins
      LT -> AwayWins
      EQ -> Tied

-- | The winning side's score, or 'Nothing' for a tie.
matchupWinner :: Matchup -> Maybe TeamScore
matchupWinner m = case matchupOutcome m of
  HomeWins -> Just (matchupHome m)
  AwayWins -> Just (matchupAway m)
  Tied     -> Nothing

-- | Pair consecutive elements. With an odd count the last element has
-- no opponent and is returned separately.
pairOff :: [a] -> ([(a, a)], Maybe a)
pairOff (x : y : rest) = let (pairs, bye) = pairOff rest in ((x, y) : pairs, bye)
pairOff [x]            = ([], Just x)
pairOff []             = ([], Nothing)

-- | Matchups for a list of team scores, pairing them in list order. The
-- second component is the team with a bye, if the count is odd.
matchupsFor :: [TeamScore] -> ([Matchup], Maybe TeamScore)
matchupsFor scores =
  let (pairs, bye) = pairOff scores
  in (map (uncurry decideMatchup) pairs, bye)
