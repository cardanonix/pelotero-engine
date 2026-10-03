-- |
-- Module      : Pelotero.Draft.Feasibility
-- Description : Whether the picks left in a draft can all be placed on rosters.
--
-- A draft can finish only if every remaining pick can put some available
-- player into an open slot on the picking team's roster, with no player
-- used twice. 'placeablePicks' computes the largest number of remaining
-- picks that can be placed at the same time. The draft can finish
-- exactly when that number equals the number of picks left.
--
-- The question is a bipartite assignment with capacities, solved as a
-- maximum flow:
--
-- > source --(players of position p)--> p
-- > p      --(open slots s on team t)--> (t, s)   when p is eligible for s
-- > (t, s) --(open slots s on team t)--> t
-- > t      --(picks t still makes)-----> sink
--
-- The answer is exact for the draft state machine. The machine places a
-- player in the first eligible slot with room ('openSlotFor'), not in the
-- slot an assignment would choose, and this never removes an option:
-- pitchers fit 'SlotStartingPitcher' and 'SlotReliefPitcher' equally, and
-- every batter a position slot accepts is also accepted by
-- 'SlotUtility', so an assignment that uses the utility slot for a player
-- can trade places with one that uses his position slot.
--
-- Players of the same position are interchangeable here, so the graph
-- has one node per position rather than one per player and its size
-- does not grow with the pool. Teams that must fill every open slot are
-- pooled per slot (see 'flowNetwork'), so in a normal draft it does not
-- grow with the number of teams either. Augmenting along shortest paths
-- (Edmonds-Karp) needs a number of augmentations bounded by the size of
-- the graph, whatever the capacities, so the cost depends on the graph
-- and not on how many players the pool holds.
module Pelotero.Draft.Feasibility
  ( TeamNeed (..)
  , placeablePicks
  ) where

import qualified Data.Foldable               as F
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Sequence               (Seq (..))
import qualified Data.Sequence               as Seq
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           Pelotero.Domain.Eligibility (SlotCounts, eligibleSlots, slotCount)
import           Pelotero.Domain.Position    (Position)
import           Pelotero.Domain.Roster
                     ( RosterLimits
                     , RosterSlot
                     , allRosterSlots
                     , rosterLimitFor
                     )

-- | One team's part of the rest of a draft.
data TeamNeed = TeamNeed
  { tnCounts :: !SlotCounts
    -- ^ How full the team's roster already is.
  , tnPicks  :: !Int
    -- ^ How many picks the team still makes.
  }
  deriving stock (Show, Eq)

-- | The largest number of the remaining picks that can all be placed at
-- once, given the roster limits, how many available players hold each
-- position, and each team's roster and remaining picks. It never
-- exceeds the sum of 'tnPicks'.
placeablePicks :: RosterLimits -> Map Position Int -> [TeamNeed] -> Int
placeablePicks limits supply needs = maxFlow (flowNetwork limits supply needs)

-- ---------------------------------------------------------------------
-- Network
-- ---------------------------------------------------------------------

data Node
  = Source
  | Sink
  | PositionNode !Position
  | PooledSlotNode !RosterSlot
    -- ^ One slot across every team that must fill all its open slots.
  | SlotNode     !Int !RosterSlot
    -- ^ Team index, slot, for a team that fills only some open slots.
  | TeamNode     !Int
  deriving stock (Show, Eq, Ord)

-- | Residual capacities and the neighbours of every node in either
-- direction.
data Network = Network
  { nwResidual  :: !(Map (Node, Node) Int)
  , nwNeighbors :: !(Map Node (Set Node))
  }

-- | Build the network. A team whose remaining picks equal its open
-- slots must fill every one of them, so its own team node constrains
-- nothing, and all such teams share one node per slot whose capacity is
-- their combined open slots. In a normal draft every team is like this
-- until the end, which keeps the network at one node per position and
-- one per slot however many teams the league has. A team with fewer
-- picks than open slots chooses which slots to fill and keeps its own
-- nodes.
flowNetwork :: RosterLimits -> Map Position Int -> [TeamNeed] -> Network
flowNetwork limits supply needs = F.foldl' addEdge emptyNetwork edges
  where
    emptyNetwork   = Network Map.empty Map.empty
    open need slot = max 0 (rosterLimitFor slot limits - slotCount slot (tnCounts need))
    totalOpen need = sum (map (open need) allRosterSlots)
    fillsAll need  = tnPicks need == totalOpen need
    pooledNeeds    = filter fillsAll needs
    ownNeeds       = zip [0 :: Int ..] (filter (not . fillsAll) needs)
    pooledOpen slot = sum (map (`open` slot) pooledNeeds)
    edges = filter (\(_, _, c) -> c > 0) $ concat
      [ [ (Source, PositionNode pos, n) | (pos, n) <- Map.toList supply ]
      , [ (PositionNode pos, PooledSlotNode slot, pooledOpen slot)
        | pos <- Map.keys supply
        , slot <- F.toList (eligibleSlots pos)
        ]
      , [ (PooledSlotNode slot, Sink, pooledOpen slot) | slot <- allRosterSlots ]
      , [ (PositionNode pos, SlotNode i slot, open need slot)
        | pos <- Map.keys supply
        , (i, need) <- ownNeeds
        , slot <- F.toList (eligibleSlots pos)
        ]
      , [ (SlotNode i slot, TeamNode i, open need slot)
        | (i, need) <- ownNeeds
        , slot <- allRosterSlots
        ]
      , [ (TeamNode i, Sink, tnPicks need) | (i, need) <- ownNeeds ]
      ]

-- | Add a forward edge with its capacity and register both directions
-- as neighbours, so flow pushed forward can later be pushed back.
addEdge :: Network -> (Node, Node, Int) -> Network
addEdge (Network residual neighbors) (u, v, c) = Network
  { nwResidual  = Map.insertWith (+) (u, v) c residual
  , nwNeighbors = Map.insertWith Set.union u (Set.singleton v)
                $ Map.insertWith Set.union v (Set.singleton u) neighbors
  }

-- ---------------------------------------------------------------------
-- Maximum flow (Edmonds-Karp)
-- ---------------------------------------------------------------------

maxFlow :: Network -> Int
maxFlow = go 0
  where
    go total network = case shortestAugmentingPath network of
      Nothing   -> total
      Just path ->
        let bottleneck = minimum (map (capacity network) path)
        in go (total + bottleneck) (F.foldl' (push bottleneck) network path)

    push amount network (u, v) = network
      { nwResidual = Map.insertWith (+) (v, u) amount
                   $ Map.adjust (subtract amount) (u, v) (nwResidual network)
      }

capacity :: Network -> (Node, Node) -> Int
capacity network edge = Map.findWithDefault 0 edge (nwResidual network)

-- | A shortest source-to-sink path through edges with residual capacity
-- left, as a non-empty list of edges, or 'Nothing' when the sink cannot
-- be reached.
shortestAugmentingPath :: Network -> Maybe [(Node, Node)]
shortestAugmentingPath network = search (Seq.singleton Source) (Map.singleton Source Source)
  where
    search Empty _ = Nothing
    search (u :<| queue) parents
      | u == Sink = pathTo Sink parents []
      | otherwise =
          let next = [ v
                     | v <- Set.toList (Map.findWithDefault Set.empty u (nwNeighbors network))
                     , Map.notMember v parents
                     , capacity network (u, v) > 0
                     ]
              parents' = F.foldl' (\m v -> Map.insert v u m) parents next
          in search (queue <> Seq.fromList next) parents'

    pathTo v parents acc
      | v == Source = if null acc then Nothing else Just acc
      | otherwise   = case Map.lookup v parents of
          Just u  -> pathTo u parents ((u, v) : acc)
          Nothing -> Nothing
