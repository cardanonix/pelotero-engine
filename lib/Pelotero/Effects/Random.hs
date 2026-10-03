{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Pelotero.Effects.Random
-- Description : Seeded randomness as an effect.
--
-- The only interpreter is seeded. Every run that draws random values is
-- reproducible from its seed, which is what makes a randomised draft
-- usable as a test: a failing run can be replayed exactly. Callers that
-- want a fresh run pick a seed themselves (for example from the system
-- entropy source), log it, and pass it to 'runRandomSeeded'.
module Pelotero.Effects.Random
  ( -- * Effect
    Random (..)
  , shuffle
  , uniformInt
    -- * Interpreter
  , runRandomSeeded
    -- * Pure core
  , shuffleWith
  ) where

import           Data.Foldable              (toList)
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Effectful
import           Effectful.Dispatch.Dynamic (reinterpret, send)
import           Effectful.State.Static.Local (State, evalState, state)
import           System.Random              (RandomGen, StdGen, mkStdGen, uniformR)

data Random :: Effect where
  Shuffle    :: [a] -> Random m [a]
  UniformInt :: (Int, Int) -> Random m Int

type instance DispatchOf Random = 'Dynamic

-- | A uniformly random permutation of the list.
shuffle :: Random :> es => [a] -> Eff es [a]
shuffle = send . Shuffle

-- | A uniformly random 'Int' in the inclusive range. The bounds may be
-- given in either order.
uniformInt :: Random :> es => (Int, Int) -> Eff es Int
uniformInt = send . UniformInt

-- | Interpret 'Random' with a generator built from the given seed. The
-- same seed and the same sequence of requests always produce the same
-- values.
runRandomSeeded :: Int -> Eff (Random : es) a -> Eff es a
runRandomSeeded seed = reinterpret (evalState (mkStdGen seed)) $ \_ -> \case
  Shuffle xs       -> draw (shuffleWith xs)
  UniformInt range -> draw (uniformR range)
  where
    draw :: State StdGen :> hs => (StdGen -> (b, StdGen)) -> Eff hs b
    draw = state

-- | Fisher-Yates shuffle over a 'Seq': repeatedly remove a uniformly
-- chosen element from what is left. O(n log n).
shuffleWith :: RandomGen g => [a] -> g -> ([a], g)
shuffleWith xs = go (Seq.fromList xs) []
  where
    go :: RandomGen g => Seq a -> [a] -> g -> ([a], g)
    go remaining acc g
      | Seq.null remaining = (acc, g)
      | otherwise =
          let (i, g') = uniformR (0, Seq.length remaining - 1) g
          in go (Seq.deleteAt i remaining) (toList (Seq.lookup i remaining) ++ acc) g'
