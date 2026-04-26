{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}


-- | The 'Clock' effect: getting the current time without baking 'IO' in.
--
-- Sync code that wants a "synced at" timestamp uses 'now'; tests can run
-- the same code through 'runClockFixed' to get a deterministic value.
-- This is a dynamic effect because we genuinely have multiple
-- interpretations (real wall clock vs. fixed-time test).
module Pelotero.Effects.Clock
  ( Clock(..)
  , now
  , runClockIO
  , runClockFixed
  ) where

import Data.Time (UTCTime, getCurrentTime)

import Effectful (Effect, IOE, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import Effectful.Dispatch.Dynamic (interpret_, send)

-- | "Read the wall clock."
data Clock :: Effect where
  Now :: Clock m UTCTime

type instance DispatchOf Clock = 'Dynamic

now :: Clock E.:> es => E.Eff es UTCTime
now = send Now

-- | Real clock interpreter.
runClockIO :: IOE E.:> es => E.Eff (Clock : es) a -> E.Eff es a
runClockIO = interpret_ $ \case
  Now -> E.liftIO getCurrentTime

-- | Fixed-time interpreter for tests. Every call to 'now' returns the
-- same value.
runClockFixed :: UTCTime -> E.Eff (Clock : es) a -> E.Eff es a
runClockFixed t = interpret_ $ \case
  Now -> pure t