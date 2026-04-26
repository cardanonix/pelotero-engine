{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | The 'DbPool' effect: provides a 'Pool' to other interpreters that
-- need DB access.
--
-- This is a *static* effect because the pool is a fixed environment value
-- — it doesn't need dynamic dispatch and the static representation is
-- cheaper. Other DB-backed interpreters (like 'runPlayersDB') depend on
-- @DbPool :> es@ and pull the pool out via 'getPool'.
--
-- Note: there is no in-memory variant of 'DbPool'. In-memory interpreters
-- of capability effects (Players, Teams, etc.) don't go through 'DbPool'
-- at all — they're shaped differently. That's intentional.
module Pelotero.Effects.DbPool
  ( DbPool
  , runDbPool
  , getPool
  ) where

import Effectful (Effect, IOE, Dispatch(Static), DispatchOf)
import Effectful.Dispatch.Static
  ( SideEffects(WithSideEffects)
  , StaticRep
  , evalStaticRep
  , getStaticRep
  )
import qualified Effectful as E

import Pelotero.DB.Pool (Pool)

-- | Effect carrying a database connection pool.
data DbPool :: Effect

type instance DispatchOf DbPool = 'Static 'WithSideEffects

-- | The pool lives in the static rep. We don't expose the constructor;
-- callers go through 'getPool'.
data instance StaticRep DbPool = DbPoolRep !Pool

-- | Provide a pool for the duration of an effectful computation.
runDbPool
  :: IOE E.:> es
  => Pool
  -> E.Eff (DbPool : es) a
  -> E.Eff es a
runDbPool pool = evalStaticRep (DbPoolRep pool)

-- | Retrieve the pool from the environment.
getPool :: DbPool E.:> es => E.Eff es Pool
getPool = do
  DbPoolRep pool <- getStaticRep
  pure pool