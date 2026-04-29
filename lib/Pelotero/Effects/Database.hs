{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

-- | Database effect.
--
-- One operation: 'runTx', which runs a 'Tx.Transaction' against the
-- database. Each call is one independent transaction acquired from the
-- pool: writes commit on success, roll back on error or thrown exception.
-- This matches the semantics of 'Pelotero.DB.Pool.runTransaction' exactly.
module Pelotero.Effects.Database
  ( Database(..)
  , runTx
  , runDatabasePool
  ) where

import qualified Hasql.Transaction as Tx

import           Effectful (Effect, IOE, Dispatch(Dynamic), DispatchOf)
import qualified Effectful as E
import           Effectful.Dispatch.Dynamic (interpret_, send)

import           Pelotero.DB.Pool (Pool, DBError(..))
import qualified Pelotero.DB.Pool as Pool

data Database :: Effect where
  RunTx :: Tx.Transaction a -> Database m a

type instance DispatchOf Database = 'Dynamic

runTx :: Database E.:> es => Tx.Transaction a -> E.Eff es a
runTx = send . RunTx

runDatabasePool
  :: IOE E.:> es
  => Pool
  -> E.Eff (Database : es) a
  -> E.Eff es a
runDatabasePool pool = interpret_ $ \case
  RunTx tx -> runOrThrow (Pool.runTransaction pool tx)

runOrThrow :: IOE E.:> es' => IO (Either DBError a) -> E.Eff es' a
runOrThrow io = E.liftIO io >>= \case
  Right a  -> pure a
  Left err -> E.liftIO (ioError (userError ("DB error: " <> show err)))