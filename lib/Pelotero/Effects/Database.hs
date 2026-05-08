{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Pelotero.Effects.Database
-- Description : Database transaction effect.
--
-- Each 'runTx' is its own atomic unit. Composing two 'runTx' calls
-- is /not/ one transaction; for multi-step atomicity build a single
-- 'Tx.Transaction' and pass it to one 'runTx'.
--
-- 'runDatabasePool' threads 'DBError' through the typed 'Error'
-- channel and logs the failure at 'ErrorS' before re-raising via
-- 'throwError', so a caller that discards the 'Left' still leaves a
-- record in the log stream.
module Pelotero.Effects.Database
  ( Database
  , runTx
  , runDatabasePool
  ) where

import qualified Hasql.Transaction          as Tx
import           Effectful
import           Effectful.Dispatch.Dynamic
import           Effectful.Error.Static     (Error, throwError)
import qualified Pelotero.DB.Pool           as Pool
import           Pelotero.DB.Pool           (DBError, Pool)
import           Pelotero.Effects.Logging   (Logging, Severity (..), logFM)

data Database :: Effect where
  RunTx :: Tx.Transaction a -> Database m a

type instance DispatchOf Database = Dynamic

runTx :: Database :> es => Tx.Transaction a -> Eff es a
runTx tx = send (RunTx tx)

-- | Production interpreter. On a failed transaction:
--
--  1. logs the rendered error at 'ErrorS', then
--  2. raises the error via 'throwError'.
--
-- The repository runners ('runFetchLogDB', 'runPlayersDB', etc.) do
-- not need to mention 'Logging' or 'Error' 'DBError' in their own
-- signatures; the constraint only appears here, and propagates to
-- the action via the type-level closure of the effect stack.
runDatabasePool
  :: ( IOE :> es
     , Logging :> es
     , Error DBError :> es
     )
  => Pool
  -> Eff (Database : es) a
  -> Eff es a
runDatabasePool pool = interpret $ \_ -> \case
  RunTx tx -> do
    result <- liftIO $ Pool.runTransaction pool tx
    case result of
      Right a  -> pure a
      Left err -> do
        logFM ErrorS (Pool.renderDBError err)
        throwError err