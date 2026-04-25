-- | Shared setup for integration tests.
--
-- Every test runs its work inside a transaction that is unconditionally
-- rolled back via 'Tx.condemn', so nothing commits to the database.
-- Tests insert data freely without coordinating cleanup; rows vanish
-- when the test ends, regardless of whether assertions passed.
module IntegrationTest.Setup
  ( withTestPool
  , runRolledBack
  ) where

import Control.Exception        (bracket, throwIO)
import qualified Hasql.Transaction as Tx

import           Pelotero.DB.Pool (Pool)
import qualified Pelotero.DB.Pool as DBPool

-- | Acquire a pool from libpq env vars, share it across all tests in a run.
withTestPool :: (Pool -> IO a) -> IO a
withTestPool action = do
  cfg <- DBPool.loadDBConfig
  bracket (DBPool.acquire cfg) DBPool.release action

-- | Run a 'Tx.Transaction' against the pool with an unconditional rollback
-- at the end. The result of the transaction is returned; the rollback
-- happens regardless of success or failure.
--
-- Throws on any pool error so the test body can use @do@-notation freely
-- and let HSpec attribute the failure correctly.
runRolledBack :: Pool -> Tx.Transaction a -> IO a
runRolledBack pool tx = do
  r <- DBPool.runTransaction pool $ do
    a <- tx
    Tx.condemn  -- mark for rollback; commit is suppressed
    pure a
  case r of
    Right a  -> pure a
    Left err -> throwIO (userError ("DB error in test: " <> show err))