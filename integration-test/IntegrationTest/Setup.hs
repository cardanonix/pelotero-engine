-- | Shared setup for integration tests.
--
-- Two isolation strategies live here:
--
--   * 'runRolledBack' — for tests that talk to repos directly via
--     'Tx.Transaction'. Wraps the work in a single transaction that is
--     unconditionally rolled back via 'Tx.condemn'. Nothing commits.
--
--   * 'cleanDatabase' — for tests that commit (because they go through
--     the effect interpreters, which run each operation as its own
--     transaction). Truncates the tables those tests touch so each run
--     starts from a known state. Tests must call this before doing
--     anything else.
module IntegrationTest.Setup
  ( withTestPool
  , runRolledBack
  , cleanDatabase
  ) where

import Control.Exception        (bracket, throwIO)
import qualified Hasql.Session     as Session
import qualified Hasql.Statement   as Statement
import qualified Hasql.Decoders    as Decoders
import qualified Hasql.Encoders    as Encoders
import qualified Hasql.Transaction as Tx

import           Pelotero.DB.Pool (Pool)
import qualified Pelotero.DB.Pool as DBPool

withTestPool :: (Pool -> IO a) -> IO a
withTestPool action = do
  cfg <- DBPool.loadDBConfig
  bracket (DBPool.acquire cfg) DBPool.release action

runRolledBack :: Pool -> Tx.Transaction a -> IO a
runRolledBack pool tx = do
  r <- DBPool.runTransaction pool $ do
    a <- tx
    Tx.condemn
    pure a
  case r of
    Right a  -> pure a
    Left err -> throwIO (userError ("DB error in test: " <> show err))

-- | Truncate every table that effect-using tests might touch, so each
-- run starts from a known empty state. Identity sequences reset so DB
-- ids are stable across runs (mostly cosmetic; tests look up by
-- external id, not sequence-allocated id).
--
-- CASCADE is required because most of these tables have FK references
-- to each other.
cleanDatabase :: Pool -> IO ()
cleanDatabase pool = do
  r <- DBPool.runSession pool $ Session.statement () truncateStmt
  case r of
    Right () -> pure ()
    Left err -> throwIO (userError ("cleanDatabase failed: " <> show err))
  where
    truncateStmt = Statement.Statement
      sql Encoders.noParams Decoders.noResult True
    sql = mconcat
      [ "TRUNCATE TABLE "
      , "team, "
      , "team_external_id, "
      , "player, "
      , "player_external_id, "
      , "game, "
      , "game_external_id, "
      , "game_player_batting, "
      , "game_player_pitching, "
      , "provider_fetch_log, "
      , "league_config, "
      , "league_team, "
      , "roster_slot, "
      , "lineup_slot, "
      , "player_ranking, "
      , "draft_pick "
      , "RESTART IDENTITY CASCADE"
      ]