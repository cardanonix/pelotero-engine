module IntegrationTest.Setup
  ( withTestPool
  , runRolledBack
  , runEffectsOrFail
  , cleanDatabase
  ) where

import           Control.Exception        (bracket, throwIO)
import qualified Hasql.Decoders           as Decoders
import qualified Hasql.Encoders           as Encoders
import qualified Hasql.Session            as Session
import qualified Hasql.Statement          as Statement
import qualified Hasql.Transaction        as Tx

import           Pelotero.DB.Pool         (DBError, Pool)
import qualified Pelotero.DB.Pool         as DBPool

-- | Acquire a connection pool for the duration of an action and release
-- it afterwards. Intended to be used with hspec's 'aroundAll' so that
-- one pool serves the entire suite (per-test isolation comes from
-- 'runRolledBack', not from per-test pool acquisition).
withTestPool :: (Pool -> IO a) -> IO a
withTestPool action = do
  cfg <- DBPool.loadDBConfig
  bracket (DBPool.acquire cfg) DBPool.release action

-- | Run a Transaction and roll it back regardless of outcome.
-- Each test starts with a clean slate without paying for a fresh pool.
runRolledBack :: Pool -> Tx.Transaction a -> IO a
runRolledBack pool tx = do
  r <- DBPool.runTransaction pool $ do
    a <- tx
    Tx.condemn
    pure a
  case r of
    Right a  -> pure a
    Left err -> throwIO (userError ("DB error in test: " <> show err))

runEffectsOrFail :: IO (Either DBError a) -> IO a
runEffectsOrFail io = do
  r <- io
  case r of
    Right a  -> pure a
    Left err -> throwIO (userError ("DB error in test: " <> show err))

-- | TRUNCATE every table the integration suite touches. Use sparingly
-- -- the rolled-back transaction pattern in 'runRolledBack' is cheaper
-- and gives you isolation for free. This is for tests that genuinely
-- need to commit (e.g. effect interpreter tests that run multiple
-- transactions).
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
      , "lineup_snapshot, "
      , "provider_fetch_log, "
      , "league_config, "
      , "league_team, "
      , "roster_slot, "
      , "lineup_slot, "
      , "player_ranking, "
      , "draft_pick "
      , "RESTART IDENTITY CASCADE"
      ]