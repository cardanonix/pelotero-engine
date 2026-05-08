module IntegrationTest.Setup
  ( withTestPool
  , runRolledBack
  , runEffectsOrFail
  , cleanDatabase
  ) where

import Control.Exception        (bracket, throwIO)
import qualified Hasql.Session     as Session
import qualified Hasql.Statement   as Statement
import qualified Hasql.Decoders    as Decoders
import qualified Hasql.Encoders    as Encoders
import qualified Hasql.Transaction as Tx

import           Pelotero.DB.Pool (DBError, Pool)
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

-- | For tests that go through the effect stack: takes the result of
-- a fully-discharged @runEff . runErrorNoCallStack @DBError . ...@
-- chain and turns 'Left' into a thrown exception. Tests then assert
-- against the bare success value.
runEffectsOrFail :: IO (Either DBError a) -> IO a
runEffectsOrFail io = do
  r <- io
  case r of
    Right a  -> pure a
    Left err -> throwIO (userError ("DB error in test: " <> show err))

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