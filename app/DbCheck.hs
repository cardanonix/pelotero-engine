-- | Standalone health check: load DB config from the environment, acquire
-- a pool, run migrations, exit non-zero on any failure.
module Main (main) where

import Control.Exception (bracket)
import Data.Text         (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.Exit       (exitFailure, exitSuccess)

import qualified Pelotero.DB.Pool      as Pool
import qualified Pelotero.DB.Migration as Mig

migrationsDir :: FilePath
migrationsDir = "db/migrations"

main :: IO ()
main = do
  cfg <- Pool.loadDBConfig
  TIO.putStrLn $ "Connecting to "
    <> Pool.dbUser cfg <> "@"
    <> Pool.dbHost cfg <> ":"
    <> tshow (Pool.dbPort cfg) <> "/"
    <> Pool.dbName cfg

  bracket (Pool.acquire cfg) Pool.release $ \pool -> do
    result <- Mig.runMigrations pool migrationsDir
    case result of
      Left err -> do
        TIO.putStrLn $ "FAIL: " <> Pool.renderDBError err
        exitFailure
      Right o -> do
        TIO.putStrLn $ "OK: total="     <> tshow (Mig.migrationsTotalSeen o)
                    <> " applied="      <> tshow (Mig.migrationsAppliedNow o)
                    <> " already="      <> tshow (Mig.migrationsAlreadyApplied o)
        exitSuccess

tshow :: Show a => a -> Text
tshow = T.pack . show