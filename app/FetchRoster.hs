-- | CLI: fetch active MLB rosters and sync to the database via the
-- effects-based pipeline.
module Main (main) where

import Control.Exception     (bracket)
import Data.Foldable         (traverse_)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import System.Exit           (exitFailure)
import System.IO             (hPutStrLn, stderr)

import qualified Effectful      as E
import Effectful              (runEff)

import qualified Pelotero.DB.Migration as Mig
import qualified Pelotero.DB.Pool      as Pool
import           Pelotero.DB.Provider  (ProviderName(..))
import           Pelotero.Effects.Clock    (runClockIO)
import           Pelotero.Effects.DbPool   (runDbPool)
import           Pelotero.Effects.FetchLog (runFetchLogDB)
import           Pelotero.Effects.Players  (runPlayersDB)
import           Pelotero.Effects.Teams    (runTeamsDB)
import qualified Pelotero.MLB.Convert  as Convert
import qualified Pelotero.MLB.Fetch    as Fetch
import qualified Pelotero.Sync.Players as Sync

season :: Int
season = 2025

main :: IO ()
main = do
  cfg <- Pool.loadDBConfig
  TIO.putStrLn $ "Connecting to "
    <> Pool.dbHost cfg <> ":" <> tshow (Pool.dbPort cfg)
    <> "/" <> Pool.dbName cfg

  bracket (Pool.acquire cfg) Pool.release $ \pool -> do
    -- Run migrations
    mResult <- Mig.runMigrations pool "db/migrations"
    case mResult of
      Left err -> die ("Migration failed: " <> T.unpack (Pool.renderDBError err))
      Right _  -> pure ()

    -- Fetch from MLB (IO, network)
    TIO.putStrLn "Fetching rosters from MLB..."
    fetchResult <- Fetch.fetchRosters season
    case fetchResult of
      Left err -> die err
      Right fetched -> do
        TIO.putStrLn $ "Fetched " <> tshow (length (Fetch.frTeams fetched))
          <> " teams, " <> tshow (length (Fetch.frPlayers fetched)) <> " players."
        traverse_ (TIO.hPutStrLn stderr . Convert.renderWarning) (Fetch.frWarnings fetched)

        -- Sync via effects (DB writes)
        syncResult <- runEff
          . runClockIO
          . runDbPool pool
          . runFetchLogDB
          . runTeamsDB
          . runPlayersDB
          $ Sync.syncRosters
              ProviderMLB
              (T.pack (show season))
              (Fetch.frPayloadSha fetched)
              (Fetch.frTeams fetched)
              (Fetch.frPlayers fetched)

        TIO.putStrLn $ "Synced "
          <> tshow (Sync.syncTeamsUpserted syncResult) <> " teams, "
          <> tshow (Sync.syncPlayersUpserted syncResult) <> " players."

tshow :: Show a => a -> T.Text
tshow = T.pack . show

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure