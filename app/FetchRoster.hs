module Main (main) where

import Control.Exception     (bracket)
import Data.Foldable         (traverse_)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import System.Exit           (exitFailure)
import System.IO             (hPutStrLn, stderr)

import           Effectful   (runEff)
import qualified Effectful   as E

import qualified Pelotero.DB.Migration       as Mig
import qualified Pelotero.DB.Pool            as Pool
import           Pelotero.DB.Provider        (ProviderName(..))
import           Pelotero.Effects.Clock      (runClockIO)
import           Pelotero.Effects.Database   (runDatabasePool)
import           Pelotero.Effects.FetchLog   (runFetchLogDB)
import           Pelotero.Effects.MLBClient
  (fetchRosters, runMLBClientHTTP)
import           Pelotero.Effects.Players    (runPlayersDB)
import           Pelotero.Effects.Teams      (runTeamsDB)
import qualified Pelotero.MLB.Convert        as Convert
import           Pelotero.MLB.Fetch          (FetchedRosters(..))
import qualified Pelotero.Sync.Players       as Sync

season :: Int
season = 2025

main :: IO ()
main = do
  cfg <- Pool.loadDBConfig
  TIO.putStrLn $ "Connecting to "
    <> Pool.dbHost cfg <> ":" <> tshow (Pool.dbPort cfg)
    <> "/" <> Pool.dbName cfg

  bracket (Pool.acquire cfg) Pool.release $ \pool -> do
    mResult <- Mig.runMigrations pool "db/migrations"
    case mResult of
      Left err -> die ("Migration failed: " <> T.unpack (Pool.renderDBError err))
      Right _  -> pure ()

    TIO.putStrLn "Fetching rosters from MLB..."

    runEff
      . runClockIO
      . runDatabasePool pool
      . runFetchLogDB
      . runTeamsDB
      . runPlayersDB
      . runMLBClientHTTP
      $ do
          fetchResult <- fetchRosters season
          case fetchResult of
            Left err -> E.liftIO (die err)
            Right fetched -> do
              E.liftIO $ TIO.putStrLn $ "Fetched "
                <> tshow (length (frTeams fetched))
                <> " teams, "
                <> tshow (length (frPlayers fetched))
                <> " players."
              E.liftIO $ traverse_
                (TIO.hPutStrLn stderr . Convert.renderWarning)
                (frWarnings fetched)

              syncResult <- Sync.syncRosters
                ProviderMLB
                (T.pack (show season))
                (frPayloadSha fetched)
                (frTeams fetched)
                (frPlayers fetched)

              E.liftIO $ TIO.putStrLn $ "Synced "
                <> tshow (Sync.syncTeamsUpserted syncResult)
                <> " teams, "
                <> tshow (Sync.syncPlayersUpserted syncResult)
                <> " players."

tshow :: Show a => a -> T.Text
tshow = T.pack . show

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure