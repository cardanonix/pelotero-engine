{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import           Control.Exception           (bracket)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           System.Exit                 (exitFailure)
import           System.IO                   (hPutStrLn, stderr)

import           Effectful                   (Eff, IOE, (:>))
import qualified Effectful                   as E

import qualified Pelotero.DB.Migration       as Mig
import qualified Pelotero.DB.Pool            as Pool
import           Pelotero.DB.Pool            (renderDBError)
import           Pelotero.DB.Provider        (ProviderName (..))
import           Pelotero.App                (AppEffects, runApp)
import           Pelotero.Effects.Clock      (Clock)
import           Pelotero.Effects.FetchLog   (FetchLog)
import           Pelotero.Effects.Logging
  ( Logging
  , Namespace (..)
  , Severity (..)
  , addNamespace
  , logFM
  , withStdoutLogEnv
  )
import           Pelotero.Effects.MLBClient  (MLBClient, fetchRosters)
import           Pelotero.Effects.Players    (Players)
import           Pelotero.Effects.Teams      (Teams)
import qualified Pelotero.MLB.Convert        as Convert
import           Pelotero.MLB.Fetch          (FetchedRosters (..))
import qualified Pelotero.Sync.Players       as Sync

season :: Int
season = 2025

main :: IO ()
main =
  withStdoutLogEnv
    (Namespace ["pelotero", "fetch-rosters"])
    "production"
    InfoS $ \logEnv -> do
      cfg <- Pool.loadDBConfig

      TIO.putStrLn $ "Connecting to "
        <> Pool.dbHost cfg <> ":" <> tshow (Pool.dbPort cfg)
        <> "/" <> Pool.dbName cfg

      bracket (Pool.acquire cfg) Pool.release $ \pool -> do
        mResult <- Mig.runMigrations pool "db/migrations"
        case mResult of
          Left err -> die ("Migration failed: " <> T.unpack (renderDBError err))
          Right _  -> pure ()

        result <- runApp pool logEnv (work :: Eff AppEffects ())
        case result of
          Left err -> die ("Fatal DB error: " <> T.unpack (renderDBError err))
          Right () -> pure ()
  where
    work
      :: ( Players   :> es
         , Teams     :> es
         , FetchLog  :> es
         , Clock     :> es
         , MLBClient :> es
         , Logging   :> es
         , IOE       :> es
         )
      => Eff es ()
    work = addNamespace (Namespace ["sync"]) $ do
      logFM InfoS $ "fetching rosters for season " <> tshow season
      fetchResult <- fetchRosters season
      case fetchResult of
        Left err -> do
          logFM ErrorS $ "fetch failed: " <> T.pack err
          E.liftIO exitFailure
        Right fetched -> do
          logFM InfoS $ "fetched "
            <> tshow (length (frTeams fetched)) <> " teams, "
            <> tshow (length (frPlayers fetched)) <> " players"

          mapM_ (logFM WarningS . Convert.renderWarning) (frWarnings fetched)

          syncResult <- Sync.syncRosters
            ProviderMLB
            (T.pack (show season))
            (frPayloadSha fetched)
            (frTeams fetched)
            (frPlayers fetched)

          logFM InfoS $ "synced "
            <> tshow (Sync.syncTeamsUpserted syncResult) <> " teams, "
            <> tshow (Sync.syncPlayersUpserted syncResult) <> " players"

tshow :: Show a => a -> T.Text
tshow = T.pack . show

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure