{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Crypto.Hash (SHA256(..), hashWith)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)

import DB.Connection (defaultConfig, initializeDB)
import DB.Migration (runMigrations)
import DB.Player (upsertPlayers, logFetch, getLastFetch, getAllPlayers)
import MLB.Client (fetchActiveRoster, MLBError(..))
import Types.Player (Player, PlayerFetchRecord(..))

main :: IO ()
main = do
  args <- getArgs
  season <- case args of
    (s:_) -> case readMaybe s of
      Just yr -> pure yr
      Nothing -> do
        hPutStrLn stderr "Usage: fetch-rosters <season-year>"
        exitFailure
    _ -> do
      hPutStrLn stderr "Usage: fetch-rosters <season-year>"
      exitFailure

  -- DB setup
  config <- defaultConfig
  pool <- initializeDB config
  runMigrations pool

  -- Check if we already fetched recently
  lastFetch <- getLastFetch pool season
  case lastFetch of
    Just record -> do
      hPutStrLn stderr $ "Last fetch for season " ++ show season
        ++ ": " ++ show (playerCount record) ++ " players"
        ++ " at " ++ show (fetchedAt record)
      hPutStrLn stderr "Proceeding with fresh fetch..."
    Nothing ->
      hPutStrLn stderr $ "No previous fetch for season " ++ show season

  -- Fetch from MLB
  result <- fetchActiveRoster season
  case result of
    Left err -> do
      hPutStrLn stderr $ "MLB API error: " ++ show err
      exitFailure
    Right players -> do
      hPutStrLn stderr $ "Got " ++ show (length players) ++ " players from MLB"

      -- Compute checksum of the response
      let checksum = computeChecksum players

      -- Check if data actually changed
      let unchanged = case lastFetch of
            Just prev -> fetchChecksum prev == checksum
            Nothing   -> False

      if unchanged
        then
          hPutStrLn stderr "Data unchanged since last fetch, skipping upsert."
        else do
          -- Upsert all players
          upsertPlayers pool players
          hPutStrLn stderr $ "Upserted " ++ show (length players) ++ " players"

      -- Log the fetch either way
      logFetch pool season checksum (length players)
      hPutStrLn stderr "Fetch logged. Done."

computeChecksum :: [Player] -> Text
computeChecksum players =
  let bs = BL.toStrict $ encode players
      hash = hashWith SHA256 bs
  in T.pack (show hash)