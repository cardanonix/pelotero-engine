-- Start of /home/bismuth/git/pelotero-engine/app/DbCheck.hs
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
tshow = T.pack . show-- End of /home/bismuth/git/pelotero-engine/app/DbCheck.hs

-- Start of /home/bismuth/git/pelotero-engine/app/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO as TIO
import qualified Pelotero.Prelude as P

main :: IO ()
main = do
  TIO.putStrLn (P.appName <> " " <> P.appVersion)
  TIO.putStrLn "Phase 0 skeleton. Subcommands TBD."-- End of /home/bismuth/git/pelotero-engine/app/Main.hs

-- Start of /home/bismuth/git/pelotero-engine/app/FetchRoster.hs
-- | Smoke-test CLI: fetch active MLB players and the team list, then
-- upsert both into the database.
--
-- This is intentionally minimal — two HTTP calls, one transaction, exit.
-- The real sync pipeline (Phase 4) will orchestrate multiple endpoints,
-- handle incremental fetches via 'Pelotero.DB.FetchLog' checksums, and
-- run inside an effects tier. This binary exists to prove the
-- wire → domain → repository plumbing actually works end-to-end.
--
-- Architecture:
--
--   * 'getCurrentTime' is captured once in 'main' and threaded through as
--     a pure parameter. No 'unsafePerformIO' inside the 'Tx.Transaction'.
--   * Teams and players are upserted in a single transaction; the in-memory
--     'Map TeamId DbTeamId' built from team upserts is reused to attach
--     players to the right surrogate. Atomicity: either both phases commit
--     or neither does.
--   * The fetch log is recorded inside the same transaction, so a failed
--     sync doesn't leave a misleading "we synced this" record.
--
-- Usage:
--
--   $ pelotero-fetch-roster
--   Fetched 30 teams from MLB.
--   Fetched 1234 players from MLB.
--   Synced 30 teams, 1234 players.
module Main (main) where

import           Control.Exception          (bracket)
import           Control.Monad              (forM)
import qualified Crypto.Hash.SHA256         as SHA256
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Foldable              (traverse_)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import qualified Hasql.Transaction          as Tx
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTPS
import qualified Network.HTTP.Types.Status  as Status
import           System.Exit                (exitFailure)
import           System.IO                  (hPutStrLn, stderr)

import qualified Pelotero.DB.FetchLog as FetchLog
import qualified Pelotero.DB.Player   as Player
import qualified Pelotero.DB.Pool     as Pool
import qualified Pelotero.DB.Team     as Team
import           Pelotero.DB.FetchLog (FetchLogRow(..))
import           Pelotero.DB.Player   (PlayerRow(..))
import           Pelotero.DB.Pool     (Pool)
import           Pelotero.DB.Provider (ProviderName(..))
import           Pelotero.DB.Team     (TeamRow(..))
import           Pelotero.Domain.Id   (DbTeamId, PlayerId(..), TeamId(..))
import qualified Pelotero.Domain.Player   as DPlayer
import qualified Pelotero.Domain.Position as DPos
import qualified Pelotero.MLB.Convert     as Convert
import qualified Pelotero.MLB.Wire.Player as WirePlayer
import qualified Pelotero.MLB.Wire.Team   as WireTeam

--------------------------------------------------------------------------------
-- Endpoints

-- | Hardcoded for this smoke test. Real sync layer will thread season
-- selection from configuration.
season :: Int
season = 2025

teamsUrl :: String
teamsUrl = "https://statsapi.mlb.com/api/v1/teams?sportId=1&season=" <> show season

playersUrl :: String
playersUrl = "https://statsapi.mlb.com/api/v1/sports/1/players?season=" <> show season

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  syncedAt <- getCurrentTime
  cfg <- Pool.loadDBConfig

  -- Fetch first; if the network is broken we'd rather fail before opening
  -- any database connections.
  mgr <- HTTPS.newTlsManager
  teamsBody   <- fetchOrDie mgr teamsUrl
  playersBody <- fetchOrDie mgr playersUrl

  wireTeams <- decodeOrDie "/teams"   teamsBody   :: IO WireTeam.WireTeamEnvelope
  wirePlrs  <- decodeOrDie "/players" playersBody :: IO WirePlayer.WirePlayerEnvelope

  let teams = WireTeam.wireTeams wireTeams
      (warnings, players) = Convert.convertPlayers wirePlrs

  TIO.putStrLn $ "Fetched " <> tshow (length teams)   <> " teams from MLB."
  TIO.putStrLn $ "Fetched " <> tshow (length players) <> " players from MLB."
  traverse_ (TIO.hPutStrLn stderr . Convert.renderWarning) warnings

  -- All DB work in one transaction.
  bracket (Pool.acquire cfg) Pool.release $ \pool -> do
    result <- Pool.runTransaction pool $
      sync syncedAt teams players (LBS.toStrict teamsBody <> LBS.toStrict playersBody)
    case result of
      Left err -> die ("DB error: " <> T.unpack (Pool.renderDBError err))
      Right (nt, np) ->
        TIO.putStrLn $ "Synced " <> tshow nt <> " teams, " <> tshow np <> " players."

--------------------------------------------------------------------------------
-- HTTP

fetchOrDie :: HTTP.Manager -> String -> IO LBS.ByteString
fetchOrDie mgr url = do
  req <- HTTP.parseRequest url
  resp <- HTTP.httpLbs req mgr
  let st = HTTP.responseStatus resp
  if Status.statusIsSuccessful st
    then pure (HTTP.responseBody resp)
    else die ("HTTP " <> show (Status.statusCode st) <> " from " <> url
              <> ": " <> LBS8.unpack (LBS.take 200 (HTTP.responseBody resp)))

decodeOrDie :: Aeson.FromJSON a => String -> LBS.ByteString -> IO a
decodeOrDie label body =
  case Aeson.eitherDecode body of
    Left err -> die ("Failed to parse " <> label <> ": " <> err)
    Right a  -> pure a

--------------------------------------------------------------------------------
-- Sync (the whole thing in one Transaction)

sync
  :: UTCTime
  -> [WireTeam.WireTeam]
  -> [DPlayer.Player]
  -> BS.ByteString          -- ^ raw bytes for the fetch checksum
  -> Tx.Transaction (Int, Int)
sync syncedAt teams players rawBytes = do
  -- 1. Upsert teams. Build the MLB-id → DB-id map from the upsert results
  -- so the player phase doesn't need a second round of lookups.
  teamMap <- fmap Map.fromList . forM teams $ \wt -> do
    let mlbTid = TeamId (WireTeam.wtId wt)
        row = TeamRow
          { teamRowId                 = Nothing
          , teamRowName               = WireTeam.wtName wt
          , teamRowAbbreviation       = WireTeam.wtAbbreviation wt
          , teamRowLocationName       = maybe T.empty id (WireTeam.wtLocationName wt)
          , teamRowLastSyncedProvider = Just ProviderMLB
          , teamRowLastSyncedAt       = Just syncedAt
          }
    dbTid <- Team.upsertByExternalIdT
               ProviderMLB (tshow (WireTeam.wtId wt)) row
    pure (mlbTid, dbTid)

  -- 2. Upsert players, attaching their DB team id from the map.
  traverse_ (upsertPlayer syncedAt teamMap) players

  -- 3. Record the fetch in the same transaction. Resource scope distinguishes
  -- which sync this was; we record one fetch row per resource per sync run.
  -- The SHA-256 covers both payloads concatenated — a coarse signal but
  -- sufficient for "did anything upstream change".
  let sha = TE.decodeUtf8 (B16.encode (SHA256.hash rawBytes))
  FetchLog.recordFetchT FetchLogRow
    { fetchLogId            = Nothing
    , fetchLogProvider      = ProviderMLB
    , fetchLogResource      = "active-rosters"
    , fetchLogScope         = T.pack (show season)
    , fetchLogFetchedAt     = Nothing
    , fetchLogPayloadSha256 = sha
    , fetchLogRecordCount   = fromIntegral (length players)
    }

  pure (length teams, length players)

upsertPlayer
  :: UTCTime
  -> Map.Map TeamId DbTeamId
  -> DPlayer.Player
  -> Tx.Transaction ()
upsertPlayer syncedAt teamMap p = do
  let pid    = DPlayer.playerId p
      dbTeam = DPlayer.playerTeamId p >>= flip Map.lookup teamMap
      row = PlayerRow
        { playerRowId                 = Nothing
        , playerRowFirstName          = DPlayer.playerFirstName p
        , playerRowLastName           = DPlayer.playerLastName  p
        , playerRowNameSlug           = DPlayer.playerNameSlug  p
        , playerRowPosition           = DPos.renderPosition <$> DPlayer.playerPosition p
        , playerRowBatSide            = handToChar <$> DPlayer.playerBatSide p
        , playerRowPitchHand          = handToChar <$> DPlayer.playerPitchHand p
        , playerRowActive             = DPlayer.playerActive p
        , playerRowCurrentTeamId      = dbTeam
        , playerRowLastSyncedProvider = Just ProviderMLB
        , playerRowLastSyncedAt       = Just syncedAt
        }
  _ <- Player.upsertByExternalIdT
         ProviderMLB (tshow (unPlayerId pid)) row
  pure ()
  where
    -- 'renderHandedness' produces "L", "R", or "S" — exactly one character
    -- in every case. The schema CHECK constraint also limits to those three.
    -- 'T.head' is partial in general but provably total here.
    handToChar = T.head . DPlayer.renderHandedness

--------------------------------------------------------------------------------
-- Misc

tshow :: Show a => a -> T.Text
tshow = T.pack . show

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  exitFailure-- End of /home/bismuth/git/pelotero-engine/app/FetchRoster.hs

