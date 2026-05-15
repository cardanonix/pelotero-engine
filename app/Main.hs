{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Single-binary entry point for the engine.
--
-- Subcommands:
--   pelotero db check
--   pelotero sync rosters    --season YEAR
--   pelotero sync schedule   --from DATE --to DATE
--   pelotero sync boxscores  --from DATE --to DATE
--   pelotero snapshot lineups --on-date DATE
--   pelotero score           --league-id ID
--   pelotero draft run       --league-id ID
--
-- All subcommands except @db check@ share the same harness: load the DB
-- config from env, acquire a pool, run pending migrations, open a Katip
-- stdout log env, then dispatch the per-subcommand work in 'AppEffects'.
module Main (main) where

import           Control.Exception              (bracket)
import           Control.Monad                  (forM_)
import           Data.Foldable                  (traverse_)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           Data.Time
                     ( Day
                     , defaultTimeLocale
                     , parseTimeM
                     , showGregorian
                     )

import           Options.Applicative

import           System.Exit                    (exitFailure, exitSuccess)
import           System.IO                      (hPutStrLn, stderr)

import           Effectful                      (Eff)
import qualified Effectful                      as E

import qualified Pelotero.DB.Migration          as Mig
import qualified Pelotero.DB.Pool               as Pool
import           Pelotero.DB.Pool               (renderDBError)
import           Pelotero.DB.Provider           (ProviderName (..))

import           Pelotero.App                   (AppEffects, runApp)
import           Pelotero.Domain.Id             (DbLeagueConfigId (..))
import qualified Pelotero.Domain.Scoring        as Scoring
import qualified Pelotero.Draft                 as Draft
import qualified Pelotero.Draft.Run             as DraftRun
import           Pelotero.Effects.Logging
                     ( Namespace (..)
                     , Severity (..)
                     , addNamespace
                     , logFM
                     , withStdoutLogEnv
                     )
import qualified Pelotero.Effects.MLBClient     as MLB
import qualified Pelotero.Lineup.Snapshot       as Snapshot
import qualified Pelotero.MLB.Convert           as Convert
import           Pelotero.MLB.Fetch
                     ( FetchedRosters (..)
                     , FetchedSchedule (..)
                     )
import qualified Pelotero.Score                 as Score
import qualified Pelotero.Sync.Boxscores        as Box
import qualified Pelotero.Sync.Players          as SyncPlayers
import qualified Pelotero.Sync.Schedule         as SyncSchedule

-- ---------------------------------------------------------------------
-- Command surface
-- ---------------------------------------------------------------------

data Command
  = CmdDbCheck
  | CmdSyncRosters     SyncRostersOpts
  | CmdSyncSchedule    DateRangeOpts
  | CmdSyncBoxscores   DateRangeOpts
  | CmdSnapshotLineups SnapshotLineupsOpts
  | CmdScore           IdOpts
  | CmdDraftRun        IdOpts
  deriving stock (Show)

newtype SyncRostersOpts = SyncRostersOpts { sroSeason :: Int }
  deriving stock (Show)

data DateRangeOpts = DateRangeOpts
  { droFrom :: !Day
  , droTo   :: !Day
  }
  deriving stock (Show)

newtype SnapshotLineupsOpts = SnapshotLineupsOpts { sloOnDate :: Day }
  deriving stock (Show)

newtype IdOpts = IdOpts { ioLeagueId :: DbLeagueConfigId }
  deriving stock (Show)

-- ---------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------

main :: IO ()
main = execParser cliInfo >>= dispatch

dispatch :: Command -> IO ()
dispatch = \case
  CmdDbCheck              -> runDbCheck
  CmdSyncRosters opts     -> runWithApp "sync-rosters"     (workSyncRosters opts)
  CmdSyncSchedule opts    -> runWithApp "sync-schedule"    (workSyncSchedule opts)
  CmdSyncBoxscores opts   -> runWithApp "sync-boxscores"   (workSyncBoxscores opts)
  CmdSnapshotLineups opts -> runWithApp "snapshot-lineups" (workSnapshotLineups opts)
  CmdScore opts           -> runWithApp "score"            (workScore opts)
  CmdDraftRun opts        -> runWithApp "draft-run"        (workDraftRun opts)

-- ---------------------------------------------------------------------
-- Setup harness
-- ---------------------------------------------------------------------

migrationsDir :: FilePath
migrationsDir = "db/migrations"

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

-- | Standard subcommand harness. Acquires a pool, applies pending
-- migrations, opens a stdout log env, then runs the work through
-- 'runApp'. Dies on migration failure or fatal 'DBError'.
runWithApp :: T.Text -> Eff AppEffects () -> IO ()
runWithApp tag work =
  withStdoutLogEnv (Namespace ["pelotero", tag]) "production" InfoS $ \logEnv -> do
    cfg <- Pool.loadDBConfig
    bracket (Pool.acquire cfg) Pool.release $ \pool -> do
      m <- Mig.runMigrations pool migrationsDir
      case m of
        Left err -> die ("Migration failed: " <> T.unpack (renderDBError err))
        Right _  -> pure ()
      result <- runApp pool logEnv work
      case result of
        Left err -> die ("Fatal DB error: " <> T.unpack (renderDBError err))
        Right () -> pure ()

-- ---------------------------------------------------------------------
-- db check
-- ---------------------------------------------------------------------

runDbCheck :: IO ()
runDbCheck = do
  cfg <- Pool.loadDBConfig
  TIO.putStrLn $ "Connecting to "
    <> Pool.dbUser cfg <> "@"
    <> Pool.dbHost cfg <> ":"
    <> tshow (Pool.dbPort cfg) <> "/"
    <> Pool.dbName cfg
  bracket (Pool.acquire cfg) Pool.release $ \pool -> do
    r <- Mig.runMigrations pool migrationsDir
    case r of
      Left err -> do
        TIO.putStrLn $ "FAIL: " <> renderDBError err
        exitFailure
      Right o  -> do
        TIO.putStrLn $ "OK: total="    <> tshow (Mig.migrationsTotalSeen o)
                    <> " applied="     <> tshow (Mig.migrationsAppliedNow o)
                    <> " already="     <> tshow (Mig.migrationsAlreadyApplied o)
        exitSuccess

-- ---------------------------------------------------------------------
-- sync rosters
-- ---------------------------------------------------------------------

workSyncRosters :: SyncRostersOpts -> Eff AppEffects ()
workSyncRosters (SyncRostersOpts season) = addNamespace (Namespace ["sync"]) $ do
  logFM InfoS $ "fetching rosters for season " <> tshow season
  fr <- MLB.fetchRosters season
  case fr of
    Left err -> do
      logFM ErrorS $ "fetch failed: " <> T.pack err
      E.liftIO exitFailure
    Right f  -> do
      logFM InfoS $ "fetched "
        <> tshow (length (frTeams   f)) <> " teams, "
        <> tshow (length (frPlayers f)) <> " players"
      traverse_ (logFM WarningS . Convert.renderWarning) (frWarnings f)
      sr <- SyncPlayers.syncRosters
              ProviderMLB
              (T.pack (show season))
              (frPayloadSha f)
              (frTeams   f)
              (frPlayers f)
      logFM InfoS $ "synced "
        <> tshow (SyncPlayers.syncTeamsUpserted   sr) <> " teams, "
        <> tshow (SyncPlayers.syncPlayersUpserted sr) <> " players"

-- ---------------------------------------------------------------------
-- sync schedule
-- ---------------------------------------------------------------------

workSyncSchedule :: DateRangeOpts -> Eff AppEffects ()
workSyncSchedule (DateRangeOpts fromDate toDate) =
  addNamespace (Namespace ["sync"]) $ do
    let scope = scopeOf fromDate toDate
    logFM InfoS $ "fetching schedule for " <> scope
    sr <- MLB.fetchSchedule (showGregorian fromDate) (showGregorian toDate)
    case sr of
      Left err -> do
        logFM ErrorS $ "fetch failed: " <> T.pack err
        E.liftIO exitFailure
      Right f -> do
        logFM InfoS $ "fetched " <> tshow (length (fsGames f)) <> " games"
        traverse_ (logFM WarningS . Convert.renderWarning) (fsWarnings f)
        result <- SyncSchedule.syncSchedule
                    ProviderMLB
                    scope
                    (fsPayloadSha f)
                    (fsGames f)
        logFM InfoS $ "synced "
          <> tshow (SyncSchedule.schedGamesUpserted result) <> " games"
          <> " (skipped "
          <> tshow (SyncSchedule.schedGamesSkipped result) <> ")"

-- ---------------------------------------------------------------------
-- sync boxscores
-- ---------------------------------------------------------------------

workSyncBoxscores :: DateRangeOpts -> Eff AppEffects ()
workSyncBoxscores (DateRangeOpts fromDate toDate) =
  addNamespace (Namespace ["sync"]) $ do
    logFM InfoS $ "syncing boxscores for " <> scopeOf fromDate toDate
    result <- Box.syncBoxscoresForDateRange ProviderMLB fromDate toDate
    logFM InfoS $ "boxscores: "
      <> tshow (Box.boxGamesProcessed   result) <> " processed, "
      <> tshow (Box.boxGamesUnchanged   result) <> " unchanged, "
      <> tshow (length (Box.boxErrors   result)) <> " errors, "
      <> tshow (Box.boxBattingUpserted  result) <> " batting rows, "
      <> tshow (Box.boxPitchingUpserted result) <> " pitching rows, "
      <> tshow (Box.boxPlayersSkipped   result) <> " players skipped"
    traverse_ (logFM WarningS . Convert.renderWarning)
              (Box.boxConvertWarnings result)
    forM_ (Box.boxErrors result) $ \err ->
      logFM WarningS $ "boxscore error: " <> tshow err

-- ---------------------------------------------------------------------
-- snapshot lineups
-- ---------------------------------------------------------------------

workSnapshotLineups :: SnapshotLineupsOpts -> Eff AppEffects ()
workSnapshotLineups (SnapshotLineupsOpts day) =
  addNamespace (Namespace ["snapshot"]) $ do
    result <- Snapshot.snapshotLineupsForDate day
    logFM InfoS $ "snapshot complete: "
      <> tshow (Snapshot.snapTeamsSnapshotted result) <> " teams snapshotted, "
      <> tshow (Snapshot.snapTeamsAlreadyDone result) <> " already done, "
      <> tshow (Snapshot.snapRowsInserted     result) <> " rows total"

-- ---------------------------------------------------------------------
-- score
-- ---------------------------------------------------------------------

workScore :: IdOpts -> Eff AppEffects ()
workScore (IdOpts lcid) = addNamespace (Namespace ["score"]) $ do
  mLs <- Score.scoreLeague lcid
  case mLs of
    Nothing -> do
      logFM ErrorS $ "league " <> tshow lcid <> " not found"
      E.liftIO exitFailure
    Just ls -> do
      logFM InfoS $ "scored league " <> tshow (Score.lscLeague ls)
                 <> " (" <> tshow (Score.lscPeriodStart ls)
                 <> ".." <> tshow (Score.lscPeriodEnd ls) <> ")"
      forM_ (Score.lscTeams ls) $ \ts ->
        logFM InfoS $ "  team " <> tshow (Score.tsTeam ts)
                   <> ": "      <> renderPoints (Score.tsTotalPoints ts)

renderPoints :: Scoring.Points -> T.Text
renderPoints (Scoring.Points r) =
  T.pack (show (fromRational r :: Double))

-- ---------------------------------------------------------------------
-- draft run
-- ---------------------------------------------------------------------

workDraftRun :: IdOpts -> Eff AppEffects ()
workDraftRun (IdOpts lcid) = addNamespace (Namespace ["draft"]) $ do
  result <- DraftRun.runAutoDraftForLeague lcid
  case result of
    Left err -> do
      logFM ErrorS $ "auto-draft failed: " <> tshow err
      E.liftIO exitFailure
    Right summary -> do
      logFM InfoS $ "draft complete: league "
                 <> tshow (Draft.dsLeague summary) <> " with "
                 <> tshow (length (Draft.dsPicks summary)) <> " picks"

-- ---------------------------------------------------------------------
-- optparse-applicative
-- ---------------------------------------------------------------------

cliInfo :: ParserInfo Command
cliInfo = info (commandP <**> helper)
  ( fullDesc
 <> progDesc "Pelotero fantasy baseball engine"
 <> header "pelotero - sync, snapshot, score, and draft from a single binary"
  )

commandP :: Parser Command
commandP = hsubparser
  ( command "db"       (info dbP       (progDesc "Database operations"))
 <> command "sync"     (info syncP     (progDesc "Sync data from upstream"))
 <> command "snapshot" (info snapshotP (progDesc "Lineup snapshot operations"))
 <> command "score"    (info scoreP    (progDesc "Score a league"))
 <> command "draft"    (info draftP    (progDesc "Draft operations"))
  )

dbP :: Parser Command
dbP = hsubparser
  ( command "check" (info (pure CmdDbCheck)
                          (progDesc "Verify and apply pending migrations"))
  )

syncP :: Parser Command
syncP = hsubparser
  ( command "rosters"
      (info (CmdSyncRosters <$> syncRostersOpts)
            (progDesc "Sync teams and active rosters"))
 <> command "schedule"
      (info (CmdSyncSchedule <$> dateRangeOpts)
            (progDesc "Sync the game schedule for a date range"))
 <> command "boxscores"
      (info (CmdSyncBoxscores <$> dateRangeOpts)
            (progDesc "Sync boxscores for games in a date range"))
  )

snapshotP :: Parser Command
snapshotP = hsubparser
  ( command "lineups"
      (info (CmdSnapshotLineups <$> snapshotLineupsOpts)
            (progDesc "Snapshot active-league lineups for the day's games"))
  )

scoreP :: Parser Command
scoreP = CmdScore <$> idOpts

draftP :: Parser Command
draftP = hsubparser
  ( command "run"
      (info (CmdDraftRun <$> idOpts)
            (progDesc "Run the auto-draft loop for a league"))
  )

syncRostersOpts :: Parser SyncRostersOpts
syncRostersOpts = SyncRostersOpts
  <$> option auto
        ( long "season"
       <> metavar "YEAR"
       <> help "Season year (e.g. 2025)"
        )

dateRangeOpts :: Parser DateRangeOpts
dateRangeOpts = DateRangeOpts
  <$> dayOption (long "from" <> metavar "DATE"
              <> help "Start date, inclusive (YYYY-MM-DD)")
  <*> dayOption (long "to"   <> metavar "DATE"
              <> help "End date, inclusive (YYYY-MM-DD)")

snapshotLineupsOpts :: Parser SnapshotLineupsOpts
snapshotLineupsOpts = SnapshotLineupsOpts
  <$> dayOption (long "on-date" <> metavar "DATE"
              <> help "Day to snapshot (YYYY-MM-DD)")

idOpts :: Parser IdOpts
idOpts = IdOpts
  <$> (DbLeagueConfigId <$> option auto
        ( long "league-id"
       <> metavar "ID"
       <> help "Database id of the league_config row"
        ))

dayOption :: Mod OptionFields Day -> Parser Day
dayOption mods = option (eitherReader parseDay) mods
  where
    parseDay s = case parseTimeM True defaultTimeLocale "%Y-%m-%d" s of
      Just d  -> Right d
      Nothing -> Left ("invalid date '" <> s <> "': expected YYYY-MM-DD")

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

scopeOf :: Day -> Day -> T.Text
scopeOf a b = T.pack (showGregorian a) <> ".." <> T.pack (showGregorian b)

tshow :: Show a => a -> T.Text
tshow = T.pack . show