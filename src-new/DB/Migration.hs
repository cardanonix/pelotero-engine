{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB.Migration
  ( runMigrations
  ) where

import qualified Data.Pool as Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import DB.Connection (withConnection)
import System.IO (hPutStrLn, stderr)

runMigrations :: Pool.Pool Connection -> IO ()
runMigrations pool = withConnection pool $ \conn -> do
  hPutStrLn stderr "Running migrations..."

  _ <- execute_ conn
    [sql|
      CREATE TABLE IF NOT EXISTS player (
        player_id        INTEGER PRIMARY KEY,
        use_name         TEXT NOT NULL,
        use_last_name    TEXT NOT NULL,
        name_slug        TEXT NOT NULL,
        current_team     INTEGER NOT NULL,
        primary_position TEXT NOT NULL,
        bat_side         TEXT NOT NULL,
        pitch_hand       TEXT NOT NULL,
        active           BOOLEAN NOT NULL,
        created_at       TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
        updated_at       TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
      )
    |]

  _ <- execute_ conn
    [sql|
      CREATE TABLE IF NOT EXISTS player_fetch_log (
        id             SERIAL PRIMARY KEY,
        season         INTEGER NOT NULL,
        fetched_at     TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
        checksum       TEXT NOT NULL,
        player_count   INTEGER NOT NULL
      )
    |]

  -- We'll add more tables here as we go:
  -- game_schedule, game_stats, batting_stats, pitching_stats,
  -- league_config, team, roster, lineup, player_ranking, etc.

  hPutStrLn stderr "Migrations complete."