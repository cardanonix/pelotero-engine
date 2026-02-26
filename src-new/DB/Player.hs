{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module DB.Player
  ( upsertPlayer
  , upsertPlayers
  , getAllPlayers
  , getPlayerById
  , getPlayersByTeam
  , getPlayersByPosition
  , getActivePlayers
  , logFetch
  , getLastFetch
  ) where

import Control.Monad (void, forM_)
import qualified Data.Pool as Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import DB.Connection (withConnection)
import Types.Player

-- Upsert: insert or update on conflict. This is the key operation --
-- we just blast the full roster in and let Postgres figure out what changed.
upsertPlayer :: Connection -> Player -> IO ()
upsertPlayer conn Player{..} =
  void $ execute conn
    [sql|
      INSERT INTO player
        (player_id, use_name, use_last_name, name_slug,
         current_team, primary_position, bat_side, pitch_hand, active,
         updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, NOW())
      ON CONFLICT (player_id) DO UPDATE SET
        use_name         = EXCLUDED.use_name,
        use_last_name    = EXCLUDED.use_last_name,
        name_slug        = EXCLUDED.name_slug,
        current_team     = EXCLUDED.current_team,
        primary_position = EXCLUDED.primary_position,
        bat_side         = EXCLUDED.bat_side,
        pitch_hand       = EXCLUDED.pitch_hand,
        active           = EXCLUDED.active,
        updated_at       = NOW()
    |]
    ( playerId, useName, useLastName, nameSlug
    , currentTeam, primaryPosition, batSide, pitchHand, active
    )

upsertPlayers :: Pool.Pool Connection -> [Player] -> IO ()
upsertPlayers pool players = withConnection pool $ \conn -> do
  -- Wrap in a transaction for atomicity and speed
  withTransaction conn $
    forM_ players (upsertPlayer conn)

getAllPlayers :: Pool.Pool Connection -> IO [Player]
getAllPlayers pool = withConnection pool $ \conn ->
  query_ conn
    [sql|
      SELECT player_id, use_name, use_last_name, name_slug,
             current_team, primary_position, bat_side, pitch_hand, active
      FROM player
      ORDER BY use_last_name, use_name
    |]

getPlayerById :: Pool.Pool Connection -> Int -> IO (Maybe Player)
getPlayerById pool pid = withConnection pool $ \conn -> do
  results <- query conn
    [sql|
      SELECT player_id, use_name, use_last_name, name_slug,
             current_team, primary_position, bat_side, pitch_hand, active
      FROM player
      WHERE player_id = ?
    |]
    (Only pid)
  pure $ case results of
    [p] -> Just p
    _   -> Nothing

getPlayersByTeam :: Pool.Pool Connection -> Int -> IO [Player]
getPlayersByTeam pool teamId = withConnection pool $ \conn ->
  query conn
    [sql|
      SELECT player_id, use_name, use_last_name, name_slug,
             current_team, primary_position, bat_side, pitch_hand, active
      FROM player
      WHERE current_team = ?
      ORDER BY primary_position, use_last_name
    |]
    (Only teamId)

getPlayersByPosition :: Pool.Pool Connection -> Text -> IO [Player]
getPlayersByPosition pool pos = withConnection pool $ \conn ->
  query conn
    [sql|
      SELECT player_id, use_name, use_last_name, name_slug,
             current_team, primary_position, bat_side, pitch_hand, active
      FROM player
      WHERE primary_position = ?
      ORDER BY use_last_name
    |]
    (Only pos)

getActivePlayers :: Pool.Pool Connection -> IO [Player]
getActivePlayers pool = withConnection pool $ \conn ->
  query_ conn
    [sql|
      SELECT player_id, use_name, use_last_name, name_slug,
             current_team, primary_position, bat_side, pitch_hand, active
      FROM player
      WHERE active = TRUE
      ORDER BY use_last_name, use_name
    |]

logFetch :: Pool.Pool Connection -> Int -> Text -> Int -> IO ()
logFetch pool season checksum count = withConnection pool $ \conn ->
  void $ execute conn
    [sql|
      INSERT INTO player_fetch_log (season, checksum, player_count)
      VALUES (?, ?, ?)
    |]
    (season, checksum, count)

getLastFetch :: Pool.Pool Connection -> Int -> IO (Maybe PlayerFetchRecord)
getLastFetch pool season = withConnection pool $ \conn -> do
  results <- query conn
    [sql|
      SELECT season, fetched_at, checksum, player_count
      FROM player_fetch_log
      WHERE season = ?
      ORDER BY fetched_at DESC
      LIMIT 1
    |]
    (Only season)
  pure $ case results of
    [r] -> Just r
    _   -> Nothing