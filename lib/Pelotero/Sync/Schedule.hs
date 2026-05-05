{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Schedule sync pipeline, provider-agnostic.
--
-- Takes already-converted domain 'Game' values plus the payload SHA-256
-- computed by the caller, and upserts them. Team references are resolved
-- to DB surrogate keys via the Teams effect; games referencing unknown
-- teams are skipped (and counted as such in the result).
--
-- Writes one fetch-log entry per call so "have we synced this date range"
-- is queryable without inspecting game rows.
module Pelotero.Sync.Schedule
  ( ScheduleSyncResult(..)
  , syncSchedule
  ) where

import qualified Data.Text       as T
import Data.Time.Clock           (UTCTime)

import Effectful (Eff, (:>))

import Pelotero.DB.FetchLog      (FetchLogRow(..))
import Pelotero.DB.Game          (GameRow(..))
import Pelotero.DB.Provider      (ProviderName)
import Pelotero.Domain.Game      (Game(..))
import Pelotero.Domain.Id        (DbTeamId, TeamId(..), unGameId)

import Pelotero.Effects.Clock    (Clock, now)
import Pelotero.Effects.FetchLog (FetchLog, recordFetch)
import Pelotero.Effects.Games    (Games, upsertGameByExternalId)
import Pelotero.Effects.Teams    (Teams, lookupTeamByExternalId)

--------------------------------------------------------------------------------
-- Result type

data ScheduleSyncResult = ScheduleSyncResult
  { schedGamesUpserted :: !Int
  , schedGamesSkipped  :: !Int
  , schedFetchSha256   :: !T.Text
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Public entry point

syncSchedule
  :: ( Games    :> es
     , Teams    :> es
     , FetchLog :> es
     , Clock    :> es
     )
  => ProviderName
  -> T.Text                  -- ^ scope (e.g. "2025-04-01..2025-04-07")
  -> T.Text                  -- ^ SHA-256 of raw payload, computed by caller
  -> [Game]
  -> Eff es ScheduleSyncResult
syncSchedule provider scope payloadSha games = do
  syncedAt <- now
  results  <- mapM (upsertOneGame provider syncedAt) games

  let upserted = length (filter id results)
      skipped  = length (filter not results)

  recordFetch FetchLogRow
    { fetchLogId            = Nothing
    , fetchLogProvider      = provider
    , fetchLogResource      = "schedule"
    , fetchLogScope         = scope
    , fetchLogFetchedAt     = Nothing
    , fetchLogPayloadSha256 = payloadSha
    , fetchLogRecordCount   = fromIntegral (length games)
    }

  pure ScheduleSyncResult
    { schedGamesUpserted = upserted
    , schedGamesSkipped  = skipped
    , schedFetchSha256   = payloadSha
    }

--------------------------------------------------------------------------------
-- Helpers

upsertOneGame
  :: (Games :> es, Teams :> es)
  => ProviderName
  -> UTCTime
  -> Game
  -> Eff es Bool
upsertOneGame provider syncedAt game = do
  mAway <- resolveTeam provider (gameAwayTeam game)
  mHome <- resolveTeam provider (gameHomeTeam game)
  case (mAway, mHome) of
    (Just dbAway, Just dbHome) -> do
      let extId = T.pack (show (unGameId (gameId game)))
          row   = GameRow
            { gameRowId                 = Nothing
            , gameRowGameDate           = gameDate game
            , gameRowAwayTeamId         = dbAway
            , gameRowHomeTeamId         = dbHome
            , gameRowLastSyncedProvider = Just provider
            , gameRowLastSyncedAt       = Just syncedAt
            }
      _ <- upsertGameByExternalId provider extId row
      pure True
    _ -> pure False

resolveTeam :: Teams :> es => ProviderName -> TeamId -> Eff es (Maybe DbTeamId)
resolveTeam provider (TeamId mlbId) =
  lookupTeamByExternalId provider (T.pack (show mlbId))