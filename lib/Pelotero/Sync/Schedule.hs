{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}

module Pelotero.Sync.Schedule
  ( ScheduleSyncResult (..)
  , syncSchedule
  ) where

import qualified Data.Text                 as T
import           Data.Time.Clock           (UTCTime)

import           Effectful                 (Eff, (:>))

import           Pelotero.DB.FetchLog      (FetchLogRow (..))
import           Pelotero.DB.Game          (GameRow (..))
import           Pelotero.DB.Provider      (ProviderName)
import           Pelotero.Domain.Game      (Game (..))
import           Pelotero.Domain.Id        (DbTeamId, TeamId)

import           Pelotero.Effects.Clock    (Clock, now)
import           Pelotero.Effects.FetchLog (FetchLog, recordFetch)
import           Pelotero.Effects.Games    (Games, upsertGameByExternalId)
import           Pelotero.Effects.Teams    (Teams, lookupTeamByExternalId)

import           Pelotero.Provider.ExternalId
                     ( externalIdFromGameId
                     , externalIdFromTeamId
                     )

data ScheduleSyncResult = ScheduleSyncResult
  { schedGamesUpserted :: !Int
  , schedGamesSkipped  :: !Int
  , schedFetchSha256   :: !T.Text
  }
  deriving stock (Show, Eq)

syncSchedule
  :: ( Games    :> es
     , Teams    :> es
     , FetchLog :> es
     , Clock    :> es
     )
  => ProviderName
  -> T.Text                 -- ^ scope (e.g. "2025-04-01..2025-04-07")
  -> T.Text                 -- ^ SHA-256 of raw payload, computed by caller
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

-- | Upsert a single 'Game'; returns 'False' (counted as skipped) if
-- either side's team has not been ingested yet.
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
      let extId = externalIdFromGameId (gameId game)
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

resolveTeam
  :: Teams :> es
  => ProviderName
  -> TeamId
  -> Eff es (Maybe DbTeamId)
resolveTeam provider tid =
  lookupTeamByExternalId provider (externalIdFromTeamId tid)