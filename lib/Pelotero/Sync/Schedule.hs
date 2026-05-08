{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Pelotero.Sync.Schedule
  ( ScheduleSyncResult (..)
  , syncSchedule
  , upsertOneGame
  , resolveTeam
  , logConvertWarnings
  ) where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Effectful
import Katip (Severity (..))

import Pelotero.DB.FetchLog (FetchLogRow (..))
import Pelotero.DB.Game (GameRow (..))
import Pelotero.DB.Provider (ProviderName)
import Pelotero.Domain.Game (Game (..))
import Pelotero.Domain.Id (DbTeamId, TeamId)
import Pelotero.Effects.Clock (Clock)
import qualified Pelotero.Effects.Clock as Clock
import Pelotero.Effects.FetchLog (FetchLog)
import qualified Pelotero.Effects.FetchLog as FetchLog
import Pelotero.Effects.Games (Games)
import qualified Pelotero.Effects.Games as Games
import Pelotero.Effects.Logging (Logging, logFM)
import Pelotero.Effects.Teams (Teams)
import qualified Pelotero.Effects.Teams as Teams
import qualified Pelotero.MLB.Convert as Convert
import Pelotero.Provider.ExternalId (externalIdFromGameId, externalIdFromTeamId)

-- | Summary returned from a schedule sync run.
--
-- In the SHA short-circuit path (Phase C.1), all counts are zero: the
-- inbound payload matched the most recent fetch for this scope and no
-- rows were touched.
data ScheduleSyncResult = ScheduleSyncResult
  { schedGamesUpserted :: !Int
  , schedGamesSkipped  :: !Int
  , schedFetchSha256   :: !T.Text
  }
  deriving stock (Show, Eq)

scheduleResource :: T.Text
scheduleResource = "schedule"

-- | Provider-agnostic upsert pipeline for already-converted games.
-- Idempotent by SHA: if 'payloadSha' equals the most recent recorded
-- SHA for ('provider', 'scheduleResource', 'scope'), the call returns
-- immediately with a zero-row 'ScheduleSyncResult' and a single InfoS
-- log line.
syncSchedule
  :: ( Games    :> es
     , Teams    :> es
     , FetchLog :> es
     , Clock    :> es
     , Logging  :> es
     )
  => ProviderName
  -> T.Text                 -- ^ scope (e.g. "2025-04-01..2025-04-07")
  -> T.Text                 -- ^ SHA-256 of raw payload, computed by caller
  -> [Game]
  -> Eff es ScheduleSyncResult
syncSchedule provider scope payloadSha games = do
  prior <- FetchLog.getLastFetch provider scheduleResource scope
  case prior of
    Just FetchLogRow { fetchLogPayloadSha256 = oldSha }
      | oldSha == payloadSha -> do
          logFM InfoS $
            "schedule: payload unchanged, skipping; sha=" <> payloadSha
          pure ScheduleSyncResult
            { schedGamesUpserted = 0
            , schedGamesSkipped  = 0
            , schedFetchSha256   = payloadSha
            }
    _ -> do
      syncedAt <- Clock.now
      results  <- mapM (upsertOneGame provider syncedAt) games
      let upserted = length (filter id results)
          skipped  = length games - upserted
      FetchLog.recordFetch FetchLogRow
        { fetchLogId            = Nothing
        , fetchLogProvider      = provider
        , fetchLogResource      = scheduleResource
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

-- | Upsert one game; returns True on success, False if either team is
-- not in the local DB (in which case the game is skipped entirely).
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
    (Just awayDb, Just homeDb) -> do
      let extId = externalIdFromGameId (gameId game)
          row = GameRow
            { gameRowId                 = Nothing
            , gameRowGameDate           = gameDate game
            , gameRowAwayTeamId         = awayDb
            , gameRowHomeTeamId         = homeDb
            , gameRowLastSyncedProvider = Just provider
            , gameRowLastSyncedAt       = Just syncedAt
            }
      _ <- Games.upsertGameByExternalId provider extId row
      pure True
    _ -> pure False

resolveTeam
  :: Teams :> es
  => ProviderName
  -> TeamId
  -> Eff es (Maybe DbTeamId)
resolveTeam provider tid =
  Teams.lookupTeamByExternalId provider (externalIdFromTeamId tid)

logConvertWarnings :: Logging :> es => [Convert.ConvertWarning] -> Eff es ()
logConvertWarnings = mapM_ (\w -> logFM WarningS (Convert.renderWarning w))