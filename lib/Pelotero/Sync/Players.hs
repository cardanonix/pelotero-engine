{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Pelotero.Sync.Players
  ( SyncResult (..)
  , syncRosters
  , upsertAllTeams
  , upsertAllPlayers
  , logConvertWarnings
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Effectful
import Katip (Severity (..))

import Pelotero.DB.FetchLog (FetchLogRow (..))
import Pelotero.DB.Player (PlayerRow (..))
import Pelotero.DB.Provider (ProviderName)
import Pelotero.DB.Team (TeamRow (..))
import Pelotero.Domain.Id (DbTeamId, TeamId)
import qualified Pelotero.Domain.Player as DPlayer
import qualified Pelotero.Domain.Position as DPos
import qualified Pelotero.Domain.Team as DTeam
import Pelotero.Effects.Clock (Clock)
import qualified Pelotero.Effects.Clock as Clock
import Pelotero.Effects.FetchLog (FetchLog)
import qualified Pelotero.Effects.FetchLog as FetchLog
import Pelotero.Effects.Logging (Logging, logFM)
import Pelotero.Effects.Players (Players)
import qualified Pelotero.Effects.Players as Players
import Pelotero.Effects.Teams (Teams)
import qualified Pelotero.Effects.Teams as Teams
import qualified Pelotero.MLB.Convert as Convert
import Pelotero.Provider.ExternalId (externalIdFromPlayerId, externalIdFromTeamId)

-- | Summary returned from a roster sync run.
--
-- In the SHA short-circuit path (Phase C.1), 'syncTeamsUpserted' and
-- 'syncPlayersUpserted' are zero: the inbound payload matched the most
-- recent fetch for this scope and no rows were touched.
data SyncResult = SyncResult
  { syncTeamsUpserted   :: !Int
  , syncPlayersUpserted :: !Int
  , syncFetchSha256     :: !T.Text
  }
  deriving stock (Show, Eq)

-- | The 'resource' label written into 'provider_fetch_log' rows produced
-- by 'syncRosters'. The MLB roster URL pre-filters to active players, so
-- this is the literal truth of what was fetched.
rostersResource :: T.Text
rostersResource = "active-rosters"

-- | Provider-agnostic upsert pipeline: teams, then players, then a
-- fetch-log entry. Idempotent by SHA: if 'payloadSha' equals the most
-- recent recorded SHA for ('provider', 'rostersResource', 'scope'), the
-- call returns immediately with a zero-row 'SyncResult' and a single
-- InfoS log line.
syncRosters
  :: ( Players  :> es
     , Teams    :> es
     , FetchLog :> es
     , Clock    :> es
     , Logging  :> es
     )
  => ProviderName
  -> T.Text             -- ^ scope (e.g. season as text)
  -> T.Text             -- ^ SHA-256 of raw payload, computed by caller
  -> [DTeam.Team]
  -> [DPlayer.Player]
  -> Eff es SyncResult
syncRosters provider scope payloadSha teams players = do
  prior <- FetchLog.getLastFetch provider rostersResource scope
  case prior of
    Just FetchLogRow { fetchLogPayloadSha256 = oldSha }
      | oldSha == payloadSha -> do
          logFM InfoS $
            "rosters: payload unchanged, skipping; sha=" <> payloadSha
          pure SyncResult
            { syncTeamsUpserted   = 0
            , syncPlayersUpserted = 0
            , syncFetchSha256     = payloadSha
            }
    _ -> do
      syncedAt <- Clock.now
      teamMap  <- upsertAllTeams provider syncedAt teams
      upsertAllPlayers provider syncedAt teamMap players
      FetchLog.recordFetch FetchLogRow
        { fetchLogId            = Nothing
        , fetchLogProvider      = provider
        , fetchLogResource      = rostersResource
        , fetchLogScope         = scope
        , fetchLogFetchedAt     = Nothing
        , fetchLogPayloadSha256 = payloadSha
        , fetchLogRecordCount   = fromIntegral (length players)
        }
      pure SyncResult
        { syncTeamsUpserted   = length teams
        , syncPlayersUpserted = length players
        , syncFetchSha256     = payloadSha
        }

upsertAllTeams
  :: Teams :> es
  => ProviderName
  -> UTCTime
  -> [DTeam.Team]
  -> Eff es (Map.Map TeamId DbTeamId)
upsertAllTeams provider syncedAt = fmap Map.fromList . traverse upsertOne
  where
    upsertOne team = do
      let extId = externalIdFromTeamId (DTeam.teamId team)
          row = TeamRow
            { teamRowId                 = Nothing
            , teamRowName               = DTeam.teamName team
            , teamRowAbbreviation       = DTeam.teamAbbreviation team
            , teamRowLocationName       = DTeam.teamLocationName team
            , teamRowLastSyncedProvider = Just provider
            , teamRowLastSyncedAt       = Just syncedAt
            }
      dbId <- Teams.upsertTeamByExternalId provider extId row
      pure (DTeam.teamId team, dbId)

upsertAllPlayers
  :: Players :> es
  => ProviderName
  -> UTCTime
  -> Map.Map TeamId DbTeamId
  -> [DPlayer.Player]
  -> Eff es ()
upsertAllPlayers provider syncedAt teamMap = mapM_ upsertOne
  where
    upsertOne player = do
      let extId = externalIdFromPlayerId (DPlayer.playerId player)
          row = PlayerRow
            { playerRowId                 = Nothing
            , playerRowFirstName          = DPlayer.playerFirstName player
            , playerRowLastName           = DPlayer.playerLastName player
            , playerRowNameSlug           = DPlayer.playerNameSlug player
            , playerRowPosition           = fmap DPos.renderPosition
                                                 (DPlayer.playerPosition player)
            , playerRowBatSide            = fmap DPlayer.handChar
                                                 (DPlayer.playerBatSide player)
            , playerRowPitchHand          = fmap DPlayer.handChar
                                                 (DPlayer.playerPitchHand player)
            , playerRowActive             = DPlayer.playerActive player
            , playerRowCurrentTeamId      = DPlayer.playerTeamId player
                                              >>= flip Map.lookup teamMap
            , playerRowLastSyncedProvider = Just provider
            , playerRowLastSyncedAt       = Just syncedAt
            }
      _ <- Players.upsertPlayerByExternalId provider extId row
      pure ()

-- | Render each ConvertWarning at WarningS through the Logging effect.
logConvertWarnings :: Logging :> es => [Convert.ConvertWarning] -> Eff es ()
logConvertWarnings = mapM_ (\w -> logFM WarningS (Convert.renderWarning w))