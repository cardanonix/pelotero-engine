{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Roster sync pipeline, provider-agnostic.
--
-- Takes already-converted domain types and upserts them via effects.
-- Parsing raw bytes and converting wire → domain happens upstream in the
-- caller (CLI glue, test harness, alternative provider module).
--
-- Sync ordering: teams before players, because each player carries a
-- reference to its current team.
module Pelotero.Sync.Players
  ( SyncResult(..)
  , syncRosters
  ) where

import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import Data.Time.Clock                  (UTCTime)

import Effectful (Eff, (:>))

import Pelotero.DB.FetchLog       (FetchLogRow(..))
import Pelotero.DB.Player         (PlayerRow(..))
import Pelotero.DB.Provider       (ProviderName)
import Pelotero.DB.Team           (TeamRow(..))
import Pelotero.Domain.Id         (DbTeamId, PlayerId(..), TeamId(..), unPlayerId, unTeamId)
import qualified Pelotero.Domain.Player    as DPlayer
import qualified Pelotero.Domain.Position  as DPos
import qualified Pelotero.Domain.Team      as DTeam

import Pelotero.Effects.Clock     (Clock, now)
import Pelotero.Effects.FetchLog  (FetchLog, recordFetch)
import Pelotero.Effects.Players   (Players, upsertPlayerByExternalId)
import Pelotero.Effects.Teams     (Teams, upsertTeamByExternalId)

--------------------------------------------------------------------------------
-- Result type

data SyncResult = SyncResult
  { syncTeamsUpserted   :: !Int
  , syncPlayersUpserted :: !Int
  , syncFetchSha256     :: !T.Text
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Public entry point

-- | Upsert teams then players, record the fetch. All inputs are
-- provider-agnostic domain types; the caller is responsible for fetching
-- raw data, parsing it, converting wire → domain, and computing the
-- payload checksum.
syncRosters
  :: ( Players  :> es
     , Teams    :> es
     , FetchLog :> es
     , Clock    :> es
     )
  => ProviderName
  -> T.Text              -- ^ scope (e.g. "2025" for season)
  -> T.Text              -- ^ SHA-256 of raw payload, computed by caller
  -> [DTeam.Team]        -- ^ teams, already converted from wire format
  -> [DPlayer.Player]    -- ^ players, already converted from wire format
  -> Eff es SyncResult
syncRosters provider scope payloadSha teams players = do
  syncedAt <- now

  teamMap <- upsertAllTeams provider syncedAt teams
  upsertAllPlayers provider syncedAt teamMap players

  recordFetch FetchLogRow
    { fetchLogId            = Nothing
    , fetchLogProvider      = provider
    , fetchLogResource      = "active-rosters"
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

--------------------------------------------------------------------------------
-- Helpers

upsertAllTeams
  :: Teams :> es
  => ProviderName
  -> UTCTime
  -> [DTeam.Team]
  -> Eff es (Map.Map TeamId DbTeamId)
upsertAllTeams provider syncedAt teams =
  Map.fromList <$> traverse upsertOne teams
  where
    upsertOne t = do
      let tid   = DTeam.teamId t
          extId = T.pack (show (unTeamId tid))
          row   = TeamRow
            { teamRowId                 = Nothing
            , teamRowName               = DTeam.teamName t
            , teamRowAbbreviation       = DTeam.teamAbbreviation t
            , teamRowLocationName       = DTeam.teamLocationName t
            , teamRowLastSyncedProvider = Just provider
            , teamRowLastSyncedAt       = Just syncedAt
            }
      dbTid <- upsertTeamByExternalId provider extId row
      pure (tid, dbTid)

upsertAllPlayers
  :: Players :> es
  => ProviderName
  -> UTCTime
  -> Map.Map TeamId DbTeamId
  -> [DPlayer.Player]
  -> Eff es ()
upsertAllPlayers provider syncedAt teamMap = mapM_ upsertOne
  where
    upsertOne p = do
      let pid    = DPlayer.playerId p
          extId  = T.pack (show (unPlayerId pid))
          dbTeam = DPlayer.playerTeamId p >>= flip Map.lookup teamMap
          row    = PlayerRow
            { playerRowId                 = Nothing
            , playerRowFirstName          = DPlayer.playerFirstName p
            , playerRowLastName           = DPlayer.playerLastName  p
            , playerRowNameSlug           = DPlayer.playerNameSlug  p
            , playerRowPosition           = DPos.renderPosition <$> DPlayer.playerPosition p
            , playerRowBatSide            = handToChar <$> DPlayer.playerBatSide p
            , playerRowPitchHand          = handToChar <$> DPlayer.playerPitchHand p
            , playerRowActive             = DPlayer.playerActive p
            , playerRowCurrentTeamId      = dbTeam
            , playerRowLastSyncedProvider = Just provider
            , playerRowLastSyncedAt       = Just syncedAt
            }
      _ <- upsertPlayerByExternalId provider extId row
      pure ()

    handToChar = T.head . DPlayer.renderHandedness