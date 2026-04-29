{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Schedule sync pipeline, provider-agnostic.
--
-- Takes already-converted domain 'Game' values and upserts them. Team
-- references are resolved to DB surrogate keys via the Teams effect;
-- games referencing unknown teams are skipped.
module Pelotero.Sync.Schedule
  ( ScheduleSyncResult(..)
  , syncSchedule
  ) where

import qualified Data.Text       as T
import Data.Time.Clock           (UTCTime)

import Effectful (Eff, (:>))

import Pelotero.DB.Game          (GameRow(..))
import Pelotero.DB.Provider      (ProviderName)
import Pelotero.Domain.Game      (Game(..))
import Pelotero.Domain.Id        (DbTeamId, TeamId(..), unGameId)

import Pelotero.Effects.Clock    (Clock, now)
import Pelotero.Effects.Games    (Games, upsertGameByExternalId)
import Pelotero.Effects.Teams    (Teams, lookupTeamByExternalId)

--------------------------------------------------------------------------------
-- Result type

data ScheduleSyncResult = ScheduleSyncResult
  { schedGamesUpserted :: !Int
  , schedGamesSkipped  :: !Int
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Public entry point

syncSchedule
  :: ( Games :> es
     , Teams :> es
     , Clock :> es
     )
  => ProviderName
  -> [Game]
  -> Eff es ScheduleSyncResult
syncSchedule provider games = do
  syncedAt <- now
  results  <- mapM (upsertOneGame provider syncedAt) games

  let upserted = length (filter id results)
      skipped  = length (filter not results)

  pure ScheduleSyncResult
    { schedGamesUpserted = upserted
    , schedGamesSkipped  = skipped
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