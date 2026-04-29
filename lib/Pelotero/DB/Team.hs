{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pelotero.DB.Team
  ( TeamRow(..)
  , insertTeamT
  , updateTeamT
  , getByIdT
  , getAllT
  , linkExternalIdT
  , lookupByExternalIdT
  , getExternalIdT
  , upsertByExternalIdT
  , insertTeam
  , updateTeam
  , getById
  , getAll
  , linkExternalId
  , lookupByExternalId
  , getExternalId
  , upsertByExternalId
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           GHC.Generics               (Generic)

import qualified Hasql.Transaction          as Tx

import           Rel8                       ( Column
                                            , Name
                                            , Rel8able
                                            , Result
                                            , TableSchema(..)
                                            , (==.)
                                            )
import qualified Rel8                       as R

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.DB.Rel8Instances ()
import Pelotero.Domain.Id    (DbTeamId(..))

-- ============================================================================
-- team
-- ============================================================================

data Team f = Team
  { _teamId                 :: Column f DbTeamId
  , _teamName               :: Column f Text
  , _teamAbbreviation       :: Column f Text
  , _teamLocationName       :: Column f Text
  , _teamLastSyncedProvider :: Column f (Maybe ProviderName)
  , _teamLastSyncedAt       :: Column f (Maybe UTCTime)
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Team f)
deriving stock instance f ~ Result => Eq   (Team f)

teamSchema :: TableSchema (Team Name)
teamSchema = TableSchema
  { name    = "team"
  , columns = Team
      { _teamId                 = "id"
      , _teamName               = "name"
      , _teamAbbreviation       = "abbreviation"
      , _teamLocationName       = "location_name"
      , _teamLastSyncedProvider = "last_synced_provider"
      , _teamLastSyncedAt       = "last_synced_at"
      }
  }

-- ============================================================================
-- team_external_id
-- ============================================================================

data TeamExternalId f = TeamExternalId
  { _teidTeamId     :: Column f DbTeamId
  , _teidProvider   :: Column f ProviderName
  , _teidExternalId :: Column f Text
  , _teidFetchedAt  :: Column f UTCTime
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

teamExternalIdSchema :: TableSchema (TeamExternalId Name)
teamExternalIdSchema = TableSchema
  { name    = "team_external_id"
  , columns = TeamExternalId
      { _teidTeamId     = "team_id"
      , _teidProvider   = "provider"
      , _teidExternalId = "external_id"
      , _teidFetchedAt  = "fetched_at"
      }
  }

-- ============================================================================
-- Public row type (API compatibility)
-- ============================================================================

data TeamRow = TeamRow
  { teamRowId                 :: !(Maybe DbTeamId)
  , teamRowName               :: !Text
  , teamRowAbbreviation       :: !Text
  , teamRowLocationName       :: !Text
  , teamRowLastSyncedProvider :: !(Maybe ProviderName)
  , teamRowLastSyncedAt       :: !(Maybe UTCTime)
  }
  deriving stock (Show, Eq)

fromResult :: Team Result -> TeamRow
fromResult Team{..} = TeamRow
  { teamRowId                 = Just _teamId
  , teamRowName               = _teamName
  , teamRowAbbreviation       = _teamAbbreviation
  , teamRowLocationName       = _teamLocationName
  , teamRowLastSyncedProvider = _teamLastSyncedProvider
  , teamRowLastSyncedAt       = _teamLastSyncedAt
  }

-- ============================================================================
-- Transaction-flavored CRUD
-- ============================================================================

insertTeamT :: TeamRow -> Tx.Transaction DbTeamId
insertTeamT row = Tx.statement () $ R.run1 $ R.insert R.Insert
  { R.into       = teamSchema
  , R.rows       = R.values
      [ Team
          { _teamId                 = R.unsafeDefault
          , _teamName               = R.lit (teamRowName row)
          , _teamAbbreviation       = R.lit (teamRowAbbreviation row)
          , _teamLocationName       = R.lit (teamRowLocationName row)
          , _teamLastSyncedProvider = R.lit (teamRowLastSyncedProvider row)
          , _teamLastSyncedAt       = R.lit (teamRowLastSyncedAt row)
          }
      ]
  , R.onConflict = R.Abort
  , R.returning  = R.Returning _teamId
  }

updateTeamT :: DbTeamId -> TeamRow -> Tx.Transaction ()
updateTeamT tid row = Tx.statement () $ R.run_ $ R.update R.Update
  { R.target      = teamSchema
  , R.from        = pure ()
  , R.set         = \_ t -> t
      { _teamName               = R.lit (teamRowName row)
      , _teamAbbreviation       = R.lit (teamRowAbbreviation row)
      , _teamLocationName       = R.lit (teamRowLocationName row)
      , _teamLastSyncedProvider = R.lit (teamRowLastSyncedProvider row)
      , _teamLastSyncedAt       = R.lit (teamRowLastSyncedAt row)
      }
  , R.updateWhere = \_ t -> _teamId t ==. R.lit tid
  , R.returning   = R.NoReturning
  }

getByIdT :: DbTeamId -> Tx.Transaction (Maybe TeamRow)
getByIdT tid = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    t <- R.each teamSchema
    R.where_ (_teamId t ==. R.lit tid)
    pure t
  pure $ case rows of
    (t : _) -> Just (fromResult t)
    []      -> Nothing

getAllT :: Tx.Transaction [TeamRow]
getAllT = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.orderBy (_teamName >$< R.asc) (R.each teamSchema)
  pure (map fromResult rows)

linkExternalIdT :: DbTeamId -> ProviderName -> Text -> Tx.Transaction ()
linkExternalIdT tid provider extId = Tx.statement () $ R.run_ $ R.insert R.Insert
  { R.into       = teamExternalIdSchema
  , R.rows       = R.values
      [ TeamExternalId
          { _teidTeamId     = R.lit tid
          , _teidProvider   = R.lit provider
          , _teidExternalId = R.lit extId
          , _teidFetchedAt  = R.unsafeDefault
          }
      ]
  , R.onConflict = R.DoNothing
  , R.returning  = R.NoReturning
  }

lookupByExternalIdT :: ProviderName -> Text -> Tx.Transaction (Maybe DbTeamId)
lookupByExternalIdT provider extId = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    e <- R.each teamExternalIdSchema
    R.where_ (_teidProvider   e ==. R.lit provider)
    R.where_ (_teidExternalId e ==. R.lit extId)
    pure (_teidTeamId e)
  pure $ case rows of
    (tid : _) -> Just tid
    []        -> Nothing

getExternalIdT :: DbTeamId -> ProviderName -> Tx.Transaction (Maybe Text)
getExternalIdT tid provider = do
  rows <- Tx.statement () $ R.run $ R.select $ do
    e <- R.each teamExternalIdSchema
    R.where_ (_teidTeamId   e ==. R.lit tid)
    R.where_ (_teidProvider e ==. R.lit provider)
    pure (_teidExternalId e)
  pure $ case rows of
    (extId : _) -> Just extId
    []          -> Nothing

upsertByExternalIdT
  :: ProviderName -> Text -> TeamRow -> Tx.Transaction DbTeamId
upsertByExternalIdT provider extId row = do
  found <- lookupByExternalIdT provider extId
  case found of
    Just tid -> do
      updateTeamT tid row
      pure tid
    Nothing -> do
      tid <- insertTeamT row
      linkExternalIdT tid provider extId
      pure tid

-- ============================================================================
-- Pool-flavored CRUD
-- ============================================================================

insertTeam :: Pool -> TeamRow -> IO (Either DBError DbTeamId)
insertTeam pool row = runTransaction pool (insertTeamT row)

updateTeam :: Pool -> DbTeamId -> TeamRow -> IO (Either DBError ())
updateTeam pool tid row = runTransaction pool (updateTeamT tid row)

getById :: Pool -> DbTeamId -> IO (Either DBError (Maybe TeamRow))
getById pool tid = runTransaction pool (getByIdT tid)

getAll :: Pool -> IO (Either DBError [TeamRow])
getAll pool = runTransaction pool getAllT

linkExternalId :: Pool -> DbTeamId -> ProviderName -> Text -> IO (Either DBError ())
linkExternalId pool tid provider extId =
  runTransaction pool (linkExternalIdT tid provider extId)

lookupByExternalId :: Pool -> ProviderName -> Text -> IO (Either DBError (Maybe DbTeamId))
lookupByExternalId pool provider extId =
  runTransaction pool (lookupByExternalIdT provider extId)

getExternalId :: Pool -> DbTeamId -> ProviderName -> IO (Either DBError (Maybe Text))
getExternalId pool tid provider =
  runTransaction pool (getExternalIdT tid provider)

upsertByExternalId
  :: Pool -> ProviderName -> Text -> TeamRow -> IO (Either DBError DbTeamId)
upsertByExternalId pool provider extId row =
  runTransaction pool (upsertByExternalIdT provider extId row)