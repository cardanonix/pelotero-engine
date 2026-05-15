{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pelotero.DB.Team
  ( Team (..)
  , teamSchema
  , teamExternalIdSchema
  , TeamRow (..)
  , LoadedTeamRow (..)
  , teamRowToLoaded
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

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified Hasql.Transaction as Tx
import Rel8 hiding (fromResult)

import Pelotero.DB.Pool (DBError, Pool, runTransaction)
import qualified Pelotero.DB.ProviderKeyed as PK
import Pelotero.DB.ProviderKeyed (ExternalIdE (..), ProviderKeyed (..))
import Pelotero.DB.Provider (ProviderName)
import Pelotero.DB.Rel8Instances ()
import Pelotero.Domain.Id (DbTeamId)

-- ---------------------------------------------------------------------
-- Rel8 entity
-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------
-- Public row types
-- ---------------------------------------------------------------------

-- | Write-path row. The id is 'Maybe' because the row may not yet
-- have a database id at insert time. Use 'LoadedTeamRow' for reads,
-- where the id is always present.
data TeamRow = TeamRow
  { teamRowId                 :: !(Maybe DbTeamId)
  , teamRowName               :: !Text
  , teamRowAbbreviation       :: !Text
  , teamRowLocationName       :: !Text
  , teamRowLastSyncedProvider :: !(Maybe ProviderName)
  , teamRowLastSyncedAt       :: !(Maybe UTCTime)
  }
  deriving stock (Show, Eq)

-- | Read-path row. Every loaded row has an id by construction.
data LoadedTeamRow = LoadedTeamRow
  { ltrId                 :: !DbTeamId
  , ltrName               :: !Text
  , ltrAbbreviation       :: !Text
  , ltrLocationName       :: !Text
  , ltrLastSyncedProvider :: !(Maybe ProviderName)
  , ltrLastSyncedAt       :: !(Maybe UTCTime)
  }
  deriving stock (Show, Eq)

-- | Build a 'LoadedTeamRow' from a write-path 'TeamRow' plus the
-- authoritative id. Used by the in-memory effect interpreter to
-- store rows under their assigned ids.
teamRowToLoaded :: DbTeamId -> TeamRow -> LoadedTeamRow
teamRowToLoaded tid TeamRow{..} = LoadedTeamRow
  { ltrId                 = tid
  , ltrName               = teamRowName
  , ltrAbbreviation       = teamRowAbbreviation
  , ltrLocationName       = teamRowLocationName
  , ltrLastSyncedProvider = teamRowLastSyncedProvider
  , ltrLastSyncedAt       = teamRowLastSyncedAt
  }

-- ---------------------------------------------------------------------
-- Schemas
-- ---------------------------------------------------------------------

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

teamExternalIdSchema :: TableSchema (ExternalIdE DbTeamId Name)
teamExternalIdSchema = TableSchema
  { name    = "team_external_id"
  , columns = ExternalIdE
      { _eidEntityId   = "team_id"
      , _eidProvider   = "provider"
      , _eidExternalId = "external_id"
      , _eidFetchedAt  = "fetched_at"
      }
  }

-- ---------------------------------------------------------------------
-- Result <-> public row
-- ---------------------------------------------------------------------

fromResult :: Team Result -> LoadedTeamRow
fromResult Team{..} = LoadedTeamRow
  { ltrId                 = _teamId
  , ltrName               = _teamName
  , ltrAbbreviation       = _teamAbbreviation
  , ltrLocationName       = _teamLocationName
  , ltrLastSyncedProvider = _teamLastSyncedProvider
  , ltrLastSyncedAt       = _teamLastSyncedAt
  }

teamRowToExpr :: TeamRow -> Team Expr
teamRowToExpr TeamRow{..} = Team
  { _teamId                 = case teamRowId of
                                Nothing  -> unsafeDefault
                                Just tid -> lit tid
  , _teamName               = lit teamRowName
  , _teamAbbreviation       = lit teamRowAbbreviation
  , _teamLocationName       = lit teamRowLocationName
  , _teamLastSyncedProvider = lit teamRowLastSyncedProvider
  , _teamLastSyncedAt       = lit teamRowLastSyncedAt
  }

-- ---------------------------------------------------------------------
-- Insert / Update
-- ---------------------------------------------------------------------

insertTeamT :: TeamRow -> Tx.Transaction DbTeamId
insertTeamT row =
  Tx.statement () $ run1 $ insert Insert
    { into       = teamSchema
    , rows       = values [teamRowToExpr row]
    , onConflict = Abort
    , returning  = Returning _teamId
    }

updateTeamT :: DbTeamId -> TeamRow -> Tx.Transaction ()
updateTeamT tid row =
  Tx.statement () $ run_ $ update Update
    { target      = teamSchema
    , from        = pure ()
    , set         = \_ _ -> (teamRowToExpr row) { _teamId = lit tid }
    , updateWhere = \_ t -> _teamId t ==. lit tid
    , returning   = NoReturning
    }

-- ---------------------------------------------------------------------
-- Reads
-- ---------------------------------------------------------------------

getByIdT :: DbTeamId -> Tx.Transaction (Maybe LoadedTeamRow)
getByIdT tid = do
  rows <- Tx.statement () $ run $ select $ do
    t <- each teamSchema
    where_ $ _teamId t ==. lit tid
    pure t
  pure $ case rows of
    (t : _) -> Just (fromResult t)
    []      -> Nothing

getAllT :: Tx.Transaction [LoadedTeamRow]
getAllT = do
  rows <- Tx.statement () $ run $ select $
    orderBy (_teamName >$< asc) (each teamSchema)
  pure (map fromResult rows)

-- ---------------------------------------------------------------------
-- ProviderKeyed instance
-- ---------------------------------------------------------------------

instance ProviderKeyed TeamRow where
  type RowEntity TeamRow = Team
  type RowId     TeamRow = DbTeamId

  rowSchema        = teamSchema
  externalIdSchema = teamExternalIdSchema
  rowIdColumn      = _teamId
  insertRowT       = insertTeamT
  updateRowByIdT   = updateTeamT

-- ---------------------------------------------------------------------
-- External-id facades
-- ---------------------------------------------------------------------

linkExternalIdT :: DbTeamId -> ProviderName -> Text -> Tx.Transaction ()
linkExternalIdT = PK.linkExternalIdT @TeamRow

lookupByExternalIdT :: ProviderName -> Text -> Tx.Transaction (Maybe DbTeamId)
lookupByExternalIdT = PK.lookupByExternalIdT @TeamRow

getExternalIdT :: DbTeamId -> ProviderName -> Tx.Transaction (Maybe Text)
getExternalIdT = PK.getExternalIdT @TeamRow

upsertByExternalIdT :: ProviderName -> Text -> TeamRow -> Tx.Transaction DbTeamId
upsertByExternalIdT = PK.upsertByExternalIdT @TeamRow

-- ---------------------------------------------------------------------
-- Pool-flavored wrappers
-- ---------------------------------------------------------------------

insertTeam :: Pool -> TeamRow -> IO (Either DBError DbTeamId)
insertTeam pool row = runTransaction pool (insertTeamT row)

updateTeam :: Pool -> DbTeamId -> TeamRow -> IO (Either DBError ())
updateTeam pool tid row = runTransaction pool (updateTeamT tid row)

getById :: Pool -> DbTeamId -> IO (Either DBError (Maybe LoadedTeamRow))
getById pool tid = runTransaction pool (getByIdT tid)

getAll :: Pool -> IO (Either DBError [LoadedTeamRow])
getAll pool = runTransaction pool getAllT

linkExternalId :: Pool -> DbTeamId -> ProviderName -> Text -> IO (Either DBError ())
linkExternalId = PK.linkExternalId @TeamRow

lookupByExternalId :: Pool -> ProviderName -> Text -> IO (Either DBError (Maybe DbTeamId))
lookupByExternalId = PK.lookupByExternalId @TeamRow

getExternalId :: Pool -> DbTeamId -> ProviderName -> IO (Either DBError (Maybe Text))
getExternalId = PK.getExternalId @TeamRow

upsertByExternalId :: Pool -> ProviderName -> Text -> TeamRow -> IO (Either DBError DbTeamId)
upsertByExternalId = PK.upsertByExternalId @TeamRow