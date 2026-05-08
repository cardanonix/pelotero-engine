{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shared 'external_id' companion-table machinery for the three
-- provider-keyed tables (player, team, game).
--
-- Each public row type ('PlayerRow', 'TeamRow', 'GameRow') has a
-- surrogate 'BIGSERIAL' primary key plus a paired
-- @\<table\>_external_id@ table holding (entity_id, provider,
-- external_id, fetched_at). The table shape is identical; only the
-- entity-id type differs. This module captures that pattern as a
-- typeclass and provides the four reusable transactional helpers
-- (@link@, @lookup@, @getExternalId@, @upsert@) plus their Pool-flavored
-- wrappers.
--
-- Per-table modules retain their own entity-specific reads
-- ('getByIdT', 'getAllT', 'getActiveT', 'getByDateRangeT', etc.) and
-- their own 'insertRowT' \/ 'updateRowByIdT' implementations (because
-- the rel8 'Insert' \/ 'Update' clauses are row-shape-specific). Those
-- two methods are required by the 'ProviderKeyed' instance.
module Pelotero.DB.ProviderKeyed
  ( ExternalIdE (..)
  , ProviderKeyed (..)
    -- * Transactional helpers
  , linkExternalIdT
  , lookupByExternalIdT
  , getExternalIdT
  , upsertByExternalIdT
    -- * Pool-flavored wrappers
  , linkExternalId
  , lookupByExternalId
  , getExternalId
  , upsertByExternalId
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified Hasql.Transaction as Tx
import Rel8

import Pelotero.DB.Pool (DBError, Pool, runTransaction)
import Pelotero.DB.Provider (ProviderName)
import Pelotero.DB.Rel8Instances ()

-- | The shared external-id companion-table shape.
--
-- Parameterised over the surrogate id type, so the same record backs
-- @player_external_id@, @team_external_id@, and @game_external_id@.
data ExternalIdE i f = ExternalIdE
  { _eidEntityId   :: Column f i
  , _eidProvider   :: Column f ProviderName
  , _eidExternalId :: Column f Text
  , _eidFetchedAt  :: Column f UTCTime
  }
  deriving stock (Generic)

deriving anyclass instance
  (DBType i, DBEq i) => Rel8able (ExternalIdE i)

-- | Witnesses that 'row' is the public row type for a table that has a
-- surrogate id and a paired external-id companion table.
class
  ( Rel8able (RowEntity row)
  , DBType (RowId row)
  , DBEq (RowId row)
  ) => ProviderKeyed row where
  type RowEntity row :: (Type -> Type) -> Type
  type RowId row     :: Type

  rowSchema        :: TableSchema (RowEntity row Name)
  externalIdSchema :: TableSchema (ExternalIdE (RowId row) Name)
  rowIdColumn      :: RowEntity row Expr -> Expr (RowId row)

  -- | Insert a new row with a DB-assigned surrogate id and return that
  -- id. Per-table implementation because the rel8 'Insert' clause needs
  -- the row's specific 'rowToExpr' translation.
  insertRowT       :: row -> Tx.Transaction (RowId row)

  -- | Update the row identified by the given surrogate id. The row's
  -- own id field (if any) is ignored; the caller-supplied 'RowId' wins.
  updateRowByIdT   :: RowId row -> row -> Tx.Transaction ()


-- ---------------------------------------------------------------------
-- Transactional helpers
-- ---------------------------------------------------------------------

-- | INSERT into the external-id table; ON CONFLICT DO NOTHING.
linkExternalIdT
  :: forall row. ProviderKeyed row
  => RowId row -> ProviderName -> Text -> Tx.Transaction ()
linkExternalIdT rowId provider extId =
  Tx.statement () $ run_ $ insert Insert
    { into       = externalIdSchema @row
    , rows       = values
        [ ExternalIdE
            { _eidEntityId   = lit rowId
            , _eidProvider   = lit provider
            , _eidExternalId = lit extId
            , _eidFetchedAt  = unsafeDefault
            }
        ]
    , onConflict = DoNothing
    , returning  = NoReturning
    }

-- | SELECT entity_id WHERE provider AND external_id.
lookupByExternalIdT
  :: forall row. ProviderKeyed row
  => ProviderName -> Text -> Tx.Transaction (Maybe (RowId row))
lookupByExternalIdT provider extId = do
  rows <- Tx.statement () $ run $ select $ do
    e <- each (externalIdSchema @row)
    where_ $ _eidProvider   e ==. lit provider
         &&. _eidExternalId e ==. lit extId
    pure (_eidEntityId e)
  pure $ case rows of
    (x : _) -> Just x
    []      -> Nothing

-- | Reverse lookup: (rowId, provider) -> Maybe external_id.
getExternalIdT
  :: forall row. ProviderKeyed row
  => RowId row -> ProviderName -> Tx.Transaction (Maybe Text)
getExternalIdT rowId provider = do
  rows <- Tx.statement () $ run $ select $ do
    e <- each (externalIdSchema @row)
    where_ $ _eidEntityId e ==. lit rowId
         &&. _eidProvider e ==. lit provider
    pure (_eidExternalId e)
  pure $ case rows of
    (x : _) -> Just x
    []      -> Nothing

-- | Lookup-by-(provider, externalId), then either UPDATE the existing
-- row or INSERT a new one and link it. Atomic within the caller's
-- transaction.
upsertByExternalIdT
  :: forall row. ProviderKeyed row
  => ProviderName -> Text -> row -> Tx.Transaction (RowId row)
upsertByExternalIdT provider extId row = do
  mExisting <- lookupByExternalIdT @row provider extId
  case mExisting of
    Just rowId -> do
      updateRowByIdT @row rowId row
      pure rowId
    Nothing -> do
      newRowId <- insertRowT @row row
      linkExternalIdT @row newRowId provider extId
      pure newRowId


-- ---------------------------------------------------------------------
-- Pool-flavored wrappers
-- ---------------------------------------------------------------------

linkExternalId
  :: forall row. ProviderKeyed row
  => Pool -> RowId row -> ProviderName -> Text -> IO (Either DBError ())
linkExternalId pool rid p e =
  runTransaction pool (linkExternalIdT @row rid p e)

lookupByExternalId
  :: forall row. ProviderKeyed row
  => Pool
  -> ProviderName
  -> Text
  -> IO (Either DBError (Maybe (RowId row)))
lookupByExternalId pool p e =
  runTransaction pool (lookupByExternalIdT @row p e)

getExternalId
  :: forall row. ProviderKeyed row
  => Pool
  -> RowId row
  -> ProviderName
  -> IO (Either DBError (Maybe Text))
getExternalId pool rid p =
  runTransaction pool (getExternalIdT @row rid p)

upsertByExternalId
  :: forall row. ProviderKeyed row
  => Pool
  -> ProviderName
  -> Text
  -> row
  -> IO (Either DBError (RowId row))
upsertByExternalId pool p e row =
  runTransaction pool (upsertByExternalIdT @row p e row)