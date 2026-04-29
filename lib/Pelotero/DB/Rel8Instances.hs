{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Rel8 'DBType' / 'DBEq' / 'DBOrd' instances for domain types.
--
-- These are deliberate orphans. The alternatives would be:
--
--   * Put them in 'Pelotero.Domain.Id' / 'Pelotero.DB.Provider', which would
--     force those modules to depend on rel8 (wrong direction; the domain
--     layer must not know about the DB library).
--
--   * Put them in each 'Pelotero.DB.*' module, which causes orphan-instance
--     conflicts as soon as two repos reference the same type.
--
-- This module is the lowest place in the DB layer that already imports both
-- the domain types and rel8, so the instances live here. The DB-table
-- modules import this with the empty-import idiom (@import Pelotero.DB.Rel8Instances ()@)
-- to bring the instances into scope without using any names from the module.
module Pelotero.DB.Rel8Instances () where

import qualified Data.Text as T

import qualified Rel8 as R

import Pelotero.Domain.Id      ( DbDraftPickId(..)
                               , DbGameId(..)
                               , DbLeagueConfigId(..)
                               , DbLeagueTeamId(..)
                               , DbPlayerId(..)
                               , DbTeamId(..)
                               )
import Pelotero.DB.Provider    (ProviderName, parseProviderName, renderProviderName)
import Pelotero.Domain.Roster  (LineupLimits, RosterLimits)
import Pelotero.Domain.Scoring (LeagueScoring)

-- ----------------------------------------------------------------------------
-- Numeric domain ids: trivial newtype-deriving over Int64.
-- ----------------------------------------------------------------------------

deriving newtype instance R.DBType DbPlayerId
deriving newtype instance R.DBEq   DbPlayerId
deriving newtype instance R.DBOrd  DbPlayerId

deriving newtype instance R.DBType DbTeamId
deriving newtype instance R.DBEq   DbTeamId
deriving newtype instance R.DBOrd  DbTeamId

deriving newtype instance R.DBType DbGameId
deriving newtype instance R.DBEq   DbGameId
deriving newtype instance R.DBOrd  DbGameId

deriving newtype instance R.DBType DbLeagueConfigId
deriving newtype instance R.DBEq   DbLeagueConfigId
deriving newtype instance R.DBOrd  DbLeagueConfigId

deriving newtype instance R.DBType DbLeagueTeamId
deriving newtype instance R.DBEq   DbLeagueTeamId
deriving newtype instance R.DBOrd  DbLeagueTeamId

deriving newtype instance R.DBType DbDraftPickId
deriving newtype instance R.DBEq   DbDraftPickId
deriving newtype instance R.DBOrd  DbDraftPickId

-- ----------------------------------------------------------------------------
-- ProviderName: text-encoded enum with an explicit parser.
-- ----------------------------------------------------------------------------

instance R.DBType ProviderName where
  typeInformation = R.parseTypeInformation
    decodeProvider
    renderProviderName
    R.typeInformation
    where
      decodeProvider t = case parseProviderName t of
        Just p  -> Right p
        Nothing -> Left ("unknown provider: " <> T.unpack t)

instance R.DBEq ProviderName

-- ----------------------------------------------------------------------------
-- JSONB-stored domain types. These rely on existing ToJSON / FromJSON
-- instances from the Domain modules.
-- ----------------------------------------------------------------------------

deriving via R.JSONBEncoded LeagueScoring instance R.DBType LeagueScoring
deriving via R.JSONBEncoded RosterLimits  instance R.DBType RosterLimits
deriving via R.JSONBEncoded LineupLimits  instance R.DBType LineupLimits