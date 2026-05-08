{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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