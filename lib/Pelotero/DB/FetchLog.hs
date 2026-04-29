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

module Pelotero.DB.FetchLog
  ( FetchLogRow(..)
  , recordFetchT
  , getLastFetchT
  , getRecentFetchesT
  , recordFetch
  , getLastFetch
  , getRecentFetches
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Int                   (Int32, Int64)
import           Data.Maybe                 (mapMaybe)
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
                                            , (&&.)
                                            )
import qualified Rel8                       as R

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Provider  (ProviderName, parseProviderName, renderProviderName)
import Pelotero.DB.Rel8Instances ()

-- ============================================================================
-- provider_fetch_log
--
-- We decode the `provider` column as Text (not ProviderName) so an unknown
-- provider value in the database doesn't crash queries; we parse and filter
-- in fromResult. This preserves the tolerant-decode behavior of the old
-- hasql module's decProviderTolerant.
-- ============================================================================

data FetchLogE f = FetchLogE
  { _flId            :: Column f Int64
  , _flProvider      :: Column f Text
  , _flResource      :: Column f Text
  , _flScope         :: Column f Text
  , _flFetchedAt     :: Column f UTCTime
  , _flPayloadSha256 :: Column f Text
  , _flRecordCount   :: Column f Int32
  }
  deriving stock    (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (FetchLogE f)
deriving stock instance f ~ Result => Eq   (FetchLogE f)

fetchLogSchema :: TableSchema (FetchLogE Name)
fetchLogSchema = TableSchema
  { name    = "provider_fetch_log"
  , columns = FetchLogE
      { _flId            = "id"
      , _flProvider      = "provider"
      , _flResource      = "resource"
      , _flScope         = "scope"
      , _flFetchedAt     = "fetched_at"
      , _flPayloadSha256 = "payload_sha256"
      , _flRecordCount   = "record_count"
      }
  }

-- ============================================================================
-- Public row type (API compatibility with old hasql module)
-- ============================================================================

data FetchLogRow = FetchLogRow
  { fetchLogId            :: !(Maybe Int64)
  , fetchLogProvider      :: !ProviderName
  , fetchLogResource      :: !Text
  , fetchLogScope         :: !Text
  , fetchLogFetchedAt     :: !(Maybe UTCTime)
  , fetchLogPayloadSha256 :: !Text
  , fetchLogRecordCount   :: !Int32
  }
  deriving stock (Show, Eq)

-- | Convert a row from the DB. Returns Nothing if the provider string
-- doesn't parse as a known ProviderName (tolerant decoding).
fromResult :: FetchLogE Result -> Maybe FetchLogRow
fromResult FetchLogE{..} = do
  prov <- parseProviderName _flProvider
  pure FetchLogRow
    { fetchLogId            = Just _flId
    , fetchLogProvider      = prov
    , fetchLogResource      = _flResource
    , fetchLogScope         = _flScope
    , fetchLogFetchedAt     = Just _flFetchedAt
    , fetchLogPayloadSha256 = _flPayloadSha256
    , fetchLogRecordCount   = _flRecordCount
    }

-- ============================================================================
-- Transaction-flavored CRUD
-- ============================================================================

recordFetchT :: FetchLogRow -> Tx.Transaction ()
recordFetchT row = Tx.statement () $ R.run_ $ R.insert R.Insert
  { R.into       = fetchLogSchema
  , R.rows       = R.values
      [ FetchLogE
          { _flId            = R.unsafeDefault
          , _flProvider      = R.lit (renderProviderName (fetchLogProvider row))
          , _flResource      = R.lit (fetchLogResource row)
          , _flScope         = R.lit (fetchLogScope row)
          , _flFetchedAt     = R.unsafeDefault
          , _flPayloadSha256 = R.lit (fetchLogPayloadSha256 row)
          , _flRecordCount   = R.lit (fetchLogRecordCount row)
          }
      ]
  , R.onConflict = R.Abort
  , R.returning  = R.NoReturning
  }

getLastFetchT
  :: ProviderName -> Text -> Text -> Tx.Transaction (Maybe FetchLogRow)
getLastFetchT provider resource scope = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.limit 1 $
      R.orderBy ((_flFetchedAt >$< R.desc) <> (_flId >$< R.desc)) $ do
        r <- R.each fetchLogSchema
        R.where_
          (   _flProvider r ==. R.lit (renderProviderName provider)
          &&. _flResource r ==. R.lit resource
          &&. _flScope    r ==. R.lit scope
          )
        pure r
  pure $ case mapMaybe fromResult rows of
    (x : _) -> Just x
    []      -> Nothing

getRecentFetchesT
  :: ProviderName -> Text -> Int32 -> Tx.Transaction [FetchLogRow]
getRecentFetchesT provider resource lim = do
  rows <- Tx.statement () $ R.run $ R.select $
    R.limit (fromIntegral lim) $
      R.orderBy ((_flFetchedAt >$< R.desc) <> (_flId >$< R.desc)) $ do
        r <- R.each fetchLogSchema
        R.where_
          (   _flProvider r ==. R.lit (renderProviderName provider)
          &&. _flResource r ==. R.lit resource
          )
        pure r
  pure (mapMaybe fromResult rows)

-- ============================================================================
-- Pool-flavored CRUD
-- ============================================================================

recordFetch :: Pool -> FetchLogRow -> IO (Either DBError ())
recordFetch pool row = runTransaction pool (recordFetchT row)

getLastFetch
  :: Pool -> ProviderName -> Text -> Text -> IO (Either DBError (Maybe FetchLogRow))
getLastFetch pool provider resource scope =
  runTransaction pool (getLastFetchT provider resource scope)

getRecentFetches
  :: Pool -> ProviderName -> Text -> Int32 -> IO (Either DBError [FetchLogRow])
getRecentFetches pool provider resource lim =
  runTransaction pool (getRecentFetchesT provider resource lim)