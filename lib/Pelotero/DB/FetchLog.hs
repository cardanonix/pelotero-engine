-- | Repository for the @provider_fetch_log@ table.
--
-- Every successful sync run from an external provider is recorded here:
-- which provider, which resource ("rosters", "schedule", "boxscore", ...),
-- which scope (a season year, a date, a game id — opaque text), when it
-- happened, the SHA-256 of the payload, and how many records the payload
-- contained.
--
-- Two read paths matter:
--
--   * 'getLastFetchT' — used by sync to skip work when the upstream payload
--     hasn't changed since last run.
--   * 'getRecentFetchesT' — operational visibility.
--
-- A row whose @provider@ column doesn't parse to a recognised
-- 'ProviderName' is silently filtered from results. This can only happen
-- after rolling back a deploy that introduced a new provider; rather than
-- crashing the read path, we treat such rows as inert.
module Pelotero.DB.FetchLog
  ( -- * Row type
    FetchLogRow(..)
    -- * Transaction-level API
  , recordFetchT
  , getLastFetchT
  , getRecentFetchesT
    -- * Pool/IO API (wrappers)
  , recordFetch
  , getLastFetch
  , getRecentFetches
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int                   (Int32, Int64)
import Data.Text                  (Text)
import Data.Time                  (UTCTime)
import qualified Data.Vector as V
import qualified Hasql.Decoders   as D
import qualified Hasql.Encoders   as E
import qualified Hasql.Statement  as Stmt
import qualified Hasql.Transaction as Tx

import Pelotero.DB.Pool      (DBError, Pool, runTransaction)
import Pelotero.DB.Provider  (ProviderName)
import Pelotero.DB.Statement

--------------------------------------------------------------------------------
-- Row type

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

--------------------------------------------------------------------------------
-- Transaction-level API

recordFetchT :: FetchLogRow -> Tx.Transaction ()
recordFetchT row = Tx.statement (toFieldsTuple row) insertStmt

getLastFetchT
  :: ProviderName -> Text -> Text -> Tx.Transaction (Maybe FetchLogRow)
getLastFetchT provider resource scope =
  Tx.statement (provider, resource, scope) selectLastStmt

getRecentFetchesT
  :: ProviderName -> Text -> Int32 -> Tx.Transaction [FetchLogRow]
getRecentFetchesT provider resource lim =
  V.toList <$> Tx.statement (provider, resource, lim) selectRecentStmt

--------------------------------------------------------------------------------
-- Pool/IO API

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

--------------------------------------------------------------------------------
-- Field tuple and encoders

type FetchLogFields =
  ( ProviderName
  , Text
  , Text
  , Text
  , Int32
  )

toFieldsTuple :: FetchLogRow -> FetchLogFields
toFieldsTuple FetchLogRow{..} =
  ( fetchLogProvider
  , fetchLogResource
  , fetchLogScope
  , fetchLogPayloadSha256
  , fetchLogRecordCount
  )

fetchLogFieldsEncoder :: E.Params FetchLogFields
fetchLogFieldsEncoder =
     ((\(a,_,_,_,_) -> a) >$< encProvider)
  <> ((\(_,b,_,_,_) -> b) >$< encText)
  <> ((\(_,_,c,_,_) -> c) >$< encText)
  <> ((\(_,_,_,d,_) -> d) >$< encText)
  <> ((\(_,_,_,_,e) -> e) >$< encInt32)

-- Local helpers (Int32 codecs not yet hoisted to Statement.hs).
encInt32 :: E.Params Int32
encInt32 = E.param (E.nonNullable E.int4)

decInt32 :: D.Row Int32
decInt32 = D.column (D.nonNullable D.int4)

--------------------------------------------------------------------------------
-- Row decoder
--
-- Yields 'Nothing' if the provider column doesn't parse. Public API filters
-- these out via 'V.mapMaybe' / @join@.

rowDecoderTolerant :: D.Row (Maybe FetchLogRow)
rowDecoderTolerant = do
  rid       <- decInt64
  prov      <- decProviderTolerant
  resource  <- decText
  scope     <- decText
  ts        <- decUTCTime
  sha       <- decText
  cnt       <- decInt32
  pure $ flip fmap prov $ \p -> FetchLogRow
    { fetchLogId            = Just rid
    , fetchLogProvider      = p
    , fetchLogResource      = resource
    , fetchLogScope         = scope
    , fetchLogFetchedAt     = Just ts
    , fetchLogPayloadSha256 = sha
    , fetchLogRecordCount   = cnt
    }

--------------------------------------------------------------------------------
-- Statements

insertStmt :: Stmt.Statement FetchLogFields ()
insertStmt = Stmt.Statement sql fetchLogFieldsEncoder D.noResult True
  where
    sql = "INSERT INTO provider_fetch_log \
          \  (provider, resource, scope, payload_sha256, record_count) \
          \VALUES ($1, $2, $3, $4, $5)"

selectLastStmt :: Stmt.Statement (ProviderName, Text, Text) (Maybe FetchLogRow)
selectLastStmt = Stmt.Statement sql encoder decoder True
  where
    sql = "SELECT id, provider, resource, scope, fetched_at, payload_sha256, record_count \
          \FROM provider_fetch_log \
          \WHERE provider = $1 AND resource = $2 AND scope = $3 \
          \ORDER BY fetched_at DESC, id DESC \
          \LIMIT 1"
    encoder =
         ((\(a,_,_) -> a) >$< encProvider)
      <> ((\(_,b,_) -> b) >$< encText)
      <> ((\(_,_,c) -> c) >$< encText)
    decoder = (>>= id) <$> D.rowMaybe rowDecoderTolerant

selectRecentStmt :: Stmt.Statement (ProviderName, Text, Int32) (V.Vector FetchLogRow)
selectRecentStmt = Stmt.Statement sql encoder decoder True
  where
    sql = "SELECT id, provider, resource, scope, fetched_at, payload_sha256, record_count \
          \FROM provider_fetch_log \
          \WHERE provider = $1 AND resource = $2 \
          \ORDER BY fetched_at DESC, id DESC \
          \LIMIT $3"
    encoder =
         ((\(a,_,_) -> a) >$< encProvider)
      <> ((\(_,b,_) -> b) >$< encText)
      <> ((\(_,_,c) -> c) >$< encInt32)
    decoder = V.mapMaybe id <$> D.rowVector rowDecoderTolerant