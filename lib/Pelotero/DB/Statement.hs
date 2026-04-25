-- | Shared encoder and decoder primitives for repository modules.
--
-- Hasql's @Encoders@ and @Decoders@ APIs are flexible but verbose. Most
-- columns we care about are one of: required scalar, optional scalar,
-- timestamp, or surrogate-key wrapper. The helpers here name those four
-- patterns once, so individual repositories read like data dictionaries
-- rather than parser definitions.
module Pelotero.DB.Statement
  ( -- * Encoders for individual values
    encInt64
  , encInt64Maybe
  , encInt32Maybe
  , encText
  , encTextMaybe
  , encBool
  , encChar1Maybe
  , encDay
  , encUTCTime
  , encUTCTimeMaybe
    -- * Decoders for individual columns
  , decInt64
  , decInt64Maybe
  , decInt32Maybe
  , decText
  , decTextMaybe
  , decBool
  , decChar1Maybe
  , decDay
  , decUTCTime
  , decUTCTimeMaybe
    -- * Surrogate-key helpers
  , encDbPlayerId
  , decDbPlayerId
  , encDbTeamId
  , decDbTeamId
  , encDbTeamIdMaybe
  , decDbTeamIdMaybe
  , encDbGameId
  , decDbGameId
    -- * Provider helpers
  , encProvider
  , encProviderMaybe
  , decProviderTolerant
  , decProviderMaybe
  ) where

import Data.Char                  (chr)
import Data.Functor.Contravariant ((>$<))
import Data.Int                   (Int32, Int64)
import Data.Text                  (Text)
import qualified Data.Text as T
import Data.Time                  (Day, UTCTime)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import Pelotero.DB.Provider (ProviderName, parseProviderName, renderProviderName)
import Pelotero.Domain.Id   (DbGameId(..), DbPlayerId(..), DbTeamId(..))

--------------------------------------------------------------------------------
-- Plain encoders

encInt64 :: E.Params Int64
encInt64 = E.param (E.nonNullable E.int8)

encInt64Maybe :: E.Params (Maybe Int64)
encInt64Maybe = E.param (E.nullable E.int8)

encInt32Maybe :: E.Params (Maybe Int32)
encInt32Maybe = E.param (E.nullable E.int4)

encText :: E.Params Text
encText = E.param (E.nonNullable E.text)

encTextMaybe :: E.Params (Maybe Text)
encTextMaybe = E.param (E.nullable E.text)

encBool :: E.Params Bool
encBool = E.param (E.nonNullable E.bool)

-- | Encode @CHAR(1)@ from 'Maybe Char'. Postgres wants 'Text' on the wire.
encChar1Maybe :: E.Params (Maybe Char)
encChar1Maybe = fmap (T.pack . pure) >$< encTextMaybe

encDay :: E.Params Day
encDay = E.param (E.nonNullable E.date)

encUTCTime :: E.Params UTCTime
encUTCTime = E.param (E.nonNullable E.timestamptz)

encUTCTimeMaybe :: E.Params (Maybe UTCTime)
encUTCTimeMaybe = E.param (E.nullable E.timestamptz)

--------------------------------------------------------------------------------
-- Plain decoders (one column within a row)

decInt64 :: D.Row Int64
decInt64 = D.column (D.nonNullable D.int8)

decInt64Maybe :: D.Row (Maybe Int64)
decInt64Maybe = D.column (D.nullable D.int8)

decInt32Maybe :: D.Row (Maybe Int32)
decInt32Maybe = D.column (D.nullable D.int4)

decText :: D.Row Text
decText = D.column (D.nonNullable D.text)

decTextMaybe :: D.Row (Maybe Text)
decTextMaybe = D.column (D.nullable D.text)

decBool :: D.Row Bool
decBool = D.column (D.nonNullable D.bool)

-- | Decode @CHAR(1)@ as 'Maybe Char'. Empty string collapses to 'Nothing';
-- multi-character strings shouldn't reach us (the schema CHECKs single-char),
-- but if they did we'd take the first character.
decChar1Maybe :: D.Row (Maybe Char)
decChar1Maybe = fmap unpackChar1 <$> decTextMaybe
  where
    unpackChar1 t = case T.uncons t of
      Just (c, _) -> c
      Nothing     -> chr 0

decDay :: D.Row Day
decDay = D.column (D.nonNullable D.date)

decUTCTime :: D.Row UTCTime
decUTCTime = D.column (D.nonNullable D.timestamptz)

decUTCTimeMaybe :: D.Row (Maybe UTCTime)
decUTCTimeMaybe = D.column (D.nullable D.timestamptz)

--------------------------------------------------------------------------------
-- Surrogate-key helpers

encDbPlayerId :: E.Params DbPlayerId
encDbPlayerId = unDbPlayerId >$< encInt64

decDbPlayerId :: D.Row DbPlayerId
decDbPlayerId = DbPlayerId <$> decInt64

encDbTeamId :: E.Params DbTeamId
encDbTeamId = unDbTeamId >$< encInt64

decDbTeamId :: D.Row DbTeamId
decDbTeamId = DbTeamId <$> decInt64

encDbTeamIdMaybe :: E.Params (Maybe DbTeamId)
encDbTeamIdMaybe = fmap unDbTeamId >$< encInt64Maybe

decDbTeamIdMaybe :: D.Row (Maybe DbTeamId)
decDbTeamIdMaybe = fmap DbTeamId <$> decInt64Maybe

encDbGameId :: E.Params DbGameId
encDbGameId = unDbGameId >$< encInt64

decDbGameId :: D.Row DbGameId
decDbGameId = DbGameId <$> decInt64

--------------------------------------------------------------------------------
-- Provider helpers

encProvider :: E.Params ProviderName
encProvider = renderProviderName >$< encText

encProviderMaybe :: E.Params (Maybe ProviderName)
encProviderMaybe = fmap renderProviderName >$< encTextMaybe

-- | Decode a non-null provider column tolerantly: returns 'Nothing' if the
-- text doesn't parse to a known 'ProviderName'. Use for forward-compat in
-- columns where the schema guarantees non-null but the application doesn't
-- want to crash on an unrecognised value.
decProviderTolerant :: D.Row (Maybe ProviderName)
decProviderTolerant = parseProviderName <$> decText

-- | Decode a nullable provider column. NULL → 'Nothing'; non-NULL but
-- unrecognised → 'Nothing'; recognised → 'Just'.
decProviderMaybe :: D.Row (Maybe ProviderName)
decProviderMaybe = (>>= parseProviderName) <$> decTextMaybe