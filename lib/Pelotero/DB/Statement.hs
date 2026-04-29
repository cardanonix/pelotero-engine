module Pelotero.DB.Statement
  (
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

  , encDbPlayerId
  , decDbPlayerId
  , encDbTeamId
  , decDbTeamId
  , encDbTeamIdMaybe
  , decDbTeamIdMaybe
  , encDbGameId
  , decDbGameId

  , encDbLeagueConfigId
  , decDbLeagueConfigId
  , encDbLeagueTeamId
  , decDbLeagueTeamId
  , encDbLeagueTeamIdMaybe
  , decDbLeagueTeamIdMaybe
  , encDbDraftPickId
  , decDbDraftPickId

  , encJsonb
  , decJsonb

  , encProvider
  , encProviderMaybe
  , decProviderTolerant
  , decProviderMaybe
  ) where

import           Data.Bifunctor             (first)
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (chr)
import           Data.Functor.Contravariant ((>$<))
import           Data.Int                   (Int32, Int64)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (Day, UTCTime)
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import qualified Data.Aeson                 as Aeson

import Pelotero.Domain.Id   (DbDraftPickId(..), DbGameId(..), DbLeagueConfigId(..)
                            , DbLeagueTeamId(..), DbPlayerId(..), DbTeamId(..))
import Pelotero.DB.Provider (ProviderName, parseProviderName, renderProviderName)

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

encChar1Maybe :: E.Params (Maybe Char)
encChar1Maybe = fmap (T.pack . pure) >$< encTextMaybe

encDay :: E.Params Day
encDay = E.param (E.nonNullable E.date)

encUTCTime :: E.Params UTCTime
encUTCTime = E.param (E.nonNullable E.timestamptz)

encUTCTimeMaybe :: E.Params (Maybe UTCTime)
encUTCTimeMaybe = E.param (E.nullable E.timestamptz)

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

encProvider :: E.Params ProviderName
encProvider = renderProviderName >$< encText

encProviderMaybe :: E.Params (Maybe ProviderName)
encProviderMaybe = fmap renderProviderName >$< encTextMaybe

decProviderTolerant :: D.Row (Maybe ProviderName)
decProviderTolerant = parseProviderName <$> decText

decProviderMaybe :: D.Row (Maybe ProviderName)
decProviderMaybe = (>>= parseProviderName) <$> decTextMaybe

encDbLeagueConfigId :: E.Params DbLeagueConfigId
encDbLeagueConfigId = unDbLeagueConfigId >$< encInt64

decDbLeagueConfigId :: D.Row DbLeagueConfigId
decDbLeagueConfigId = DbLeagueConfigId <$> decInt64

encDbLeagueTeamId :: E.Params DbLeagueTeamId
encDbLeagueTeamId = unDbLeagueTeamId >$< encInt64

decDbLeagueTeamId :: D.Row DbLeagueTeamId
decDbLeagueTeamId = DbLeagueTeamId <$> decInt64

encDbLeagueTeamIdMaybe :: E.Params (Maybe DbLeagueTeamId)
encDbLeagueTeamIdMaybe = fmap unDbLeagueTeamId >$< encInt64Maybe

decDbLeagueTeamIdMaybe :: D.Row (Maybe DbLeagueTeamId)
decDbLeagueTeamIdMaybe = fmap DbLeagueTeamId <$> decInt64Maybe

encDbDraftPickId :: E.Params DbDraftPickId
encDbDraftPickId = unDbDraftPickId >$< encInt64

decDbDraftPickId :: D.Row DbDraftPickId
decDbDraftPickId = DbDraftPickId <$> decInt64

-- JSONB: encode goes lazy -> strict, decode parses strict bytes inside the
-- decoder itself so a malformed payload becomes a hasql RowError, not an
-- impure crash via 'error'.
encJsonb :: Aeson.ToJSON a => E.Params a
encJsonb = (BL.toStrict . Aeson.encode) >$< E.param (E.nonNullable E.jsonbBytes)

decJsonb :: Aeson.FromJSON a => D.Row a
decJsonb = D.column (D.nonNullable (D.jsonbBytes parse))
  where
    parse = first (\err -> T.pack ("JSONB decode failure: " <> err))
          . Aeson.eitherDecodeStrict