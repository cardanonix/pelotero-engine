{-# LANGUAGE OverloadedStrings #-}

-- | Provider-id <-> Text conversions.
--
-- Single source of truth for how upstream identifiers are encoded into the
-- external_id column. Every site that previously did
-- @T.pack . show . unTeamId@ (or the @PlayerId@/@GameId@ equivalent) goes
-- through this module instead.
--
-- The encoding is intentionally identical to the legacy @show . un*Id@
-- form: existing external_id rows must remain readable. The
-- 'externalId*Roundtrip' properties in the test suite pin this down.
module Pelotero.Provider.ExternalId
  ( -- * Encoding (Id -> Text)
    externalIdFromTeamId
  , externalIdFromPlayerId
  , externalIdFromGameId

    -- * Decoding (Text -> Maybe Id)
  , externalIdToTeamId
  , externalIdToPlayerId
  , externalIdToGameId
  ) where

import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.Read  as TR

import           Pelotero.Domain.Id
                     ( GameId (..)
                     , PlayerId (..)
                     , TeamId (..)
                     )

--------------------------------------------------------------------------------
-- Encoding

externalIdFromTeamId :: TeamId -> Text
externalIdFromTeamId = T.pack . show . unTeamId

externalIdFromPlayerId :: PlayerId -> Text
externalIdFromPlayerId = T.pack . show . unPlayerId

externalIdFromGameId :: GameId -> Text
externalIdFromGameId = T.pack . show . unGameId

--------------------------------------------------------------------------------
-- Decoding

externalIdToTeamId :: Text -> Maybe TeamId
externalIdToTeamId = fmap TeamId . parseIntStrict

externalIdToPlayerId :: Text -> Maybe PlayerId
externalIdToPlayerId = fmap PlayerId . parseIntStrict

externalIdToGameId :: Text -> Maybe GameId
externalIdToGameId = fmap GameId . parseIntStrict

--------------------------------------------------------------------------------
-- Internal

-- | Parse an 'Int' from 'Text', requiring the entire input to be consumed.
--
-- 'TR.signed' is used because @show@ of a negative 'Int' produces a leading
-- @\'-\'@; the encoding/decoding round-trip must include negatives even
-- though wire ids are positive in practice (negative ids are filtered at
-- the wire boundary in 'Pelotero.MLB.Convert.convertPlayer').
parseIntStrict :: Text -> Maybe Int
parseIntStrict t = case TR.signed TR.decimal t of
  Right (n, rest) | T.null rest -> Just n
  _                             -> Nothing