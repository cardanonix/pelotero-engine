-- | The on-the-wire shape of MLB's @\/api\/v1\/sports\/1\/players@ endpoint.
-- These types exist solely to parse the JSON; they're never used for business
-- logic. Conversion to "Pelotero.Domain.Player" happens in
-- "Pelotero.MLB.Convert".
module Pelotero.MLB.Wire.Player
  ( WirePlayerEnvelope(..)
  , WirePlayer(..)
  , WireTeamRef(..)
  , WirePositionRef(..)
  , WireHandRef(..)
  ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import Data.Text (Text)

-- | Top-level response: @{ "people": [ ... ] }@.
newtype WirePlayerEnvelope = WirePlayerEnvelope
  { wirePlayers :: [WirePlayer] }
  deriving stock (Show, Eq)

instance FromJSON WirePlayerEnvelope where
  parseJSON = withObject "WirePlayerEnvelope" $ \o ->
    WirePlayerEnvelope <$> o .: "people"

-- | A single player record from the roster feed. Every field except @id@ and
-- @active@ is optional in practice — MLB ships partial records during early
-- spring training.
data WirePlayer = WirePlayer
  { wpId              :: Int
  , wpUseName         :: Maybe Text
  , wpUseLastName     :: Maybe Text
  , wpNameSlug        :: Maybe Text
  , wpCurrentTeam     :: Maybe WireTeamRef
  , wpPrimaryPosition :: Maybe WirePositionRef
  , wpBatSide         :: Maybe WireHandRef
  , wpPitchHand       :: Maybe WireHandRef
  , wpActive          :: Bool
  }
  deriving stock (Show, Eq)

instance FromJSON WirePlayer where
  parseJSON = withObject "WirePlayer" $ \o -> WirePlayer
    <$> o .:  "id"
    <*> o .:? "useName"
    <*> o .:? "useLastName"
    <*> o .:? "nameSlug"
    <*> o .:? "currentTeam"
    <*> o .:? "primaryPosition"
    <*> o .:? "batSide"
    <*> o .:? "pitchHand"
    <*> o .:  "active"

-- | Embedded team reference: @{ "id": 117, "name": "Houston Astros", ... }@.
data WireTeamRef = WireTeamRef
  { wtrId   :: Int
  , wtrName :: Maybe Text
  }
  deriving stock (Show, Eq)

instance FromJSON WireTeamRef where
  parseJSON = withObject "WireTeamRef" $ \o -> WireTeamRef
    <$> o .:  "id"
    <*> o .:? "name"

-- | Embedded position reference: @{ "code": "5", "abbreviation": "3B", ... }@.
data WirePositionRef = WirePositionRef
  { wprCode         :: Maybe Text
  , wprAbbreviation :: Maybe Text
  }
  deriving stock (Show, Eq)

instance FromJSON WirePositionRef where
  parseJSON = withObject "WirePositionRef" $ \o -> WirePositionRef
    <$> o .:? "code"
    <*> o .:? "abbreviation"

-- | Embedded hand reference: @{ "code": "L", "description": "Left" }@.
newtype WireHandRef = WireHandRef
  { whrCode :: Maybe Text }
  deriving stock (Show, Eq)

instance FromJSON WireHandRef where
  parseJSON = withObject "WireHandRef" $ \o -> WireHandRef
    <$> o .:? "code"