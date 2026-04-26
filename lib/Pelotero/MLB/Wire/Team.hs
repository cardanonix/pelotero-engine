-- | Wire shape for @\/api\/v1\/teams?sportId=1&season=N@. Used by the
-- roster sync to populate full team metadata before player records
-- reference team ids.
module Pelotero.MLB.Wire.Team
  ( WireTeamEnvelope(..)
  , WireTeam(..)
  ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import Data.Text (Text)

newtype WireTeamEnvelope = WireTeamEnvelope
  { wireTeams :: [WireTeam] }
  deriving stock (Show, Eq)

instance FromJSON WireTeamEnvelope where
  parseJSON = withObject "WireTeamEnvelope" $ \o ->
    WireTeamEnvelope <$> o .: "teams"

-- | A team record from the @teams@ endpoint. The fields we store in the
-- @team@ table are @name@ (full official name), @abbreviation@, and
-- @locationName@ (the city). Other fields the endpoint returns — venue,
-- league, division, etc. — are ignored here.
data WireTeam = WireTeam
  { wtId           :: Int
  , wtName         :: Text
  , wtAbbreviation :: Text
  , wtLocationName :: Maybe Text
    -- ^ Sometimes absent for short-season or international teams; we
    -- default to empty string at conversion time.
  }
  deriving stock (Show, Eq)

instance FromJSON WireTeam where
  parseJSON = withObject "WireTeam" $ \o -> WireTeam
    <$> o .:  "id"
    <*> o .:  "name"
    <*> o .:  "abbreviation"
    <*> o .:? "locationName"