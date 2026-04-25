-- | Wire shape for @\/api\/v1\/schedule\/games\/?...@. The MLB API nests
-- games under date entries, which we mirror here. Convert to the flat
-- "Pelotero.Domain.Game.GameSchedule" in "Pelotero.MLB.Convert".
module Pelotero.MLB.Wire.Schedule
  ( WireScheduleEnvelope(..)
  , WireDateEntry(..)
  , WireGame(..)
  , WireGameTeams(..)
  , WireGameTeam(..)
  ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import Data.Text (Text)

newtype WireScheduleEnvelope = WireScheduleEnvelope
  { wseDates :: [WireDateEntry] }
  deriving stock (Show, Eq)

instance FromJSON WireScheduleEnvelope where
  parseJSON = withObject "WireScheduleEnvelope" $ \o ->
    WireScheduleEnvelope <$> o .: "dates"

data WireDateEntry = WireDateEntry
  { wdeDate  :: Text          -- "YYYY-MM-DD"
  , wdeGames :: Maybe [WireGame]
  }
  deriving stock (Show, Eq)

instance FromJSON WireDateEntry where
  parseJSON = withObject "WireDateEntry" $ \o -> WireDateEntry
    <$> o .:  "date"
    <*> o .:? "games"

data WireGame = WireGame
  { wgGamePk :: Int
  , wgTeams  :: Maybe WireGameTeams
  }
  deriving stock (Show, Eq)

instance FromJSON WireGame where
  parseJSON = withObject "WireGame" $ \o -> WireGame
    <$> o .:  "gamePk"
    <*> o .:? "teams"

data WireGameTeams = WireGameTeams
  { wgtAway :: Maybe WireGameTeam
  , wgtHome :: Maybe WireGameTeam
  }
  deriving stock (Show, Eq)

instance FromJSON WireGameTeams where
  parseJSON = withObject "WireGameTeams" $ \o -> WireGameTeams
    <$> o .:? "away"
    <*> o .:? "home"

newtype WireGameTeam = WireGameTeam
  { wgtTeamId :: Maybe Int }
  deriving stock (Show, Eq)

instance FromJSON WireGameTeam where
  parseJSON = withObject "WireGameTeam" $ \o -> do
    teamObj <- o .:? "team"
    case teamObj of
      Nothing -> pure (WireGameTeam Nothing)
      Just t  -> WireGameTeam <$> withObject "team" (\to -> to .:? "id") t