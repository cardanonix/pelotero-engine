{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MLB.Parse
  ( MLBRosterResponse(..)
  , MLBPlayer(..)
  , mlbPlayerToPlayer
  , mlbResponseToPlayers
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Types.Player (Player(..))

-- Raw MLB API shapes. These exist only to parse the wire format.
-- They never escape this module.

data MLBRosterResponse = MLBRosterResponse
  { mlbPeople :: [MLBPlayer]
  } deriving (Show)

instance FromJSON MLBRosterResponse where
  parseJSON = withObject "MLBRosterResponse" $ \v ->
    MLBRosterResponse <$> v .: "people"

data MLBPlayer = MLBPlayer
  { mlbPlayerId        :: !Int
  , mlbUseName         :: !(Maybe Text)
  , mlbUseLastName     :: !(Maybe Text)
  , mlbNameSlug        :: !(Maybe Text)
  , mlbCurrentTeam     :: !(Maybe Int)
  , mlbPrimaryPosition :: !(Maybe Text)
  , mlbBatSide         :: !(Maybe Text)
  , mlbPitchHand       :: !(Maybe Text)
  , mlbActive          :: !Bool
  } deriving (Show)

instance FromJSON MLBPlayer where
  parseJSON = withObject "MLBPlayer" $ \v -> do
    mlbPlayerId        <- v .:  "id"
    mlbUseName         <- v .:? "useName"
    mlbUseLastName     <- v .:? "useLastName"
    mlbNameSlug        <- v .:? "nameSlug"
    mlbCurrentTeam     <- v .:? "currentTeam" >>= traverse (.: "id")
    mlbPrimaryPosition <- v .:? "primaryPosition" >>= traverse (.: "code")
    mlbBatSide         <- v .:? "batSide" >>= traverse (.: "code")
    mlbPitchHand       <- v .:? "pitchHand" >>= traverse (.: "code")
    mlbActive          <- v .:  "active"
    pure MLBPlayer{..}

-- Convert wire type to our domain type.
-- Defaults for missing fields rather than Maybe everywhere.
mlbPlayerToPlayer :: MLBPlayer -> Player
mlbPlayerToPlayer MLBPlayer{..} = Player
  { playerId        = mlbPlayerId
  , useName         = fromMaybe "" mlbUseName
  , useLastName     = fromMaybe "" mlbUseLastName
  , nameSlug        = fromMaybe "" mlbNameSlug
  , currentTeam     = fromMaybe 0  mlbCurrentTeam
  , primaryPosition = fromMaybe "" mlbPrimaryPosition
  , batSide         = fromMaybe "" mlbBatSide
  , pitchHand       = fromMaybe "" mlbPitchHand
  , active          = mlbActive
  }

mlbResponseToPlayers :: MLBRosterResponse -> [Player]
mlbResponseToPlayers = map mlbPlayerToPlayer . mlbPeople