{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OriginalDataTypes where

import Data.Aeson
import Data.Int (Int64)
import GHC.Generics (Generic)

data OriginalGameData = OriginalGameData
  { gamePk :: Int64
  , gameStatus :: String
  , awayPlayers :: [Player]
  , homePlayers :: [Player]
  } deriving (Show, Eq, Generic)

instance ToJSON OriginalGameData where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON OriginalGameData where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data Player = Player
  { person :: Person
  , parentTeamId :: Int
  , allPositions :: [Position]
  , status :: StatusCode
  , stats :: Stats
  } deriving (Show, Eq, Generic)

instance ToJSON Player where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON Player where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data Person = Person
  { id :: Int
  , fullName :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Person where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON Person where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data Position = Position
  { code :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Position where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON Position where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data StatusCode = StatusCode
  { code :: String
  } deriving (Show, Eq, Generic)

instance ToJSON StatusCode where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON StatusCode where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data Stats = Stats
  { batting :: Maybe BattingStats
  , pitching :: Maybe PitchingStats
  } deriving (Show, Eq, Generic)

instance ToJSON Stats where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON Stats where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}