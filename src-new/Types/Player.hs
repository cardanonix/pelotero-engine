{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Player where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToRow (ToRow(..), toRow)
import Database.PostgreSQL.Simple.ToField (toField)
import GHC.Generics (Generic)

data Player = Player
  { playerId        :: !Int
  , useName         :: !Text
  , useLastName     :: !Text
  , nameSlug        :: !Text
  , currentTeam     :: !Int
  , primaryPosition :: !Text
  , batSide         :: !Text
  , pitchHand       :: !Text
  , active          :: !Bool
  } deriving (Show, Eq, Generic)

instance FromRow Player where
  fromRow = Player
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

instance ToRow Player where
  toRow Player{..} =
    [ toField playerId
    , toField useName
    , toField useLastName
    , toField nameSlug
    , toField currentTeam
    , toField primaryPosition
    , toField batSide
    , toField pitchHand
    , toField active
    ]

-- Metadata we track ourselves, not from MLB
data PlayerFetchRecord = PlayerFetchRecord
  { fetchSeason   :: !Int
  , fetchedAt     :: !UTCTime
  , fetchChecksum :: !Text
  , playerCount   :: !Int
  } deriving (Show, Eq, Generic)

instance FromRow PlayerFetchRecord where
  fromRow = PlayerFetchRecord
    <$> field
    <*> field
    <*> field
    <*> field

instance ToRow PlayerFetchRecord where
  toRow PlayerFetchRecord{..} =
    [ toField fetchSeason
    , toField fetchedAt
    , toField fetchChecksum
    , toField playerCount
    ]