{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Ranking where

import Data.Aeson
    ( FromJSON,
      ToJSON,
      FromJSON(..),
      Result(Success),
      Value,
      decode,
      eitherDecodeStrict,
      fromJSON,
      withObject,
      (.!=),
      (.:),
      (.:?) )

import Data.Time (
    Day,
    addDays,
    defaultTimeLocale,
    diffDays,
    formatTime,
    parseTimeOrError,
    parseTimeM
 )
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (FromJSON (..), Result (Success), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

getCurrentFormattedTime :: IO String
getCurrentFormattedTime = do
    currentTime <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" currentTime
    return formattedTime

-- | Represents the top-level ranking data structure.
data RankingData = RankingData
    { teamId        :: Text
    , dataChecksum  :: Text
    , lastUpdated   :: UTCTime
    , rankings      :: [PlayerRanking]
    } deriving (Show, Eq, Generic)

instance FromJSON RankingData where
    parseJSON = withObject "RankingData" $ \v -> do
        teamId <- v .: "teamId"
        dataChecksum <- v .: "dataChecksum"
        lastUpdatedStr <- v .: "lastUpdated"
        lastUpdated <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" lastUpdatedStr
        rankings <- v .: "rankings"
        return RankingData{..}

instance ToJSON RankingData

-- Represents a player's ranking within the team.
data PlayerRanking = PlayerRanking
    { playerId :: Int
    , rank     :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON PlayerRanking
instance ToJSON PlayerRanking