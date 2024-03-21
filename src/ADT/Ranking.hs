{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Ranking where

import Data.Aeson
    ( ToJSON,
      FromJSON(..),
      Result(Success),
      Value,
      decode,
      fromJSON,
      withObject,
      (.!=),
      (.:),
      eitherDecodeStrict,
      (.!=),
      (.:?),
      object,
      (.=) 
    )
import Data.Time (
    Day,
    defaultTimeLocale,
    diffDays,
    formatTime,
    parseTimeM
 )
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Aeson.Types (toJSON)


-- | Represents the top-level ranking data structure.
data RankingData = RankingData
    { teamId        :: Text
    , dataChecksum  :: Text
    , lastUpdated   :: UTCTime
    , rankings      :: [PlayerRanking]
    } deriving (Show, Eq, Generic)

-- Represents a collection of player rankings, possibly empty
type PlayerRankings = [PlayerRanking]

-- Creates an empty collection of player rankings
mkEmptyRankings :: PlayerRankings
mkEmptyRankings = []

-- Represents a player's ranking within the team.
data PlayerRanking = PlayerRanking
    { playerId :: Int
    , rank     :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON PlayerRanking

instance FromJSON RankingData where
    parseJSON = withObject "RankingData" $ \v -> do
        teamId <- v .: "teamId"
        dataChecksum <- v .: "dataChecksum"
        lastUpdatedStr <- v .: "lastUpdated"
        lastUpdated <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" lastUpdatedStr
        rankings <- v .: "rankings"
        return RankingData{..}

instance ToJSON RankingData where
    toJSON (RankingData teamId dataChecksum lastUpdated rankings) =
        object [ "teamId"        .= teamId
               , "dataChecksum"  .= dataChecksum
               , "lastUpdated"   .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" lastUpdated
               , "rankings"      .= rankings
               ]

-- Custom ToJSON instance for PlayerRanking
instance ToJSON PlayerRanking where
    toJSON (PlayerRanking playerId rank) =
        object [ "playerId" .= playerId
               , "rank"     .= rank
               ]