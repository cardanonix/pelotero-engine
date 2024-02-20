{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Ranking where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

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