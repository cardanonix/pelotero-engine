{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module PlayerRanking where

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
      (.=),
      withScientific )


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
import qualified OfficialRoster as O
import Data.Scientific (toBoundedInteger)

-- | Represents the top-level ranking data structure.
data RankingData = RankingData
    { teamId        :: Text
    , dataChecksum  :: Text
    , lastUpdated   :: UTCTime
    , rankings      :: [PlayerRanking]
    } deriving (Show, Eq, Generic)

-- Represents a player's ranking within the team.
data PlayerRanking = PlayerRanking
    { playerId :: O.PlayerID
    , rank     :: Int
    } deriving (Show, Eq, Generic)

-- -- Represents a collection of player rankings, possibly empty
type PlayerRankings = [PlayerRanking]

-- Creates an empty collection of player rankings
mkEmptyRankings :: PlayerRankings
mkEmptyRankings = []

instance FromJSON PlayerRanking where
    parseJSON = withObject "PlayerRanking" $ \v -> do
        playerId <- v .: "playerId"
        rank     <- v .: "rank"
        return PlayerRanking{..}

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

--Draft Ordering Strategies
type DraftOrderStrategy = Int -> [Text] -> [Text]

serpentineOrderStrategy :: DraftOrderStrategy
serpentineOrderStrategy totalPicks teams =
    let rounds = totalPicks `div` length teams
    in concat $ take rounds $ cycle [teams, reverse teams]

experimentalSnakeStrategy :: DraftOrderStrategy
experimentalSnakeStrategy totalPicks teams = 
    let rounds = totalPicks `div` length teams
        patternLength = 4 -- The pattern repeats every 4 rounds
        generateRound n
            | n `mod` patternLength == 1 = teams
            | n `mod` patternLength == 2 = take (length teams) . drop (length teams `div` 2) $ cycle teams
            | n `mod` patternLength == 3 = reverse teams
            | otherwise = reverse $ take (length teams) . drop (length teams `div` 2) $ cycle teams
    in concatMap generateRound [1..rounds]

selectDraftOrderStrategy :: Text -> DraftOrderStrategy
selectDraftOrderStrategy orderType = case orderType of
    "serpentine" -> serpentineOrderStrategy
    "experimental_snake" -> experimentalSnakeStrategy
    _ -> serpentineOrderStrategy -- Default