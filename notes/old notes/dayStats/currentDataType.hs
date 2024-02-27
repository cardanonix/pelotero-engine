{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import Data.Aeson
import Data.Int (Int64)
import GHC.Generics (Generic)

data PlayerStats = PlayerStats
  { playerId :: Int
  , fullName :: String
  , parentTeamId :: Int
  , allPositions :: [Int]
  , status :: String
  , batting :: Maybe BattingStats
  , pitching :: Maybe PitchingStats
  } deriving (Show, Eq, Generic)

instance ToJSON PlayerStats where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON PlayerStats where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data Stats = Stats
  { gameId :: Int64
  , gameStats :: GameStats
  } deriving (Show, Eq, Generic)

instance ToJSON Stats where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON Stats where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data GameStats = GameStats
  { parentTeamId :: Int64
  , allPositions :: [Int]
  , status :: String
  , batting :: Maybe BattingStats
  , pitching :: Maybe PitchingStats
  } deriving (Show, Eq, Generic)

instance ToJSON GameStats where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON GameStats where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data BattingStats = BattingStats
  { gamesPlayed :: Int
  , flyOuts :: Int
  , groundOuts :: Int
  , runs :: Int
  , doubles :: Int
  , triples :: Int
  , homeRuns :: Int
  , strikeOuts :: Int
  , baseOnBalls :: Int
  , intentionalWalks :: Int
  , hits :: Int
  , hitByPitch :: Int
  , atBats :: Int
  , caughtStealing :: Int
  , stolenBases :: Int
  , groundIntoDoublePlay :: Int
  , groundIntoTriplePlay :: Int
  , plateAppearances :: Int
  , totalBases :: Int
  , rbi :: Int
  , leftOnBase :: Int
  , sacBunts :: Int
  , sacFlies :: Int
  , catchersInterference :: Int
  , pickoffs :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON BattingStats where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON BattingStats where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data PitchingStats = PitchingStats
  { gamesPlayed :: Int
  , gamesStarted :: Int
  , groundOuts :: Int
  , airOuts :: Int
  , runs :: Int
  , doubles :: Int
  , triples :: Int
  , homeRuns :: Int
  , strikeOuts :: Int
  , baseOnBalls :: Int
  , intentionalWalks :: Int
  , hits :: Int
  , hitByPitch :: Int
  , atBats :: Int
  , caughtStealing :: Int
  , stolenBases :: Int
  , numberOfPitches :: Int
  , inningsPitched :: String
  , wins :: Int
  , losses :: Int
  , saves :: Int
  , saveOpportunities :: Int
  , holds :: Int
  , blownSaves :: Int
  , earnedRuns :: Int
  , battersFaced :: Int
  , outs :: Int
  , gamesPitched :: Int
  , completeGames :: Int
  , shutouts :: Int
  , pitchesThrown :: Int
  , balls :: Int
  , strikes :: Int
  , hitBatsmen :: Int
  , balks :: Int
  , wildPitches :: Int
  , pickoffs :: Int
  , rbi :: Int
  , gamesFinished :: Int
  , inheritedRunners :: Int
  , inheritedRunnersScored :: Int
  , catchersInterference :: Int
  , sacBunts :: Int
  , sacFlies :: Int
  , passedBall :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON PitchingStats where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON PitchingStats where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
