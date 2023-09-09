{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}

module InputADT where

import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), Result (Success), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V

-- Top level data type
data GameData = GameData
  { teams :: Teams
  }
  deriving (Show, Eq)

-- JSON instances
instance FromJSON GameData where
  parseJSON :: Value -> Parser GameData
  parseJSON = withObject "GameData" $ \v ->
    GameData <$> v .: "teams"

-- Teams data type
data Teams = Teams
  { away :: TeamData,
    home :: TeamData
  }
  deriving (Show, Eq)

instance FromJSON Teams where
  parseJSON :: Value -> Parser Teams
  parseJSON = withObject "Teams" $ \v ->
    Teams
      <$> v
      .: "away"
      <*> v
      .: "home"

data TeamData = TeamData
  { players :: M.Map Text Player
  }
  deriving (Show, Eq)

hasValidPositions :: Value -> Bool
hasValidPositions val = case fromJSON val :: Result Player of
  Success player -> case allPositions player of
    Just positions -> not (null positions)
    Nothing -> False
  _ -> False

instance FromJSON TeamData where
  parseJSON :: Value -> Parser TeamData
  parseJSON = withObject "TeamData" $ \v -> do
    playersMap <- v .: "players" :: Parser (M.Map Text Value)
    let maybePlayersList =
          map
            ( \(k, v) ->
                if hasValidPositions v
                  then case fromJSON v of
                    Success player -> Just (k, player)
                    _ -> Nothing
                  else Nothing
            )
            (M.toList playersMap)

    let validPlayers = M.fromList $ catMaybes maybePlayersList
    pure TeamData {players = validPlayers}

type Players = [(Text, Player)]

-- Player data structure
data Player = Player
  { person :: Person,
    parentTeamId :: Int,
    allPositions :: Maybe [Position],
    status :: Status,
    stats :: PlayerStats
  }
  deriving (Show, Eq)

instance FromJSON Player where
  parseJSON :: Value -> Parser Player
  parseJSON = withObject "Player" $ \v -> do
    person <- v .: "person"
    teamId <- v .: "parentTeamId"
    positions <- v .:? "allPositions"
    let validPositions = case positions of
          Just ps -> if null ps then Nothing else Just ps
          Nothing -> Nothing
    status <- v .: "status"
    stats <- v .: "stats"
    return $ Player person teamId validPositions status stats

data Person = Person
  { personId :: Int,
    fullName :: Text
  }
  deriving (Show, Eq)

instance FromJSON Person where
  parseJSON :: Value -> Parser Person
  parseJSON = withObject "Person" $ \v ->
    Person
      <$> v
      .: "id"
      <*> v
      .: "fullName"

data Position = Position
  { pos_code :: Text
  }
  deriving (Show, Eq)

instance FromJSON Position where
  parseJSON :: Value -> Parser Position
  parseJSON = withObject "Position" $ \v ->
    Position
      <$> v
      .: "code"

data Status = Status
  { status_code :: Text
  }
  deriving (Show, Eq)

instance FromJSON Status where
  parseJSON :: Value -> Parser Status
  parseJSON = withObject "Status" $ \v ->
    Status
      <$> v
      .: "code"

data PlayerStats = PlayerStats
  { batting :: Maybe BattingStats,
    pitching :: Maybe PitchingStats
  }
  deriving (Show, Eq)

instance FromJSON PlayerStats where
  parseJSON :: Value -> Parser PlayerStats
  parseJSON = withObject "PlayerStats" $ \v ->
    PlayerStats
      <$> v .:? "batting"
      <*> v .:? "pitching"

data BattingStats = BattingStats
  { bat_gamesPlayed :: Maybe Int,
    bat_flyOuts :: Maybe Int,
    bat_groundOuts :: Maybe Int,
    bat_runs :: Maybe Int,
    bat_doubles :: Maybe Int,
    bat_triples :: Maybe Int,
    bat_homeRuns :: Maybe Int,
    bat_strikeOuts :: Maybe Int,
    bat_baseOnBalls :: Maybe Int,
    bat_intentionalWalks :: Maybe Int,
    bat_hits :: Maybe Int,
    bat_hitByPitch :: Maybe Int,
    bat_atBats :: Maybe Int,
    bat_caughtStealing :: Maybe Int,
    bat_stolenBases :: Maybe Int,
    bat_groundIntoDoublePlay :: Maybe Int,
    bat_groundIntoTriplePlay :: Maybe Int,
    bat_plateAppearances :: Maybe Int,
    bat_totalBases :: Maybe Int,
    bat_rbi :: Maybe Int,
    bat_leftOnBase :: Maybe Int,
    bat_sacBunts :: Maybe Int,
    bat_sacFlies :: Maybe Int,
    bat_catchersInterference :: Maybe Int,
    bat_pickoffs :: Maybe Int
  }
  deriving (Show, Eq)

instance FromJSON BattingStats where
  parseJSON :: Value -> Parser BattingStats
  parseJSON = withObject "BattingStats" $ \v ->
    BattingStats
      <$> v .:? "gamesPlayed"
      <*> v .:? "flyOuts"
      <*> v .:? "groundOuts"
      <*> v .:? "runs"
      <*> v .:? "doubles"
      <*> v .:? "triples"
      <*> v .:? "homeRuns"
      <*> v .:? "strikeOuts"
      <*> v .:? "baseOnBalls"
      <*> v .:? "intentionalWalks"
      <*> v .:? "hits"
      <*> v .:? "hitByPitch"
      <*> v .:? "atBats"
      <*> v .:? "caughtStealing"
      <*> v .:? "stolenBases"
      <*> v .:? "groundIntoDoublePlay"
      <*> v .:? "groundIntoTriplePlay"
      <*> v .:? "plateAppearances"
      <*> v .:? "totalBases"
      <*> v .:? "rbi"
      <*> v .:? "leftOnBase"
      <*> v .:? "sacBunts"
      <*> v .:? "sacFlies"
      <*> v .:? "catchersInterference"
      <*> v .:? "pickoffs"

data PitchingStats = PitchingStats
  { pit_gamesPlayed :: Maybe Int,
    pit_gamesStarted :: Maybe Int,
    pit_flyOuts :: Maybe Int,
    pit_groundOuts :: Maybe Int,
    pit_airOuts :: Maybe Int,
    pit_runs :: Maybe Int,
    pit_doubles :: Maybe Int,
    pit_triples :: Maybe Int,
    pit_homeRuns :: Maybe Int,
    pit_strikeOuts :: Maybe Int,
    pit_baseOnBalls :: Maybe Int,
    pit_intentionalWalks :: Maybe Int,
    pit_hits :: Maybe Int,
    pit_hitByPitch :: Maybe Int,
    pit_atBats :: Maybe Int,
    pit_caughtStealing :: Maybe Int,
    pit_stolenBases :: Maybe Int,
    pit_numberOfPitches :: Maybe Int,
    pit_inningsPitched :: Maybe Text,
    pit_wins :: Maybe Int,
    pit_losses :: Maybe Int,
    pit_saves :: Maybe Int,
    pit_saveOpportunities :: Maybe Int,
    pit_holds :: Maybe Int,
    pit_blownSaves :: Maybe Int,
    pit_earnedRuns :: Maybe Int,
    pit_battersFaced :: Maybe Int,
    pit_outs :: Maybe Int,
    pit_gamesPitched :: Maybe Int,
    pit_completeGames :: Maybe Int,
    pit_shutouts :: Maybe Int,
    pit_pitchesThrown :: Maybe Int,
    pit_balls :: Maybe Int,
    pit_strikes :: Maybe Int,
    pit_hitBatsmen :: Maybe Int,
    pit_balks :: Maybe Int,
    pit_wildPitches :: Maybe Int,
    pit_pickoffs :: Maybe Int,
    pit_rbi :: Maybe Int,
    pit_gamesFinished :: Maybe Int,
    pit_inheritedRunners :: Maybe Int,
    pit_inheritedRunnersScored :: Maybe Int,
    pit_catchersInterference :: Maybe Int,
    pit_sacBunts :: Maybe Int,
    pit_sacFlies :: Maybe Int,
    pit_passedBall :: Maybe Int
  }
  deriving (Show, Eq)

instance FromJSON PitchingStats where
  parseJSON :: Value -> Parser PitchingStats
  parseJSON = withObject "PitchingStats" $ \v ->
    PitchingStats
      <$> v .:? "gamesPlayed"
      <*> v .:? "gamesStarted"
      <*> v .:? "flyOuts"
      <*> v .:? "groundOuts"
      <*> v .:? "airOuts"
      <*> v .:? "runs"
      <*> v .:? "doubles"
      <*> v .:? "triples"
      <*> v .:? "homeRuns"
      <*> v .:? "strikeOuts"
      <*> v .:? "baseOnBalls"
      <*> v .:? "intentionalWalks"
      <*> v .:? "hits"
      <*> v .:? "hitByPitch"
      <*> v .:? "atBats"
      <*> v .:? "caughtStealing"
      <*> v .:? "stolenBases"
      <*> v .:? "numberOfPitches"
      <*> v .:? "inningsPitched"
      <*> v .:? "wins"
      <*> v .:? "losses"
      <*> v .:? "saves"
      <*> v .:? "saveOpportunities"
      <*> v .:? "holds"
      <*> v .:? "blownSaves"
      <*> v .:? "earnedRuns"
      <*> v .:? "battersFaced"
      <*> v .:? "outs"
      <*> v .:? "gamesPitched"
      <*> v .:? "completeGames"
      <*> v .:? "shutouts"
      <*> v .:? "pitchesThrown"
      <*> v .:? "balls"
      <*> v .:? "strikes"
      <*> v .:? "hitBatsmen"
      <*> v .:? "balks"
      <*> v .:? "wildPitches"
      <*> v .:? "pickoffs"
      <*> v .:? "rbi"
      <*> v .:? "gamesFinished"
      <*> v .:? "inheritedRunners"
      <*> v .:? "inheritedRunnersScored"
      <*> v .:? "catchersInterference"
      <*> v .:? "sacBunts"
      <*> v .:? "sacFlies"
      <*> v .:? "passedBall"

-- ## Schedule ADT's ##
data GameSchedule where
  GameSchedule :: {dates :: [DateEntry]} -> GameSchedule
  deriving (Show, Eq)

data DateEntry where
  DateEntry :: {games :: Maybe (V.Vector GameID)} -> DateEntry
  deriving (Show, Eq)

data GameID where
  GameID :: {gamePk :: Int} -> GameID
  deriving (Show, Eq)

instance Data.Aeson.FromJSON GameSchedule where
    parseJSON = Data.Aeson.withObject "GameSchedule" $ \v -> GameSchedule
        <$> v Data.Aeson..: "dates"

instance Data.Aeson.FromJSON DateEntry where
    parseJSON = Data.Aeson.withObject "DateEntry" $ \v -> DateEntry
        <$> v Data.Aeson..:? "games"

instance Data.Aeson.FromJSON GameID where
    parseJSON = Data.Aeson.withObject "GameID" $ \v -> GameID
        <$> v Data.Aeson..: "gamePk"

-- ## Game Status ADT's ##
data LiveGameStatus where
  LiveGameStatus :: {codedGameState :: Text} -> LiveGameStatus
  deriving (Show, Eq)

instance FromJSON LiveGameStatus where
    parseJSON = withObject "LiveGameStatus" $ \v -> LiveGameStatus
        <$> v .: "codedGameState"

data LiveGameWrapper where
  LiveGameWrapper :: {gameData :: LiveGameStatus} -> LiveGameWrapper
  deriving (Show, Eq)

instance FromJSON LiveGameWrapper where
    parseJSON = withObject "LiveGameWrapper" $ \v -> LiveGameWrapper
        <$> v .: "gameData"