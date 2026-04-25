-- Start of /home/bismuth/git/pelotero-engine/notes/old notes/dayStats/sourceDataType.hs
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
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}-- End of /home/bismuth/git/pelotero-engine/notes/old notes/dayStats/sourceDataType.hs

-- Start of /home/bismuth/git/pelotero-engine/notes/old notes/dayStats/currentDataType.hs
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
-- End of /home/bismuth/git/pelotero-engine/notes/old notes/dayStats/currentDataType.hs

-- Start of /home/bismuth/git/pelotero-engine/test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}-- End of /home/bismuth/git/pelotero-engine/test/Spec.hs

-- Start of /home/bismuth/git/pelotero-engine/src-new/Types/Player.hs
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
    ]-- End of /home/bismuth/git/pelotero-engine/src-new/Types/Player.hs

-- Start of /home/bismuth/git/pelotero-engine/src-new/MLB/Client.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MLB.Client
  ( fetchActiveRoster
  , MLBError(..)
  ) where

import Control.Exception (try, SomeException, evaluate)
import qualified Data.ByteString as B
import Data.Aeson (eitherDecodeStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Simple
    ( parseRequest_
    , httpBS
    , getResponseBody
    , getResponseStatusCode
    )
import System.IO (hPutStrLn, stderr)
import Types.Player (Player)
import MLB.Parse (MLBRosterResponse, mlbResponseToPlayers)

data MLBError
  = HttpError String
  | ParseError String
  | ApiError Int String
  deriving (Show)

rosterUrl :: Int -> String
rosterUrl season =
  "https://statsapi.mlb.com/api/v1/sports/1/players?activeStatus=ACTIVE&season="
    ++ show season

fetchActiveRoster :: Int -> IO (Either MLBError [Player])
fetchActiveRoster season = do
  hPutStrLn stderr $ "Fetching active roster for season " ++ show season ++ "..."
  result <- try $ do
    let req = parseRequest_ (rosterUrl season)
    response <- httpBS req
    let status = getResponseStatusCode response
        body   = getResponseBody response
    -- Force evaluation so exceptions surface here
    _ <- evaluate (B.length body)
    pure (status, body)

  case result of
    Left (e :: SomeException) ->
      pure $ Left $ HttpError (show e)
    Right (status, body)
      | status /= 200 ->
          pure $ Left $ ApiError status ("HTTP " ++ show status)
      | otherwise ->
          case eitherDecodeStrict body of
            Left err ->
              pure $ Left $ ParseError err
            Right roster -> do
              let players = mlbResponseToPlayers roster
              hPutStrLn stderr $ "Parsed " ++ show (length players) ++ " players"
              pure $ Right players-- End of /home/bismuth/git/pelotero-engine/src-new/MLB/Client.hs

-- Start of /home/bismuth/git/pelotero-engine/src-new/MLB/Parse.hs
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
mlbResponseToPlayers = map mlbPlayerToPlayer . mlbPeople-- End of /home/bismuth/git/pelotero-engine/src-new/MLB/Parse.hs

-- Start of /home/bismuth/git/pelotero-engine/src-new/DB/Migration.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB.Migration
  ( runMigrations
  ) where

import qualified Data.Pool as Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import DB.Connection (withConnection)
import System.IO (hPutStrLn, stderr)

runMigrations :: Pool.Pool Connection -> IO ()
runMigrations pool = withConnection pool $ \conn -> do
  hPutStrLn stderr "Running migrations..."

  _ <- execute_ conn
    [sql|
      CREATE TABLE IF NOT EXISTS player (
        player_id        INTEGER PRIMARY KEY,
        use_name         TEXT NOT NULL,
        use_last_name    TEXT NOT NULL,
        name_slug        TEXT NOT NULL,
        current_team     INTEGER NOT NULL,
        primary_position TEXT NOT NULL,
        bat_side         TEXT NOT NULL,
        pitch_hand       TEXT NOT NULL,
        active           BOOLEAN NOT NULL,
        created_at       TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
        updated_at       TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
      )
    |]

  _ <- execute_ conn
    [sql|
      CREATE TABLE IF NOT EXISTS player_fetch_log (
        id             SERIAL PRIMARY KEY,
        season         INTEGER NOT NULL,
        fetched_at     TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
        checksum       TEXT NOT NULL,
        player_count   INTEGER NOT NULL
      )
    |]

  -- We'll add more tables here as we go:
  -- game_schedule, game_stats, batting_stats, pitching_stats,
  -- league_config, team, roster, lineup, player_ranking, etc.

  hPutStrLn stderr "Migrations complete."-- End of /home/bismuth/git/pelotero-engine/src-new/DB/Migration.hs

-- Start of /home/bismuth/git/pelotero-engine/src-new/DB/Player.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module DB.Player
  ( upsertPlayer
  , upsertPlayers
  , getAllPlayers
  , getPlayerById
  , getPlayersByTeam
  , getPlayersByPosition
  , getActivePlayers
  , logFetch
  , getLastFetch
  ) where

import Control.Monad (void, forM_)
import qualified Data.Pool as Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import DB.Connection (withConnection)
import Types.Player

-- Upsert: insert or update on conflict. This is the key operation --
-- we just blast the full roster in and let Postgres figure out what changed.
upsertPlayer :: Connection -> Player -> IO ()
upsertPlayer conn Player{..} =
  void $ execute conn
    [sql|
      INSERT INTO player
        (player_id, use_name, use_last_name, name_slug,
         current_team, primary_position, bat_side, pitch_hand, active,
         updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, NOW())
      ON CONFLICT (player_id) DO UPDATE SET
        use_name         = EXCLUDED.use_name,
        use_last_name    = EXCLUDED.use_last_name,
        name_slug        = EXCLUDED.name_slug,
        current_team     = EXCLUDED.current_team,
        primary_position = EXCLUDED.primary_position,
        bat_side         = EXCLUDED.bat_side,
        pitch_hand       = EXCLUDED.pitch_hand,
        active           = EXCLUDED.active,
        updated_at       = NOW()
    |]
    ( playerId, useName, useLastName, nameSlug
    , currentTeam, primaryPosition, batSide, pitchHand, active
    )

upsertPlayers :: Pool.Pool Connection -> [Player] -> IO ()
upsertPlayers pool players = withConnection pool $ \conn -> do
  -- Wrap in a transaction for atomicity and speed
  withTransaction conn $
    forM_ players (upsertPlayer conn)

getAllPlayers :: Pool.Pool Connection -> IO [Player]
getAllPlayers pool = withConnection pool $ \conn ->
  query_ conn
    [sql|
      SELECT player_id, use_name, use_last_name, name_slug,
             current_team, primary_position, bat_side, pitch_hand, active
      FROM player
      ORDER BY use_last_name, use_name
    |]

getPlayerById :: Pool.Pool Connection -> Int -> IO (Maybe Player)
getPlayerById pool pid = withConnection pool $ \conn -> do
  results <- query conn
    [sql|
      SELECT player_id, use_name, use_last_name, name_slug,
             current_team, primary_position, bat_side, pitch_hand, active
      FROM player
      WHERE player_id = ?
    |]
    (Only pid)
  pure $ case results of
    [p] -> Just p
    _   -> Nothing

getPlayersByTeam :: Pool.Pool Connection -> Int -> IO [Player]
getPlayersByTeam pool teamId = withConnection pool $ \conn ->
  query conn
    [sql|
      SELECT player_id, use_name, use_last_name, name_slug,
             current_team, primary_position, bat_side, pitch_hand, active
      FROM player
      WHERE current_team = ?
      ORDER BY primary_position, use_last_name
    |]
    (Only teamId)

getPlayersByPosition :: Pool.Pool Connection -> Text -> IO [Player]
getPlayersByPosition pool pos = withConnection pool $ \conn ->
  query conn
    [sql|
      SELECT player_id, use_name, use_last_name, name_slug,
             current_team, primary_position, bat_side, pitch_hand, active
      FROM player
      WHERE primary_position = ?
      ORDER BY use_last_name
    |]
    (Only pos)

getActivePlayers :: Pool.Pool Connection -> IO [Player]
getActivePlayers pool = withConnection pool $ \conn ->
  query_ conn
    [sql|
      SELECT player_id, use_name, use_last_name, name_slug,
             current_team, primary_position, bat_side, pitch_hand, active
      FROM player
      WHERE active = TRUE
      ORDER BY use_last_name, use_name
    |]

logFetch :: Pool.Pool Connection -> Int -> Text -> Int -> IO ()
logFetch pool season checksum count = withConnection pool $ \conn ->
  void $ execute conn
    [sql|
      INSERT INTO player_fetch_log (season, checksum, player_count)
      VALUES (?, ?, ?)
    |]
    (season, checksum, count)

getLastFetch :: Pool.Pool Connection -> Int -> IO (Maybe PlayerFetchRecord)
getLastFetch pool season = withConnection pool $ \conn -> do
  results <- query conn
    [sql|
      SELECT season, fetched_at, checksum, player_count
      FROM player_fetch_log
      WHERE season = ?
      ORDER BY fetched_at DESC
      LIMIT 1
    |]
    (Only season)
  pure $ case results of
    [r] -> Just r
    _   -> Nothing-- End of /home/bismuth/git/pelotero-engine/src-new/DB/Player.hs

-- Start of /home/bismuth/git/pelotero-engine/src-new/DB/Connection.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB.Connection
  ( DBConfig(..)
  , initializeDB
  , withConnection
  , defaultConfig
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, throwIO)
import qualified Data.Pool as Pool
import Database.PostgreSQL.Simple
import System.IO (hPutStrLn, stderr)
import System.Posix.User (getLoginName)

data DBConfig = DBConfig
  { dbHost     :: String
  , dbPort     :: Int
  , dbName     :: String
  , dbUser     :: String
  , dbPassword :: String
  , poolSize   :: Int
  }

defaultConfig :: IO DBConfig
defaultConfig = do
  user <- getLoginName
  pure DBConfig
    { dbHost     = "localhost"
    , dbPort     = 5432
    , dbName     = "fantasy_league"
    , dbUser     = user
    , dbPassword = "postgres"
    , poolSize   = 5
    }

initializeDB :: DBConfig -> IO (Pool.Pool Connection)
initializeDB config = do
  let poolConfig =
        Pool.defaultPoolConfig
          (connectWithRetry config)
          close
          0.5
          (fromIntegral $ poolSize config)
  pool <- Pool.newPool poolConfig
  Pool.withResource pool $ \conn -> do
    _ <- query_ conn "SELECT 1" :: IO [Only Int]
    pure ()
  pure pool

connectWithRetry :: DBConfig -> IO Connection
connectWithRetry DBConfig{..} = go 5
  where
    go :: Int -> IO Connection
    go 0 = error "Failed to connect to database after 5 attempts"
    go n = do
      let connInfo = defaultConnectInfo
            { connectHost     = dbHost
            , connectPort     = fromIntegral dbPort
            , connectDatabase = dbName
            , connectUser     = dbUser
            , connectPassword = dbPassword
            }
      catch (connect connInfo) $ \e -> do
        hPutStrLn stderr $ "Connection failed (" ++ show n ++ " retries left): " ++ show (e :: SqlError)
        threadDelay 2000000
        go (n - 1)

withConnection :: Pool.Pool Connection -> (Connection -> IO a) -> IO a
withConnection = Pool.withResource-- End of /home/bismuth/git/pelotero-engine/src-new/DB/Connection.hs

-- Start of /home/bismuth/git/pelotero-engine/src/FetchActiveRoster.hs
module Main (main) where

import Data.ByteString.Lazy.Char8 ( pack )
import System.Environment ( getArgs )
import Data.Csv (ToNamedRecord, namedRecord, (.=))
import Data.Aeson
import Data.Aeson.Types ( Parser, Result(..) )


import Data.ByteString (ByteString)
import qualified Data.Csv as Csv
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.Map.Strict as M


import Control.Monad ( filterM, mzero )
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Debug.Trace (traceShowM)


import Input
import Middle
import Scraper
import Conversion
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (yearStr:_) -> case readMaybe yearStr of
            Just year -> processYear year
            Nothing -> putStrLn "Error: First argument must be a year (integer)."
        _ -> putStrLn "Error: Please provide a year as the first argument."

processYear :: Int -> IO ()
processYear year = do
    let rosterPath = "appData/rosters/" ++ show year ++ "_activePlayers.json"
    activeRoster <- fetchActiveRoster year -- now using the year parameter
    case activeRoster of
        Left err -> putStrLn $ "Failed to fetch active roster: " ++ err
        Right rosterData -> writeRosterToFile rosterPath rosterData

    jsonData <- BL.readFile rosterPath

    -- Parse JSON data
    let decodedData = eitherDecode jsonData :: Either String PlayersFile

    case decodedData of
        Left err -> putStrLn err
        Right parsedData -> do
            -- Convert HashMap to List
            let playersList = HM.elems $ officialPlayers parsedData

            -- Convert to CSV
            let csvData = Csv.encodeDefaultOrderedByName playersList
            let csvPath = "appData/rosters/" ++ show year ++ "_activePlayers.csv"
            BL.writeFile csvPath csvData
-- End of /home/bismuth/git/pelotero-engine/src/FetchActiveRoster.hs

-- Start of /home/bismuth/git/pelotero-engine/src/Test.hs
module Main (main) where

import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), Result (Success), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShowM)
import Input_trace (GameData)

main :: IO ()
main = do
    jsonData <- B.readFile "testFiles/716896_boxscore_modified.json"
    let parsedResult = eitherDecodeStrict jsonData :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
    handPicked <- B.readFile "testFiles/shortened.json"
    let parsedResult = eitherDecodeStrict handPicked :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
-- End of /home/bismuth/git/pelotero-engine/src/Test.hs

-- Start of /home/bismuth/git/pelotero-engine/src/FetchStats.hs
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (filterM)
import Data.Aeson (
    FromJSON (..),
    Result (Success),
    Value,
    decode,
    eitherDecodeStrict,
    fromJSON,
    withObject,
    (.!=),
    (.:),
    (.:?),
 )
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShowM)
import Scraper (scrapeStatsForDateRange)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [startDate, endDate] -> scrapeStatsForDateRange startDate endDate
        _ -> putStrLn "Usage: fetchStats <start-date> <end-date>"
-- End of /home/bismuth/git/pelotero-engine/src/FetchStats.hs

-- Start of /home/bismuth/git/pelotero-engine/src/LeagueTEST.hs
module Main (main) where

import Config as C
import Validators
import Utility

main :: IO ()
main = do
    parsedConfig <- readJson "testFiles/prototype_config/config.json" :: IO (Either String C.Configuration)
    let fileNames =
            [ "testFiles/prototype_config/valid_roster.json"
            , "testFiles/prototype_config/invalid_roster.json"
            , "testFiles/prototype_config/invalid_lineup.json"
            , "testFiles/appData/rosters/team_001.json"
            , "testFiles/appData/rosters/team_002.json"
            , "testFiles/appData/rosters/team_003.json" -- <-- too many outfielders
            , "testFiles/appData/rosters/team_004.json" -- <-- too many outfielders
            ]
    filesContent <- mapM (\path -> readJson path :: IO FileContent) fileNames

    case parsedConfig of
        Left err -> putStrLn $ "Failed to parse Config JSON: " ++ err
        Right config -> processConfigResults config (zip fileNames filesContent)
-- End of /home/bismuth/git/pelotero-engine/src/LeagueTEST.hs

-- Start of /home/bismuth/git/pelotero-engine/src/League/Draft.hs
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}

module Draft where

import Control.Monad (forM, foldM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Maybe (mapMaybe, fromMaybe, fromJust)
import Data.List (find, delete)
import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R
import qualified PlayerRanking as PR
import Validators (countPlayersOnRoster, findPlayer, queryDraftRosterLmts, queryLgLineupLmts)
import Utility

-- Configuration and state data structures confined to draft
data DraftConfig = DraftConfig {
    cfg :: C.Configuration,
    officialPlayers :: [O.OfficialPlayer]
}

data DraftState = DraftState {
    teams :: [R.LgManager],
    availablePlayerIds :: [O.PlayerID],
    draftHistory :: [(C.TeamID, O.PlayerID)],
    currentTeamIndex :: Int,
    draftOrder :: [(C.TeamID, Int)],
    draftComplete :: Bool,
    teamRankings :: [PR.RankingData]
} deriving (Show, Eq)

instantiateDraft :: C.Configuration -> O.OfficialRoster -> [PR.RankingData] -> IO DraftState
instantiateDraft config players rankings = do
    let teamIds = C.teamId config
        teamRankings = filter (\r -> PR.teamId r `elem` teamIds) rankings
    draftOrder <- generateDraftOrder config rankings
    let teams = map (\tid -> R.LgManager "active" (C.commissioner config) tid (C.leagueID config) mkEmptyLineup mkEmptyRoster) teamIds
    return DraftState {
        teams = teams,
        availablePlayerIds = map O.playerId $ O.people players,
        draftHistory = [],
        currentTeamIndex = 0,
        draftOrder = draftOrder,
        draftComplete = False,
        teamRankings = teamRankings
    }

draftPlayers :: DraftConfig -> DraftState -> IO DraftState
draftPlayers config state
    | draftComplete state = return state
    | otherwise = do
        let currentTeamOrder = draftOrder state !! currentTeamIndex state
            teamId = fst currentTeamOrder
        let (newState, maybeError) = draftCycle config state teamId
        case maybeError of
            Just err -> putStrLn ("Error: " ++ err) >> return state
            Nothing -> draftPlayers config newState

updateState :: DraftConfig -> DraftState -> O.OfficialPlayer -> C.TeamID -> DraftState
updateState config state player teamId = 
    let team = fromJust $ find (\t -> C.unwrapTeamID (R.teamId t) == C.unwrapTeamID teamId) (teams state)
        (newRosters, isNewPlayer) = addToRosterAndLineup (cfg config) player (R.roster team) (R.current_lineup team)
        newDraftHistory = (teamId, O.playerId player) : draftHistory state
        newAvailablePlayerIds = delete (O.playerId player) (availablePlayerIds state)
        newCurrentTeamIndex = (currentTeamIndex state + 1) `mod` length (draftOrder state)
        newTeams = map (\t -> if C.unwrapTeamID (R.teamId t) == C.unwrapTeamID teamId 
                              then t { R.roster = fst newRosters, R.current_lineup = snd newRosters } 
                              else t) (teams state)
    in if isNewPlayer then state { teams = newTeams, draftHistory = newDraftHistory, availablePlayerIds = newAvailablePlayerIds, currentTeamIndex = newCurrentTeamIndex }
       else state

draftCycle :: DraftConfig -> DraftState -> C.TeamID -> (DraftState, Maybe String)
draftCycle config state teamId =
    case lookup teamId [(tid, t) | t@(R.LgManager _ _ tid _ _ _) <- teams state] of
        Just teamState ->
            let availablePlayers = filter (\p -> O.playerId p `elem` availablePlayerIds state) (officialPlayers config)
                maybePlayer = selectNextPlayer teamId (teamRankings state) availablePlayers
            in case maybePlayer of
                Nothing -> (state { draftComplete = True }, Nothing)
                Just player -> 
                    let newState = updateState config state player teamId
                    in if newState == state
                       then (state, Just "Failed to add player to roster or lineup.")
                       else (newState, Nothing)
        Nothing -> (state, Just "Team not found")

runDraftCycle :: DraftConfig -> DraftState -> R.LgManager -> (DraftState, Maybe String)
runDraftCycle config state teamState = 
    draftCycle config state (R.teamId teamState)

selectNextPlayer :: C.TeamID -> [PR.RankingData] -> [O.OfficialPlayer] -> Maybe O.OfficialPlayer
selectNextPlayer teamId rankings availablePlayers =
    let teamRanking = find (\r -> PR.teamId r == teamId) rankings
        rankedPlayerIds = maybe [] (map PR.playerId . PR.rankings) teamRanking
    in find (\p -> O.playerId p `elem` rankedPlayerIds) availablePlayers

addToRosterAndLineup :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.CurrentLineup -> ((R.Roster, R.CurrentLineup), Bool)
addToRosterAndLineup config player roster lineup =
    let positionText = O.primaryPosition player
        draftPositionText = positionCodeToDraftText positionText
        draftLimits = C.draft_limits $ C.draft_parameters config
        lgLineupLimits = C.lineup_limits $ C.point_parameters config
    in if draftPositionText == "pitcher"
       then 
           let (updatedRoster, pitcherPosition) = addPitcherToRoster config player roster
               updatedLineup = if pitcherPosition /= ""
                               then addPlayerToLineup pitcherPosition player lineup lgLineupLimits
                               else lineup
           in ((updatedRoster, updatedLineup), True)
       else 
           let (updatedRoster, isAddedToRoster) = addBatterToRoster config draftPositionText player roster draftLimits
               updatedLineup = if isAddedToRoster
                               then addPlayerToLineup draftPositionText player lineup lgLineupLimits
                               else lineup
           in ((updatedRoster, updatedLineup), isAddedToRoster)

addPlayerToLineup :: T.Text -> O.OfficialPlayer -> R.CurrentLineup -> C.LgLineupLmts -> R.CurrentLineup
addPlayerToLineup position player lineup limits =
    let playerIdText = O.playerId player
    in case position of
        "catcher" -> if length (R.cC lineup) < C.lg_catcher limits then lineup { R.cC = playerIdText : R.cC lineup } else lineup
        "first" -> if length (R.b1C lineup) < C.lg_first limits then lineup { R.b1C = playerIdText : R.b1C lineup } else lineup
        "second" -> if length (R.b2C lineup) < C.lg_second limits then lineup { R.b2C = playerIdText : R.b2C lineup } else lineup
        "third" -> if length (R.b3C lineup) < C.lg_third limits then lineup { R.b3C = playerIdText : R.b3C lineup } else lineup
        "shortstop" -> if length (R.ssC lineup) < C.lg_shortstop limits then lineup { R.ssC = playerIdText : R.ssC lineup } else lineup
        "outfield" -> if length (R.ofC lineup) < C.lg_outfield limits then lineup { R.ofC = playerIdText : R.ofC lineup } else lineup
        "utility" -> if length (R.uC lineup) < C.lg_utility limits then lineup { R.uC = playerIdText : R.uC lineup } else lineup
        "s_pitcher" -> if length (R.spC lineup) < C.lg_s_pitcher limits then lineup { R.spC = playerIdText : R.spC lineup } else lineup
        "r_pitcher" -> if length (R.rpC lineup) < C.lg_r_pitcher limits then lineup { R.rpC = playerIdText : R.rpC lineup } else lineup
        _ -> lineup  

addPlayerToPosition :: T.Text -> O.OfficialPlayer -> R.Roster -> R.Roster
addPlayerToPosition position player roster =
    let playerIdText = O.playerId player
    in case position of
        "s_pitcher" -> roster { R.spR = playerIdText : R.spR roster }
        "r_pitcher" -> roster { R.rpR = playerIdText : R.rpR roster }
        "catcher" -> roster { R.cR = playerIdText : R.cR roster }
        "first" -> roster { R.b1R = playerIdText : R.b1R roster }
        "second" -> roster { R.b2R = playerIdText : R.b2R roster }
        "third" -> roster { R.b3R = playerIdText : R.b3R roster }
        "shortstop" -> roster { R.ssR = playerIdText : R.ssR roster }
        "outfield" -> roster { R.ofR = playerIdText : R.ofR roster }
        "utility" -> roster { R.uR = playerIdText : R.uR roster }
        _ -> roster

addPitcherToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> (R.Roster, T.Text)
addPitcherToRoster config player roster =
    let spLimit = queryDraftRosterLmts "s_pitcher" $ C.draft_limits $ C.draft_parameters config
        rpLimit = queryDraftRosterLmts "r_pitcher" $ C.draft_limits $ C.draft_parameters config
        spCount = length $ R.spR roster
        rpCount = length $ R.rpR roster
    in if spCount < spLimit
       then (addPlayerToPosition "s_pitcher" player roster, "s_pitcher")
       else if rpCount < rpLimit
            then (addPlayerToPosition "r_pitcher" player roster, "r_pitcher")
            else (roster, "")

addBatterToRoster :: C.Configuration -> T.Text -> O.OfficialPlayer -> R.Roster -> C.DraftRosterLmts -> (R.Roster, Bool)
addBatterToRoster config position player roster limits =
    let playerIdText = T.pack . show $ O.playerId player
        currentCount = countPlayersOnRoster position roster
        limit = queryDraftRosterLmts position limits
    in if currentCount < limit
       then (addPlayerToPosition position player roster, True)
       else (roster, False)
-- End of /home/bismuth/git/pelotero-engine/src/League/Draft.hs

-- Start of /home/bismuth/git/pelotero-engine/src/League/Conversion.hs
{-# LANGUAGE OverloadedStrings #-}

module Conversion where
import Data.Aeson ( FromJSON(parseJSON), Value(Object), (.:) )
import Data.Csv (ToNamedRecord, namedRecord, (.=))
import qualified Data.Csv as Csv
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import Control.Monad (mzero)

-- Player structure
data Player = Player
    { active :: Bool
    , batSide :: String
    , currentTeam :: Int
    , nameSlug :: String
    , pitchHand :: String
    , playerId :: Int
    , primaryPosition :: String
    , useLastName :: String
    , useName :: String
    } deriving (Show)

-- Define the JSON structure for the entire file
newtype PlayersFile
  = PlayersFile {officialPlayers :: HM.HashMap String Player}

instance FromJSON Player where
    parseJSON (Object v) =
        Player <$> v .: "active"
               <*> v .: "batSide"
               <*> v .: "currentTeam"
               <*> v .: "nameSlug"
               <*> v .: "pitchHand"
               <*> v .: "playerId"
               <*> v .: "primaryPosition"
               <*> v .: "useLastName"
               <*> v .: "useName"
    parseJSON _ = mzero

instance Csv.ToNamedRecord Player where
    toNamedRecord p = Csv.namedRecord
        [ TE.encodeUtf8 "active" Csv..= boolToString (active p)
        , TE.encodeUtf8 "batSide" Csv..= batSide p
        , TE.encodeUtf8 "currentTeam" Csv..= currentTeam p
        , TE.encodeUtf8 "nameSlug" Csv..= nameSlug p
        , TE.encodeUtf8 "pitchHand" Csv..= pitchHand p
        , TE.encodeUtf8 "playerId" Csv..= playerId p
        , TE.encodeUtf8 "primaryPosition" Csv..= primaryPosition p
        , TE.encodeUtf8 "useLastName" Csv..= useLastName p
        , TE.encodeUtf8 "useName" Csv..= useName p
        ]

instance Csv.DefaultOrdered Player where
    headerOrder _ = Csv.header
        [ "active"
        , "batSide"
        , "currentTeam"
        , "nameSlug"
        , "pitchHand"
        , "playerId"
        , "primaryPosition"
        , "useLastName"
        , "useName"
        ]

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

instance FromJSON PlayersFile where
    parseJSON (Object v) = PlayersFile <$> v .: "officialPlayers"
    parseJSON _ = mzero-- End of /home/bismuth/git/pelotero-engine/src/League/Conversion.hs

-- Start of /home/bismuth/git/pelotero-engine/src/League/Utility.hs
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Utility where

import Control.Monad (filterM, forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.Random (getRandomBytes)
import Data.Aeson (
  FromJSON (..),
  Result (Success),
  ToJSON (..),
  Value,
  decode,
  eitherDecodeStrict,
  encode,
  fromJSON,
  parseJSON,
  withObject,
  (.!=),
  (.:),
  (.:?),
  (.=),
 )
import Data.Aeson.Types (Pair, Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (foldl', forM_)
import Data.List (delete, find, nub, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Time (
  Day,
  addDays,
  defaultTimeLocale,
  diffDays,
  formatTime,
  parseTimeM,
  parseTimeOrError,
 )
import Data.Time.Clock
import Data.Time.Clock.POSIX ()
import Data.Time.Format (defaultTimeLocale, formatTime)
import Debug.Trace (traceShow, traceShowM)
import Network.HTTP.Simple (
  getResponseBody,
  httpBS,
  parseRequest_,
 )
import System.Random (StdGen, newStdGen, randomR)
import System.Random.Shuffle (shuffleM)

import Control.Exception (IOException, catch)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import GHC.Generics (Generic)

import qualified Config as C
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import qualified Input as I
import qualified OfficialRoster as O
import qualified PlayerRanking as PR
import qualified Points as P
import qualified Roster as R

positionTextToOfficialCode :: T.Text -> T.Text
positionTextToOfficialCode code =
  case code of
    "P" -> "1"
    "C" -> "2"
    "1B" -> "3"
    "2B" -> "4"
    "3B" -> "5"
    "SS" -> "6"
    "LF" -> "7"
    "CF" -> "8"
    "RF" -> "9"
    "U" -> "10"
    _ -> "Unknown"

positionCodeToText :: T.Text -> T.Text
positionCodeToText code =
  case code of
    "P" -> "pitcher"
    "C" -> "catcher"
    "1B" -> "first"
    "2B" -> "second"
    "3B" -> "third"
    "SS" -> "shortstop"
    "LF" -> "outfield"
    "CF" -> "outfield"
    "RF" -> "outfield"
    "DH" -> "utility"
    _ -> "Unknown"

positionCodeToOfficialText :: T.Text -> T.Text
positionCodeToOfficialText code =
  case code of
    "1" -> "P"
    "2" -> "C"
    "3" -> "1B"
    "4" -> "2B"
    "5" -> "3B"
    "6" -> "SS"
    "7" -> "LF"
    "8" -> "CF"
    "9" -> "RF"
    "10" -> "DH"
    "Y" -> "DH"
    _ -> "Unknown"

-- Corrected function for translating position codes to draft text
positionCodeToDraftText :: T.Text -> T.Text
positionCodeToDraftText code =
  let officialText = positionCodeToOfficialText code
   in officialTextToDraftText officialText
 where
  officialTextToDraftText :: T.Text -> T.Text
  officialTextToDraftText officialText =
    case officialText of
      "P" -> "pitcher"
      "C" -> "catcher"
      "1B" -> "first"
      "2B" -> "second"
      "3B" -> "third"
      "SS" -> "shortstop"
      "LF" -> "outfield"
      "CF" -> "outfield"
      "RF" -> "outfield"
      "DH" -> "utility"
      _ -> "Unknown"

-- generates a list of LgManager for each team ID provided in teamId.
mkLgManagers :: C.Configuration -> [R.LgManager]
mkLgManagers config =
  map (\tid -> mkSingleLgManager config (C.commissioner config) (C.leagueID config) tid) (C.teamId config)

-- helper function creates a single LgManager, given the teamId and other details.
-- Adjusted mkSingleLgManager to accept TeamID directly without changes
mkSingleLgManager :: C.Configuration -> Text -> Text -> C.TeamID -> R.LgManager
mkSingleLgManager config commissioner leagueID teamId =
  R.LgManager
    { R.status = "active"
    , R.commissioner = commissioner
    , R.teamId = teamId
    , R.leagueID = leagueID
    , R.current_lineup = mkEmptyLineup
    , R.roster = mkEmptyRoster
    }

-- | Generates a list of LgManager for each team ID provided in the filtered list of teamId.
mkLgManagersWithFilter :: C.Configuration -> [C.TeamID] -> [R.LgManager]
mkLgManagersWithFilter config validTeamIds =
  map (\teamId -> mkSingleLgManager config (C.commissioner config) (C.leagueID config) teamId) validTeamIds

-- Creates an empty roster with no players
mkEmptyRoster :: R.Roster
mkEmptyRoster = R.Roster [] [] [] [] [] [] [] [] []

-- Creates an empty lineup with no players
mkEmptyLineup :: R.CurrentLineup
mkEmptyLineup = R.CurrentLineup [] [] [] [] [] [] [] [] []

createLgManager :: C.Configuration -> C.TeamID -> R.CurrentLineup -> R.Roster -> R.LgManager
createLgManager config teamId currentLineup roster =
  R.LgManager
    { R.status = C.status config
    , R.commissioner = C.commissioner config
    , R.teamId = teamId -- Correctly used as TeamID
    , R.leagueID = C.leagueID config
    , R.current_lineup = currentLineup
    , R.roster = roster
    }

extendRankingsWithUnrankedPlayers :: [PR.PlayerRanking] -> [O.PlayerID] -> [O.PlayerID]
extendRankingsWithUnrankedPlayers rankedPlayers allPlayerIds =
  let rankedPlayerIds = map PR.playerId rankedPlayers
      unrankedPlayerIds = filter (`notElem` rankedPlayerIds) allPlayerIds
   in rankedPlayerIds ++ unrankedPlayerIds -- Concatenate ranked with unranked

-- Generate a random ByteString of a specified length
generateRandomBytes :: Int -> IO ByteString
generateRandomBytes = getRandomBytes

-- Generate a random SHA256 hash as Text
generateRandomSHA256 :: IO Text
generateRandomSHA256 = do
  randomBytes <- generateRandomBytes 32 -- Generating 32 bytes for the SHA256 input
  let hash = hashWith SHA256 randomBytes -- Hashing the random bytes with SHA256
  return $ T.decodeUtf8 $ convertToBase Base16 hash -- Convert the hash to Text (hexadecimal representation)

createRandomTeamID :: IO C.TeamID
createRandomTeamID = do C.TeamID <$> generateRandomSHA256

-- pure function to generate a random number (and a new generator)
randomIntGen :: (Int, Int) -> StdGen -> (Int, StdGen)
randomIntGen = randomR

randomInt :: (Int, Int) -> StdGen -> (Int, StdGen)
randomInt = randomR

-- Function to shuffle a list given an StdGen
shuffleList :: [a] -> StdGen -> ([a], StdGen)
shuffleList [] gen = ([], gen)
shuffleList l gen =
  let (n, newGen) = randomR (0, length l - 1) gen
      (chosen, rest) = removeAt n l
   in let (shuffledRest, finalGen) = shuffleList rest newGen
       in (chosen : shuffledRest, finalGen)

-- Helper function to remove an element at a specific index
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = let (left, x : right) = splitAt n xs in (x, left ++ right)

-- monadic error handling for fetching and decoding
withEither :: IO (Either String a) -> (a -> IO ()) -> IO ()
withEither action successHandler = do
  result <- action
  case result of
    Left err -> putStrLn err
    Right dataPacket -> successHandler dataPacket

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _ = Nothing

-- Constructs a file path for each team's final roster JSON file
constructFilePath :: FilePath -> Int -> FilePath
constructFilePath baseDir idx = baseDir ++ "finalRoster" ++ show idx ++ ".json"

-- Fetch and decode utility
fetchAndDecodeJSON :: (FromJSON a) => String -> IO (Either String a)
fetchAndDecodeJSON url = do
  response <- httpBS (parseRequest_ url)
  return $ eitherDecodeStrict $ getResponseBody response

readJson :: (FromJSON a) => FilePath -> IO (Either String a)
readJson filePath = eitherDecodeStrict <$> B.readFile filePath

loadDataFromDir :: (FromJSON a) => FilePath -> IO [Either String a]
loadDataFromDir dir = do
  jsonFiles <- listJsonFiles dir
  mapM (readJson . (dir </>)) jsonFiles

listJsonFiles :: FilePath -> IO [FilePath]
listJsonFiles dir = do
  allFiles <- listDirectory dir
  return $ filter (\f -> takeExtension f == ".json") allFiles

writeJson :: (ToJSON a) => FilePath -> a -> IO ()
writeJson filePath = BL.writeFile filePath . encode

-- Reads and parses all ranking JSON files into data structures
-- Verify if a player is in the official roster
isPlayerInOfficialRoster :: O.PlayerID -> [O.OfficialPlayer] -> Bool
isPlayerInOfficialRoster playerId =
  any (\p -> O.playerId p == playerId)

-- positionTextToRosterPosition :: T.Text -> R.Roster -> O.OfficialPlayer -> R.Roster
-- positionTextToRosterPosition position roster player =
--   -- Implementation depends on how you're managing roster updates
--   undefined

-- Filter function to retain only those rankings where the teamId matches any lgMember
filterInvalidRankings :: [C.TeamID] -> [PR.RankingData] -> [PR.RankingData]
filterInvalidRankings teamId rankings =
  filter (\ranking -> PR.teamId ranking `elem` teamId) rankings

-- Utility function to filter out teams without rankings
filterValidTeams :: [C.TeamID] -> [PR.RankingData] -> [C.TeamID]
filterValidTeams teamIds rankings =
  let validTeamIds = map PR.teamId rankings
   in filter (`elem` validTeamIds) teamIds

findPlayerRanking :: O.PlayerID -> [PR.PlayerRanking] -> Maybe Int
findPlayerRanking playerId rankings =
  PR.rank <$> find ((== playerId) . PR.playerId) rankings

readRankings :: FilePath -> IO [Either String [PR.PlayerRanking]]
readRankings dir = do
  jsonFiles <- listJsonFiles dir
  mapM (readJson . (dir </>)) jsonFiles

loadRankings :: FilePath -> IO [Either String [PR.PlayerRanking]]
loadRankings = loadDataFromDir

loadRosters :: FilePath -> IO [Either String O.OfficialRoster]
loadRosters = loadDataFromDir

computeChecksum :: BL.ByteString -> Text
computeChecksum bs = T.pack . show . hashWith SHA256 $ BL.toStrict bs

getCurrentDate :: IO Text
getCurrentDate = T.pack . formatTime defaultTimeLocale "%Y_%m_%d_%H_%M" <$> getCurrentTime

-- formatUTCTime :: Text -> UTCTime -> (Text, Value)
-- formatUTCTime key time = key .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" time

getCurrentFormattedTime :: IO String
getCurrentFormattedTime = do
  currentTime <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" currentTime
  return formattedTime

-- Draft Ordering
totalPicksPerTeam :: C.DraftRosterLmts -> Int
totalPicksPerTeam limits =
  C.dr_catcher limits
    + C.dr_first limits
    + C.dr_second limits
    + C.dr_third limits
    + C.dr_shortstop limits
    + C.dr_outfield limits
    + C.dr_utility limits
    + C.dr_s_pitcher limits
    + C.dr_r_pitcher limits

randomizeOrder :: (MonadIO m) => [C.TeamID] -> m [C.TeamID]
randomizeOrder members = liftIO $ shuffleM members

generateDraftOrder :: (MonadIO m) => C.Configuration -> [PR.RankingData] -> m C.DraftOrder
generateDraftOrder config rankings = do
  let validTeamIds = filterValidTeams (C.teamId config) rankings
  randomizedTeams <- randomizeOrder validTeamIds
  let draftParams = C.draft_parameters config
      draftLimits = C.draft_limits draftParams
      totalPicks = totalPicksPerTeam draftLimits * length randomizedTeams
      orderStrategy = PR.selectDraftOrderStrategy (C.order draftParams)
      draftOrderTeams = orderStrategy totalPicks randomizedTeams
  return $ zip draftOrderTeams [1 ..]-- End of /home/bismuth/git/pelotero-engine/src/League/Utility.hs

-- Start of /home/bismuth/git/pelotero-engine/src/League/PointCalc.hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module PointCalc where

import Control.Monad (filterM)
import Data.Aeson (
  FromJSON (..),
  Result (Success),
  ToJSON (..),
  Value (..),
  decode,
  eitherDecodeStrict,
  encode,
  fromJSON,
  object,
  withObject,
  (.!=),
  (.:),
  (.:?),
  (.=),
 )

import qualified Data.Aeson.Key as K
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (writeFile)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as MS
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Arr (array)
import Text.Read (readMaybe)

import qualified Config as C
import qualified Input as I
import qualified Middle as M
import qualified Points as P
import qualified Roster as R
import qualified OfficialRoster as O
import Validators

-- mergeGmPoints :: P.GmPoints -> P.GmPoints -> P.GmPoints
-- mergeGmPoints (P.GmPoints b1 p1) (P.GmPoints b2 p2) = P.GmPoints (b1 ++ b2) (p1 ++ p2)
mergeGmPoints :: P.GmPoints -> P.GmPoints -> Either Text P.GmPoints
mergeGmPoints g1@(P.GmPoints id1 _ _) g2@(P.GmPoints id2 _ _) =
  if id1 /= id2 then
    Left "Attempting to merge points for different players!"
  else
    Right $ P.GmPoints id1 (b1 ++ b2) (p1 ++ p2)
  where
    P.GmPoints _ b1 p1 = g1
    P.GmPoints _ b2 p2 = g2

calculatePointsForPlayer :: C.Configuration -> O.PlayerID -> R.LgManager -> M.JsonPlayerData -> Either Text P.GmPoints
calculatePointsForPlayer config playerId lgManager stats = do
  playerType <- batterOrPitcher playerId lgManager
  let maybeStatsData = MS.lookup (O.playerIDToText playerId) (M.stats stats)
  let battingMults = C.lg_battingMults $ C.point_parameters config
  let pitchingMults = C.lg_pitchingMults $ C.point_parameters config
      
  case playerType of
    P.Batting -> case maybeStatsData >>= M.batting of
      Just battingStats -> Right $ calcBattingPoints playerId battingMults battingStats
      Nothing -> Left "Batting stats not found"
    P.Pitching -> case maybeStatsData >>= M.pitching of
      Just pitchingStats -> Right $ calcPitchingPoints playerId pitchingMults pitchingStats
      Nothing -> Left "Pitching stats not found"

calcBattingPoints :: O.PlayerID -> C.BattingMults -> I.BattingStats -> P.GmPoints
calcBattingPoints playerId mults stats =
  let s = fromIntegral (fromMaybe 0 (I.bat_hits stats) - sum [fromMaybe 0 (I.bat_triples stats), fromMaybe 0 (I.bat_doubles stats), fromMaybe 0 (I.bat_homeRuns stats)]) * C.lgb_single mults
      d = fromIntegral (fromMaybe 0 (I.bat_doubles stats)) * C.lgb_double mults
      t = fromIntegral (fromMaybe 0 (I.bat_triples stats)) * C.lgb_triple mults
      h = fromIntegral (fromMaybe 0 (I.bat_homeRuns stats)) * C.lgb_homerun mults
      rbi = fromIntegral (fromMaybe 0 (I.bat_rbi stats)) * C.lgb_rbi mults
      r = fromIntegral (fromMaybe 0 (I.bat_runs stats)) * C.lgb_run mults
      bob = fromIntegral (fromMaybe 0 (I.bat_baseOnBalls stats)) * C.lgb_base_on_balls mults
      sb = fromIntegral (fromMaybe 0 (I.bat_stolenBases stats)) * C.lgb_stolen_base mults
      hbp = fromIntegral (fromMaybe 0 (I.bat_hitByPitch stats)) * C.lgb_hit_by_pitch mults
      ko = fromIntegral (fromMaybe 0 (I.bat_strikeOuts stats)) * C.lgb_strikeout mults
      cs = fromIntegral (fromMaybe 0 (I.bat_caughtStealing stats)) * C.lgb_caught_stealing mults
  in P.GmPoints
        playerId
        [Just P.BattingGmPoints { gmb_gameId = "GameID"
          , gmb_total_points = s + d + t + h + rbi + r + bob + sb + hbp - ko - cs
          , gmb_single = s
          , gmb_double = d
          , gmb_triple = t
          , gmb_homerun = h
          , gmb_rbi = rbi
          , gmb_run = r
          , gmb_base_on_balls = bob
          , gmb_stolen_base = sb
          , gmb_hit_by_pitch = hbp
          , gmb_strikeout = ko
          , gmb_caught_stealing = cs 
          } 
        ]
        []

calcPitchingPoints :: O.PlayerID -> C.PitchingMults -> I.PitchingStats -> P.GmPoints
calcPitchingPoints playerId mults stats =
  let w = fromIntegral (fromMaybe 0 (I.pit_wins stats)) * C.lgp_win mults
      s = fromIntegral (fromMaybe 0 (I.pit_saves stats)) * C.lgp_save mults
      inningsPitched = fromMaybe "0" (I.pit_inningsPitched stats)
      parsedInnings = readMaybe (T.unpack inningsPitched) :: Maybe Double
      actualInnings = fromMaybe 0.0 parsedInnings
      qs = if actualInnings >= 6 && fromMaybe 0 (I.pit_earnedRuns stats) <= 3 then C.lgp_quality_start mults else 0
      ip = actualInnings * C.lgp_inning_pitched mults
      ko = fromIntegral (fromMaybe 0 (I.pit_strikeOuts stats)) * C.lgp_strikeout mults
      cg = fromIntegral (fromMaybe 0 (I.pit_completeGames stats)) * C.lgp_complete_game mults
      sho = fromIntegral (fromMaybe 0 (I.pit_shutouts stats)) * C.lgp_shutout mults
      bob = fromIntegral (fromMaybe 0 (I.pit_baseOnBalls stats)) * C.lgp_base_on_balls mults
      ha = fromIntegral (fromMaybe 0 (I.pit_hits stats)) * C.lgp_hits_allowed mults
      er = fromIntegral (fromMaybe 0 (I.pit_earnedRuns stats)) * C.lgp_earned_runs mults
      hbm = fromIntegral (fromMaybe 0 (I.pit_hitBatsmen stats)) * C.lgp_hit_batsman mults
      l = fromIntegral (fromMaybe 0 (I.pit_losses stats)) * C.lgp_loss mults
  in P.GmPoints
        playerId
        []
        [Just P.PitchingGmPoints { gmp_gameId = "GameID"  -- Placeholder, replace with actual game ID
            , gmp_total_points = w + s + qs + ip + ko + cg + sho - bob - ha - er - hbm - l
            , gmp_win = w
            , gmp_save = s
            , gmp_quality_start = qs
            , gmp_inning_pitched = ip
            , gmp_strikeout = ko
            , gmp_complete_game = cg
            , gmp_shutout = sho
            , gmp_base_on_balls = bob
            , gmp_hits_allowed = ha
            , gmp_earned_runs = er
            , gmp_hit_batsman = hbm
            , gmp_loss = l
            }
        ]

-- this one is only broken because it uses our old style of querying the stats and getting batter or pitcher
-- calculateAllPoints :: C.Configuration -> R.LgManager -> [M.JsonPlayerData] -> [(Text, Either Text [P.GmPoints])]
-- calculateAllPoints config lgManager = map (\playerData ->
--     let playerId = M.playerId playerData
--     in (playerId, calculatePointsForGivenPlayer config playerId lgManager playerData))

-- calculatePointsForGivenPlayer :: C.Configuration -> O.PlayerID -> R.LgManager -> M.JsonPlayerData -> Either Text [P.GmPoints]
-- calculatePointsForGivenPlayer config playerId lgManager playerData = do
--     playerType <- batterOrPitcher playerId lgManager
--     let playerAllStats = MS.elems $ M.stats playerData
--     return $ case playerType of
--         P.Batting -> map (\stats -> fromMaybe (P.GmPoints [] []) (M.batting stats) >>= calcBattingPoints (C.lg_battingMults $ C.point_parameters config)) playerAllStats
--         P.Pitching -> map (\stats -> fromMaybe (P.GmPoints [] []) (M.pitching stats) >>= calcPitchingPoints (C.lg_pitchingMults $ C.point_parameters config)) playerAllStats

-- queryPlayerId :: O.PlayerID -> P.StatType -> M.JsonPlayerData -> P.PlayerResults
-- queryPlayerId playerIdQuery statType playerData
--     | playerIdQuery /= playerID = P.NoStats
--     | statType == P.Batting   = P.BattingResults (listToMaybe $ map M.batting allStats)
--     | statType == P.Pitching  = P.PitchingResults (listToMaybe $ map M.pitching allStats)
--     where
--         playerID = M.playerId playerData
--         allStats = MS.elems $ M.stats playerData

-- End of /home/bismuth/git/pelotero-engine/src/League/PointCalc.hs

-- Start of /home/bismuth/git/pelotero-engine/src/League/Scraper.hs
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use catMaybes" #-}

module Scraper (
    scrapeStatsForDateRange,
    writeRosterToFile,
    fetchActiveRoster,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (filterM, when)
import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (
    FromJSON (parseJSON),
    Options (fieldLabelModifier),
    defaultOptions,
    eitherDecode,
    eitherDecodeStrict,
    encode,
    genericParseJSON,
    withObject,
    (.:),
    (.:?),
 )
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (
    Day,
    addDays,
    defaultTimeLocale,
    diffDays,
    formatTime,
    parseTimeOrError,
 )
import Data.Time.Clock
import Data.Time.Clock.POSIX ()
import Data.Time.Format
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Simple (
    getResponseBody,
    httpBS,
    parseRequest_,
 )
import System.Directory (createDirectoryIfMissing, doesFileExist)

import qualified Input as I
import qualified Middle as MI
import qualified Points as P
import Validators
import Utility
    ( computeChecksum, fetchAndDecodeJSON, getCurrentDate, withEither )

-- A (date String) -> [B] (list of gameIds/GameSchedule)
-- takes a date string "YYYY-MM-DD" and outputs a schedule bytestring of that day schdule
fetchGameScheduleForDate :: String -> IO (Either String I.GameSchedule)
fetchGameScheduleForDate date = do
    scheduleResult <- fetchAndDecodeJSON (scheduleUrl date)
    return $ fmap (assignDateToSchedule (T.pack date)) scheduleResult

-- B (gameId) -> C (status)
-- takes a gameId and returns IO (Either String LiveGameWrapper)
fetchGameStatus :: Int -> IO (Either String I.LiveGameWrapper)
fetchGameStatus gameId = fetchAndDecodeJSON (gameStatusUrl gameId)

-- B (gameId) -> C (status) -> D (boxscore)
-- takes a gameId and returns IO (Either String GameData)
fetchFinishedBxScore :: Int -> IO (Either String (Maybe I.GameData))
fetchFinishedBxScore gameId = do
    gameStatusResult <- fetchGameStatus gameId
    case gameStatusResult of
        Right gameDataWrapper -> do
            let liveStatusWrapper = I.gameData gameDataWrapper
            let liveStatus = I.gameStatus liveStatusWrapper
            if I.codedGameState liveStatus == "F"
                then do
                    boxscoreResult <- fetchAndDecodeJSON (boxScoreUrl gameId)
                    return $ fmap (Just . assignGameIdToPlayers gameId) boxscoreResult
                else -- \*adds gameId attribute to corresponding stats
                    return $ Right Nothing
        Left err -> return $ Left ("Error fetching game status: " ++ err)

-- -- [B] list of gameIds -> C status checks -> [D] list of boxscores
-- fetchFinishedBxScores :: [Int] -> IO (Either String (M.Map Int I.GameData))
fetchFinishedBxScores :: [Int] -> IO (Either String (M.Map Int (Maybe I.GameData)))
fetchFinishedBxScores gameIds = do
    results <- mapConcurrently fetchGame gameIds
    let combinedResults = sequenceA results -- Change the structure from [Either] to Either [..]
    return $ fmap (M.fromList . filter finishedGames) combinedResults
  where
    fetchGame gameId = do
        result <- fetchFinishedBxScore gameId
        return $ fmap (\d -> (gameId, d)) result
    finishedGames (_, Nothing) = False
    finishedGames (_, Just _) = True

-- ## OUTPUT CONVERSION ##
-- [B] list of gameIds -> C status checks -> [D] (list of box scores) -> [E] (list of player data)
fetchFinishedBxScoresToJsonPlayerData :: [Int] -> IO (Either String (M.Map Text MI.JsonPlayerData))
fetchFinishedBxScoresToJsonPlayerData gameIds = do
    gameDataResult <- fetchFinishedBxScores gameIds
    return $ fmap convertGameDataMapToJsonPlayerData gameDataResult

-- Main scraper function tying stats scaping everything together
scrapeStatsForDateRange :: String -> String -> IO ()
scrapeStatsForDateRange start end = do
    mapM_ processDate (generateDateRange start end)

processDate :: String -> IO ()
processDate date = do
    putStrLn $ "Processing " ++ date
    scheduleResult <- fetchGameScheduleForDate date
    processAndPrintGames scheduleResult
    case scheduleResult of
        Left err -> putStrLn $ "Failed to fetch game schedule: " ++ err
        Right schedule -> do
            let gameIds = extractGameIds schedule
            flattenedPlayersResult <- fetchFinishedBxScoresToJsonPlayerData gameIds
            case flattenedPlayersResult of
                Left err -> putStrLn $ "Failed to process JSON: " ++ err
                Right _flattenedPlayers -> do
                    -- The printout of flattenedPlayers has been removed
                    let filename = formatFilename date
                    writeDataToFile filename "appData/stats" _flattenedPlayers

flattenedPlayersList :: M.Map Text MI.JsonPlayerData -> M.Map Text MI.JsonPlayerData
flattenedPlayersList = id -- or simply remove this function and use the map directly

-- takes a list of tuples game id's and game data and prints them
printGameData :: Either String (M.Map Int I.GameData) -> IO ()
printGameData gameDataMapEither =
    withEither (return gameDataMapEither) $ \gameDataMap ->
        mapM_ (\(gameId, gameData) -> putStrLn $ show gameId ++ ": " ++ show gameData) (M.toList gameDataMap)

processAndPrintGames :: Either String I.GameSchedule -> IO ()
processAndPrintGames gameScheduleEither =
    withEither (return gameScheduleEither) $ \gameSchedule ->
        if hasGamesForDate gameSchedule
            then do
                let gameIds = extractGameIds gameSchedule
                _ <- fetchFinishedBxScores gameIds
                return ()
            else putStrLn "No games scheduled for the provided date."

-- takes a season and outputs a roster bytestring of that season
-- fetchActiveRoster :: Int -> IO (Either String I.ActivePlayer)
fetchActiveRoster :: Int -> IO (Either String I.ActiveRoster)
fetchActiveRoster season = fetchAndDecodeJSON (rosterUrl season)

writeRosterToFile :: FilePath -> I.ActiveRoster -> IO ()
writeRosterToFile path roster = do
    -- Original player data encoding
    let playerData = encode (I.people roster)

    -- Compute checksum and get date stamp
    dateStamp <- getCurrentDate
    let checksumValue = computeChecksum playerData
    let fullRoster = I.ActiveRoster (I.people roster) (Just dateStamp) (Just checksumValue)

    -- Encode the full roster including the checksum and date stamp
    let jsonData = encode fullRoster

    -- Write to file
    BL.writeFile path jsonData

-- Special Enhancement of fromJSON types that gets called as post-processing in the fetch functions
assignDateToSchedule :: Text -> I.GameSchedule -> I.GameSchedule
assignDateToSchedule date schedule =
    let assignToDateEntry entry = entry{I.games = fmap (V.map assignToDate) (I.games entry)}
        assignToDate gameID = gameID{I.game_date = Just date}
     in schedule{I.dates = map assignToDateEntry (I.dates schedule)}

assignGameIdToPlayers :: Int -> I.GameData -> I.GameData
assignGameIdToPlayers gameId gameData =
    let assignToTeam team = team{I.players = M.map assignToPlayer (I.players team)}
        assignToPlayer player = player{I.gameid = Just gameId}
     in gameData
            { I.teams =
                (I.teams gameData)
                    { I.away = assignToTeam (I.away (I.teams gameData))
                    , I.home = assignToTeam (I.home (I.teams gameData))
                    }
            }



-- Takes a schedule bytestring and outputs true if games are happening, false otherwise.
hasGamesForDate :: I.GameSchedule -> Bool
hasGamesForDate schedule = any (isJust . I.games) (I.dates schedule)

-- Takes a schedule bytestring and outputs an array of gameId's or errors
extractGameIds :: I.GameSchedule -> [Int]
extractGameIds gameData = concatMap (maybe [] (V.toList . fmap I.gamePk) . I.games) (I.dates gameData)

-- ## FileName Manipulation Stuff
-- Takes a filename, path, and the data to save, then writes to a JSON file at the specified path with the given filename.
writeDataToFile :: FilePath -> FilePath -> M.Map Text MI.JsonPlayerData -> IO ()
writeDataToFile filename path dataToSave = do
    createOutputDirectory path
    let fullpath = path ++ "/" ++ filename
    BL.writeFile fullpath (encode dataToSave)

-- Write Player to JSON File
writePlayerToJsonFile :: FilePath -> I.Player -> IO ()
writePlayerToJsonFile path player = B.writeFile path (convertPlayerToJson player)

-- Stat-Mutation stuff
playerToJsonPlayerData :: I.Player -> MI.JsonPlayerData
playerToJsonPlayerData p =
    MI.JsonPlayerData
        { MI.playerId = T.pack $ show $ I.personId (I.person p)
        , MI.fullName = I.fullName (I.person p)
        , MI.stats = M.singleton (maybe "" (T.pack . show) (I.gameid p)) (playerToJsonStatsData p)
        }

playerToJsonStatsData :: I.Player -> MI.JsonStatsData
playerToJsonStatsData p =
    MI.JsonStatsData
        { MI.parentTeamId = I.parentTeamId p
        , MI.allPositions = fromMaybe [] (I.allPositions p)
        , MI.statusCode = I.status_code (I.status p)
        , MI.batting = I.batting (I.stats p)
        , MI.pitching = I.pitching (I.stats p)
        }

convertPlayerToJson :: I.Player -> ByteString
convertPlayerToJson = BL.toStrict . encode . playerToJsonPlayerData

convertGameDataMapToJsonPlayerData :: M.Map Int (Maybe I.GameData) -> M.Map Text MI.JsonPlayerData
convertGameDataMapToJsonPlayerData maybeGameDataMap =
    foldl mergePlayerData M.empty allPlayerDataPairs
  where
    allPlayerDataPairs :: [(Text, MI.JsonPlayerData)]
    allPlayerDataPairs = concatMap gameDataToPlayerDataPairs (mapMaybe id (M.elems maybeGameDataMap))

    gameDataToPlayerDataPairs :: I.GameData -> [(Text, MI.JsonPlayerData)]
    gameDataToPlayerDataPairs gameData =
        let awayPlayers = M.elems $ I.players $ I.away $ I.teams gameData
            homePlayers = M.elems $ I.players $ I.home $ I.teams gameData
            allPlayers = awayPlayers ++ homePlayers
         in map (\player -> (rawStringToText $ MI.playerId (playerToJsonPlayerData player), playerToJsonPlayerData player)) allPlayers

    rawStringToText :: Text -> Text
    rawStringToText = T.replace "\\\"" "\"" . T.replace "\\\\" "\\"

    mergePlayerData :: M.Map Text MI.JsonPlayerData -> (Text, MI.JsonPlayerData) -> M.Map Text MI.JsonPlayerData
    mergePlayerData acc (playerId, newPlayerData) =
        let mergedData = case M.lookup playerId acc of
                Just existingPlayerData -> mergeJsonPlayerData existingPlayerData newPlayerData
                Nothing -> newPlayerData
         in M.insert playerId mergedData acc

mergeJsonPlayerData :: MI.JsonPlayerData -> MI.JsonPlayerData -> MI.JsonPlayerData
mergeJsonPlayerData existing new =
    MI.JsonPlayerData
        { MI.playerId = MI.playerId existing -- assuming playerIds are the same, else there's a bigger problem!
        , MI.fullName = MI.fullName existing -- assuming fullNames are the same
        , MI.stats = M.unionWith mergeJsonStatsData (MI.stats existing) (MI.stats new)
        }

mergeJsonStatsData :: MI.JsonStatsData -> MI.JsonStatsData -> MI.JsonStatsData
mergeJsonStatsData _ new = new

-- Takes a date string and formats it as a filename, like "2023_08_22.json".
formatFilename :: String -> String
formatFilename date = replace '-' '_' date ++ ".json"
  where
    replace old new = T.unpack . T.replace (T.pack [old]) (T.pack [new]) . T.pack

-- Create output directory
createOutputDirectory :: FilePath -> IO ()
createOutputDirectory = createDirectoryIfMissing True

-- ## Dates ##
-- Converts a String of format "YYYY-MM-DD" to a Day
stringToDay :: String -> Day
stringToDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

-- Increments the Day by one
incrementDay :: Day -> Day
incrementDay = addDays 1

-- Convert StatType to a string representation for reasons????
statTypeToString :: P.StatType -> String
statTypeToString P.Batting = "batting"
statTypeToString P.Pitching = "pitching"

-- Generates a list of dates from the start to the end
generateDateRange :: String -> String -> [String]
generateDateRange start end = map (formatTime defaultTimeLocale "%Y-%m-%d") dates
  where
    startDate = stringToDay start
    endDate = stringToDay end
    dates = takeWhile (<= endDate) $ iterate incrementDay startDate

-- Generate the API URL for a single day's schedule
scheduleUrl :: String -> String
scheduleUrl date = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date

-- Generate the API URL for live game status
gameStatusUrl :: Int -> String
gameStatusUrl gameId = "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live"

-- Generate the API URL for finished boxscore
boxScoreUrl :: Int -> String
boxScoreUrl gameId = "http://statsapi.mlb.com/api/v1/game/" ++ show gameId ++ "/boxscore"

-- Generate the API URL for specific years rosters
rosterUrl :: Int -> String
rosterUrl season = "https://statsapi.mlb.com/api/v1/sports/1/players?activeStatus=ACTIVE&season=" ++ show season

-- Generate the API URL for specific year's stat leaders in either batting or pitching (not working well)
seasonStatsUrl :: Int -> P.StatType -> String
seasonStatsUrl season statType = "http://statsapi.mlb.com/api/v1/stats?stats=season&sportId=1&season=" ++ show season ++ "&group=" ++ statTypeToString statType
-- End of /home/bismuth/git/pelotero-engine/src/League/Scraper.hs

-- Start of /home/bismuth/git/pelotero-engine/src/League/Generators.hs
module Main (main) where

import Control.Monad (filterM, forM, forM_)

import qualified Config as C
import Data.Aeson (FromJSON, ToJSON, encode, parseJSON, withObject, (.:))
import qualified Data.HashMap.Strict as HM
import Data.List (delete)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import qualified Input as I
import OfficialRoster as O
import qualified OfficialRoster as O
import qualified PlayerRanking as PR
import qualified Points as P
import qualified Roster as R
import System.Random (StdGen, newStdGen, randomR)
import Utility (
  computeChecksum,
  createRandomTeamID,
  generateRandomSHA256,
  readJson,
  shuffleList,
  writeJson,
 )
import Validators

-- Updated createRandomRankings function
createRandomRankings :: O.OfficialRoster -> IO [PR.PlayerRanking]
createRandomRankings officialRoster = do
  gen <- newStdGen
  let players = O.people officialRoster
  let (shuffledPlayers, _) = shuffleList players gen
  let rankings = zipWith (\rank player -> PR.PlayerRanking (O.playerId player) rank) [1 ..] shuffledPlayers
  return rankings

main :: IO ()
main = do
  eitherRoster <- readJson "testFiles/appData/rosters/activePlayers.json"
  case eitherRoster of
    Right roster -> do
      rankings <- createRandomRankings roster
      putStrLn "Randomly generated player rankings:"
      forM_ rankings $ \(PR.PlayerRanking playerId rank) ->
        putStrLn $ "Player ID: " ++ show playerId ++ ", Rank: " ++ show rank

      currentTime <- getCurrentTime
      randomTeamId <- createRandomTeamID -- Use the new function here
      let shortTeamId = T.take 12 $ C.unwrapTeamID randomTeamId -- Unwrap here for filename
      let rankingData = PR.RankingData randomTeamId (T.pack "") currentTime rankings
      let rankingDataJson = encode rankingData
      let dataChecksum = computeChecksum rankingDataJson

      let rankingDataWithChecksum = rankingData{PR.dataChecksum = dataChecksum, PR.teamId = randomTeamId}

      let fileName = "testFiles/appData/rankings/_" ++ T.unpack shortTeamId ++ "_.json"
      writeJson fileName rankingDataWithChecksum

      putStrLn $ "Rankings written to JSON file successfully at " ++ fileName
    Left error ->
      putStrLn $ "Failed to load the roster: " ++ error-- End of /home/bismuth/git/pelotero-engine/src/League/Generators.hs

-- Start of /home/bismuth/git/pelotero-engine/src/League/Validators.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use :" #-}

module Validators where

import Control.Monad (filterM, forM, unless)
import Data.Aeson (FromJSON (..), Result (Success), ToJSON (..), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import Data.List ( find, delete, nub, (\\) )
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (foldl', forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (traceShow, traceShowM)
import Data.List (sortOn)


import qualified Config as C
import qualified GHC.Generics as R
import qualified Input as I
import qualified OfficialRoster as O
import qualified Points as P
import qualified Roster as R
import qualified PlayerRanking as PR
import Utility

type FileName = String
type FileContent = Either String R.LgManager

-- data StatType = Batting | Pitching deriving (Show, Eq)

extractNameFromPath :: FileName -> String
extractNameFromPath = reverse . takeWhile (/= '/') . reverse

processConfigResults :: C.Configuration -> [(FileName, FileContent)] -> IO ()
processConfigResults config files =
    forM_ files $ \(fname, content) -> do
        putStrLn $ "\nTesting with " ++ extractNameFromPath fname ++ ":"
        testLineup config content

testLineup :: C.Configuration -> Either String R.LgManager -> IO ()
testLineup _ (Left errRoster) = putStrLn $ "Failed to parse Roster JSON: " ++ errRoster
testLineup config (Right lgManager) = do
    print $ R.current_lineup lgManager
    print lgManager
    isValid <- validateAndPrintLineup lgManager config
    if isValid
        then putStrLn "This Lineup is valid."
        else putStrLn "That lineup has discrepancies."

findPlayer :: O.PlayerID -> [O.OfficialPlayer] -> [O.PlayerID] -> Maybe O.OfficialPlayer
findPlayer playerId players availableIds =
    find (\p -> O.playerId p == playerId && playerId `elem` availableIds) players

maxPossibleTeams :: C.Configuration -> O.OfficialRoster -> Int
maxPossibleTeams config roster =
  let
    positions = ["P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH"]
    -- Calculate the max possible teams for each position and take the minimum
    maxTeamsForAllPositions = map (\pos -> maxPossibleTeamsForPosition pos config roster) positions
  in
    minimum maxTeamsForAllPositions

maxPossibleTeamsForPosition :: T.Text -> C.Configuration -> O.OfficialRoster -> Int
maxPossibleTeamsForPosition position config roster =
  let
    -- Convert the position code to the draft limit field name
    draftLimit = case positionCodeToText position of
      "pitcher" -> C.dr_s_pitcher (C.draft_limits (C.draft_parameters config)) + C.dr_r_pitcher (C.draft_limits (C.draft_parameters config))
      "catcher" -> C.dr_catcher (C.draft_limits (C.draft_parameters config))
      "first" -> C.dr_first (C.draft_limits (C.draft_parameters config))
      "second" -> C.dr_second (C.draft_limits (C.draft_parameters config))
      "third" -> C.dr_third (C.draft_limits (C.draft_parameters config))
      "shortstop" -> C.dr_shortstop (C.draft_limits (C.draft_parameters config))
      "outfield" -> C.dr_outfield (C.draft_limits (C.draft_parameters config)) * 3 -- Assuming LF, CF, RF are interchangeable
      "utility" -> C.dr_utility (C.draft_limits (C.draft_parameters config))
      _ -> 0

    -- Count the number of players available for the position
    playerCount = length $ filter (\p -> O.primaryPosition p == position) (O.people roster)
  in
    -- Calculate the maximum number of teams based on the draft limit and available players
    if draftLimit > 0 then playerCount `div` draftLimit else 0

-- This function validates a lineup and returns True or False.
isLineupValid :: R.LgManager -> C.Configuration -> Bool
isLineupValid manager config =
    case validateLineup manager config of
        Left _ -> False
        Right _ -> True

-- This function validates a roster and returns the error messages if there are any.
getRosterValidationErrors :: R.LgManager -> C.Configuration -> [String]
getRosterValidationErrors manager config =
    case validateLineup manager config of
        Left errors -> errors
        Right _ -> []

-- This function validates a roster and returns the error messages if there are any.
getLineupValidationErrors :: R.LgManager -> C.Configuration -> [String]
getLineupValidationErrors manager config =
    case validateLineup manager config of
        Left errors -> errors
        Right _ -> []

hasUniqueLineupPlayers :: R.CurrentLineup -> Either String [O.PlayerID]
hasUniqueLineupPlayers lineup =
    let allPlayers = getUniquePlayerIdsLineup lineup
        duplicates = allPlayers \\ nub allPlayers
     in if null duplicates
            then Left "No duplicate players found."
            else Right duplicates

-- Roster Validation
hasUniqueRosterPlayers :: R.Roster -> Either String [O.PlayerID]
hasUniqueRosterPlayers roster =
    let allPlayers = concat [R.cR roster, R.b1R roster, R.b2R roster, R.b3R roster, R.ssR roster, R.ofR roster, R.uR roster, R.spR roster, R.rpR roster]
        duplicates = allPlayers \\ nub allPlayers
    in if null duplicates then Left "No duplicate players found in roster." else Right duplicates

lookupPlayerInRoster :: O.PlayerID -> R.Roster -> Bool
lookupPlayerInRoster playerId roster =
    let allPlayers = concat [R.cR roster, R.b1R roster, R.b2R roster, R.b3R roster, R.ssR roster, R.ofR roster, R.uR roster, R.spR roster, R.rpR roster]
    in playerId `elem` allPlayers

lookupPlayerInLineup :: O.PlayerID -> R.CurrentLineup -> Bool
lookupPlayerInLineup playerId lineup =
    let allPlayers = concat [R.cC lineup, R.b1C lineup, R.b2C lineup, R.b3C lineup, R.ssC lineup, R.ofC lineup, R.uC lineup, R.spC lineup, R.rpC lineup]
    in playerId `elem` allPlayers

getRosterDiscrepancies :: R.Roster -> C.DraftRosterLmts -> [(String, Int)]
getRosterDiscrepancies roster limits =
    mapMaybe validatePosition [
      ("catcher", R.cR roster, C.dr_catcher limits),
      ("first", R.b1R roster, C.dr_first limits),
      ("second", R.b2R roster, C.dr_second limits),
      ("third", R.b3R roster, C.dr_third limits),
      ("shortstop", R.ssR roster, C.dr_shortstop limits),
      ("outfield", R.ofR roster, C.dr_outfield limits),
      ("utility", R.uR roster, C.dr_utility limits),
      ("s_pitcher", R.spR roster, C.dr_s_pitcher limits),
      ("r_pitcher", R.rpR roster, C.dr_r_pitcher limits)
    ]
  where
    validatePosition (posName, players, limit) =
        let diff = length players - limit
        in if diff > 0 then Just (posName, diff) else Nothing

getLineupDiscrepancies :: R.CurrentLineup -> C.LgLineupLmts -> [(String, Int)]
getLineupDiscrepancies lineup limits =
    mapMaybe validatePosition [
      ("catcher", R.cC lineup, C.lg_catcher limits),
      ("first", R.b1C lineup, C.lg_first limits),
      ("second", R.b2C lineup, C.lg_second limits),
      ("third", R.b3C lineup, C.lg_third limits),
      ("shortstop", R.ssC lineup, C.lg_shortstop limits),
      ("outfield", R.ofC lineup, C.lg_outfield limits),
      ("utility", R.uC lineup, C.lg_utility limits),
      ("s_pitcher", R.spC lineup, C.lg_s_pitcher limits),
      ("r_pitcher", R.rpC lineup, C.lg_r_pitcher limits)
    ]
  where
    validatePosition (posName, players, limit) =
        let diff = length players - limit
        in if diff > 0 then Just (posName, diff) else Nothing

countPlayersOnRoster :: T.Text -> R.Roster -> Int
countPlayersOnRoster position roster =
    case position of
        "catcher" -> length $ R.cR roster
        "first" -> length $ R.b1R roster
        "second" -> length $ R.b2R roster
        "third" -> length $ R.b3R roster
        "shortstop" -> length $ R.ssR roster
        "outfield" -> length $ R.ofR roster
        "utility" -> length $ R.uR roster
        "s_pitcher" -> length $ R.spR roster
        "r_pitcher" -> length $ R.rpR roster
        _ -> 0

countPlayersInLineup :: T.Text -> R.CurrentLineup -> Int
countPlayersInLineup position lineup =
    case position of
        "catcher" -> length $ R.cC lineup
        "first" -> length $ R.b1C lineup
        "second" -> length $ R.b2C lineup
        "third" -> length $ R.b3C lineup
        "shortstop" -> length $ R.ssC lineup
        "outfield" -> length $ R.ofC lineup
        "utility" -> length $ R.uC lineup
        "s_pitcher" -> length $ R.spC lineup
        "r_pitcher" -> length $ R.rpC lineup
        _ -> 0

-- validateRoster :: R.Roster -> C.Configuration -> Either [String] ()
-- validateRoster roster config = do
--     let discrepancies = getRosterDiscrepancies roster (C.draft_limits $ C.draft_parameters config)
--     let duplicateCheck = hasUniqueRosterPlayers roster
--     case (duplicateCheck, discrepancies) of
--         (Left _, []) -> Right ()
--         (Right duplicates, []) -> Left (map T.unpack duplicates ++ ["Duplicate player IDs found in roster."])
--         (_, errors) -> Left $ map (\(pos, diff) -> pos ++ ": Too many players in Roster - " ++ show diff) errors

playerIDToString :: O.PlayerID -> String
playerIDToString (O.PlayerID pid) = show pid

validateRoster :: R.Roster -> C.Configuration -> Either [String] ()
validateRoster roster config = do
    let discrepancies = getRosterDiscrepancies roster (C.draft_limits $ C.draft_parameters config)
    let duplicateCheck = hasUniqueRosterPlayers roster
    case (duplicateCheck, discrepancies) of
        (Left _, []) -> Right ()
        (Right duplicates, []) -> Left (map playerIDToString duplicates ++ ["Duplicate player IDs found in roster."])
        (_, errors) -> Left $ map (\(pos, diff) -> pos ++ ": Too many players in Roster - " ++ show diff) errors

validateLineup :: R.LgManager -> C.Configuration -> Either [String] ()
validateLineup manager config = do
    let discrepancies = getLineupDiscrepancies (R.current_lineup manager) (C.lineup_limits . C.point_parameters $ config)
    let duplicateCheck = hasUniqueLineupPlayers (R.current_lineup manager)
    case (duplicateCheck, discrepancies) of
        (Left _, []) -> Right ()
        (Right duplicates, []) -> Left (map playerIDToString duplicates ++ ["Duplicate player IDs found in lineup."])
        (_, errors) -> Left $ map (\(pos, diff) -> pos ++ ": Too many players in Lineup - " ++ show diff) errors

validateAndPrintLineup :: R.LgManager -> C.Configuration -> IO Bool
validateAndPrintLineup manager config = do
    let rosterConfig = C.lineup_limits . C.point_parameters $ config
    let discrepancies = getLineupDiscrepancies (R.current_lineup manager) rosterConfig
    let validPositions = null discrepancies
    let validRosterSize = all (\(_, diff) -> diff <= 0) discrepancies

    playerIdValidation <- validatePlayerId (R.current_lineup manager)

    -- Print discrepancies if any
    unless (null discrepancies) $ do
        putStrLn "Discrepancies found in roster positions:"
        mapM_ (\(pos, diff) -> putStrLn $ "This roster has " ++ show diff ++ " too many players at " ++ pos) discrepancies

    case playerIdValidation of
        Left errMsgs -> do
            putStrLn "Errors found:"
            mapM_ (putStrLn . T.unpack) errMsgs  -- Process each errMsg individually
            return False
        Right validMsg -> do
            -- Success message, all player IDs are valid
            putStrLn validMsg
            case hasUniqueLineupPlayers (R.current_lineup manager) of
                Left _ -> do
                    -- Left case should indicate success in this context, contrary to the initial advice.
                    putStrLn "No duplicate players found."
                    return $ validPositions && validRosterSize
                Right duplicates -> do
                    putStrLn "Duplicate player IDs found:"
                    mapM_ (putStrLn . T.unpack . O.playerIDToText) duplicates
                    return False

queryLimits :: T.Text -> T.Text -> C.Configuration -> Int
queryLimits limtype position config =
    case limtype of
        "draft" -> queryDraftRosterLmts position (C.draft_limits $ C.draft_parameters config)
        "lineup" -> queryLgLineupLmts position (C.lineup_limits $ C.point_parameters config)
        _ -> 0

queryDraftRosterLmts :: T.Text -> C.DraftRosterLmts -> Int
queryDraftRosterLmts position limits =
    case position of
        "catcher" -> C.dr_catcher limits
        "first" -> C.dr_first limits
        "second" -> C.dr_second limits
        "third" -> C.dr_third limits
        "shortstop" -> C.dr_shortstop limits
        "outfield" -> C.dr_outfield limits
        "utility" -> C.dr_utility limits
        "s_pitcher" -> C.dr_s_pitcher limits
        "r_pitcher" -> C.dr_r_pitcher limits
        _ -> 0

queryLgLineupLmts :: T.Text -> C.LgLineupLmts -> Int
queryLgLineupLmts position limits =
    case position of
        "catcher" -> C.lg_catcher limits
        "first" -> C.lg_first limits
        "second" -> C.lg_second limits
        "third" -> C.lg_third limits
        "shortstop" -> C.lg_shortstop limits
        "outfield" -> C.lg_outfield limits
        "utility" -> C.lg_utility limits
        "s_pitcher" -> C.lg_s_pitcher limits
        "r_pitcher" -> C.lg_r_pitcher limits
        _ -> 0     

-- also used in Leaderboard to verify that the player is valid except with that different player type
hasValidPositions :: Value -> Bool
hasValidPositions val = case fromJSON val :: Result I.Player of
    Success player -> case I.allPositions player of
        Just positions -> not (null positions)
        Nothing -> False
    _ -> False

-- Lookup a playerId in an OfficialRoster
lookupPlayerInOfficialRoster :: O.PlayerID -> O.OfficialRoster -> Bool
lookupPlayerInOfficialRoster pid roster =
    any (\player -> O.playerId player == pid) (O.people roster)

lookupPlayerId :: Text -> IO Bool
lookupPlayerId playerIdText = do
    parsedRoster <- readJson "appData/rosters/activePlayers.json" :: IO (Either String O.OfficialRoster)
    case parsedRoster of
        Left _ -> return False
        Right activeRoster ->
            case O.textToPlayerID playerIdText of
                Just pid -> return $ lookupPlayerInOfficialRoster pid activeRoster
                Nothing -> return False

validatePlayerId :: R.CurrentLineup -> IO (Either [Text] String)
validatePlayerId lineup = do
    let allPlayers = getUniquePlayerIdsLineup lineup
    nonexistent <- filterM (fmap not . lookupPlayerIdConverted) allPlayers
    if null nonexistent
        -- Return a Right value indicating success; this should be a String message.
        then return $ Right "All Players are valid."
        -- Return a Left value indicating error; this should be a list of Text values.
        else return $ Left (map O.playerIDToText nonexistent)

-- Helper function to adapt lookupPlayerId for PlayerID values
lookupPlayerIdConverted :: O.PlayerID -> IO Bool
lookupPlayerIdConverted pid = lookupPlayerId (O.playerIDToText pid)

-- Utility function to convert an Int ID to Text
intToText :: Int -> Text
intToText = T.pack . show

getUniquePlayerIdsLineup :: R.CurrentLineup -> [O.PlayerID]
getUniquePlayerIdsLineup R.CurrentLineup{..} =
    cC ++ b1C ++ b2C ++ b3C ++ ssC ++ uC ++ ofC ++ spC ++ rpC

validatePositionCount :: String -> [a] -> Int -> Maybe (String, Int)
validatePositionCount positionName players maxAllowed
    | overage > 0 = Just (positionName, overage)
    | otherwise = Nothing
  where
    overage = length players - maxAllowed

totalPlayersInLineup :: R.CurrentLineup -> Int
totalPlayersInLineup R.CurrentLineup{..} =
    length cC + length b1C + length b2C + length b3C + length ssC + 
    length uC + length ofC + length spC + length rpC

validateCurrentLineup :: R.LgManager -> C.Configuration -> Bool
validateCurrentLineup R.LgManager{..} C.Configuration{point_parameters = C.PointParameters{lineup_limits = rosterConfig}} =
    let positionalValid = null (getLineupDiscrepancies current_lineup rosterConfig)
     in case hasUniqueLineupPlayers current_lineup of
            Left _ -> positionalValid
            Right _ -> False

-- takes a playerId as a String and a LgManager and returns the player's position or fails with an error message
-- addded failure cases to protect against players being in multiple positions or not being found in the lineup
-- even though this should be impossible if I make the lineup-setting tools correctly
findPlayerPosition :: O.PlayerID -> R.LgManager -> Either Text Text
findPlayerPosition playerId mgr = 
    case concatMap (findPosition playerId) checks of
        [] -> Left "Player not found in current lineup."
        [pos] -> Right pos
        _ -> Left "Player found in multiple positions in the lineup." -- This should not occur with proper management.
  where
    lineup = R.current_lineup mgr
    checks = 
        [ ("C", R.cC lineup)
        , ("1B", R.b1C lineup)
        , ("2B", R.b2C lineup)
        , ("3B", R.b3C lineup)
        , ("SS", R.ssC lineup)
        , ("U", R.uC lineup)
        , ("OF", R.ofC lineup)
        , ("SP", R.spC lineup)
        , ("RP", R.rpC lineup)
        ]
    findPosition pid (pos, players) = if pid `elem` players then [pos] else []

-- takes a playerId as a String and a LgManager
-- and returns whether the player is to be fielded as a batter or pitcher for points calculation purposes
batterOrPitcher :: O.PlayerID -> R.LgManager -> Either Text P.StatType
batterOrPitcher playerName mgr
    | any (\posList -> playerName `elem` posList) batterPositions = Right P.Batting
    | any (\posList -> playerName `elem` posList) pitcherPositions = Right P.Pitching
    | otherwise = Left "Player not found in current lineup."
  where
    lineup = R.current_lineup mgr
    batterPositions = [R.cC lineup, R.b1C lineup, R.b2C lineup, R.b3C lineup, R.ssC lineup, R.uC lineup] ++ [R.ofC lineup]
    pitcherPositions = [R.spC lineup] ++ [R.rpC lineup]


-- Draft vs. Ranking Validation

-- Adjusted to take an Int parameter for the number of lines
analyzeDraftResults :: C.Configuration -> (R.LgManager, R.LgManager) -> (PR.RankingData, PR.RankingData) -> Int -> IO ()
analyzeDraftResults config (team1, team2) (rankings1, rankings2) linesToPrint = do
  let team1Analysis = analyzeTeamDraft config team1 rankings1
      team2Analysis = analyzeTeamDraft config team2 rankings2

  putStrLn "Team 1 Draft Analysis:"
  printAnalysis team1Analysis linesToPrint

  putStrLn "\nTeam 2 Draft Analysis:"
  printAnalysis team2Analysis linesToPrint


-- Analyze a single team's draft
analyzeTeamDraft :: C.Configuration -> R.LgManager -> PR.RankingData -> [(Int, O.PlayerID, Bool)]
analyzeTeamDraft config lgManager rankingsData =
  let allDraftedPlayerIds = getAllPlayerIdsFromTeam lgManager
      rankingMap = M.fromList [(rank, playerId) | PR.PlayerRanking playerId rank <- PR.rankings rankingsData]
      analysis = map (\(rank, playerId) -> (rank, playerId, playerId `elem` allDraftedPlayerIds)) (M.toList rankingMap)
  in sortOn (\(rank, _, _) -> rank) analysis

-- Helper function to get all player IDs from a team's roster and lineup
getAllPlayerIdsFromTeam :: R.LgManager -> [O.PlayerID]
getAllPlayerIdsFromTeam lgManager =
  let rosterIds = concatMap ($ R.roster lgManager) [R.cR, R.b1R, R.b2R, R.b3R, R.ssR, R.ofR, R.uR, R.spR, R.rpR]
      lineupIds = concatMap ($ R.current_lineup lgManager) [R.cC, R.b1C, R.b2C, R.b3C, R.ssC, R.ofC, R.uC, R.spC, R.rpC]
  in rosterIds ++ lineupIds

-- Adjusted to take an Int parameter for the number of lines and limit the output
printAnalysis :: [(Int, O.PlayerID, Bool)] -> Int -> IO ()
printAnalysis analysis linesToPrint = mapM_ printEntry (take linesToPrint analysis)
  where
    printEntry (rank, playerId, drafted) =
      putStrLn $ "Rank: " ++ show rank ++ ", Player ID: " ++ show playerId ++ ", Drafted: " ++ show drafted
-- End of /home/bismuth/git/pelotero-engine/src/League/Validators.hs

-- Start of /home/bismuth/git/pelotero-engine/src/Playground.hs
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (filterM, when)
import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (
    FromJSON (parseJSON),
    Options (fieldLabelModifier),
    defaultOptions,
    eitherDecode,
    eitherDecodeStrict,
    encode,
    genericParseJSON,
    withObject,
    (.:),
    (.:?),
 )
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (
    Day,
    addDays,
    defaultTimeLocale,
    diffDays,
    formatTime,
    parseTimeOrError,
 )
import Data.Time.Clock ()
import Data.Time.Clock.POSIX ()
import Data.Time.Format ()
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Simple (
    getResponseBody,
    httpBS,
    parseRequest_,
 )
import System.Directory (createDirectoryIfMissing, doesFileExist)

import qualified Config as C
import qualified Input as I
import qualified Leaderboard as L
import qualified Middle as MI
import qualified OfficialRoster as O
import qualified Points as P
import qualified Roster as R

-- contains a lot of functionality
import Scraper
import Validators

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Match Funded by Two Managers (Can be Randomly Paired or Chosen)"
    putStrLn ""
    putStrLn ""
    putStrLn "Verify Player 1 & 2 Roster Interactive Validation of League Paramaters"
    putStrLn ""
    putStrLn "Scrape Active Rosters"
    putStrLn "Interactive Manager Player-Ranking for Auto draft"
    putStrLn "Match Funded by Two Players which finalizes everything"
    putStrLn "Auto-Draft"
    putStrLn ""
    putStrLn "Verify Player 1 & 2 Rosters"
    putStrLn ""
    putStrLn "Set Lineup"
    putStrLn "Verify Player 1 & 2 Lineup"
    putStrLn "  (players are able to edit their lineup until a specified time Before the natch begins)"
    putStrLn ""
    putStrLn "Wait for Match-Timeframe to Expire"
    putStrLn "Fetch Updated Stats"
    putStrLn ""
    putStrLn "Calculate & compare Player 1 & 2 Points in TimeFrame"
    putStrLn ""
    putStrLn "Disbursing Winnings to Higher Score"

{-

-- A (date String) -> [B] (list of gameIds/GameSchedule)
-- takes a date string "YYYY-MM-DD" and outputs a schedule bytestring of that day schdule
fetchGameScheduleForDate :: String -> IO (Either String I.GameSchedule)
fetchGameScheduleForDate date = do
    scheduleResult <- fetchAndDecodeJSON (scheduleUrl date)
    return $ fmap (assignDateToSchedule (T.pack date)) scheduleResult

-- B (gameId) -> C (status)
-- takes a gameId and returns IO (Either String LiveGameWrapper)
fetchGameStatus :: Int -> IO (Either String I.LiveGameWrapper)
fetchGameStatus gameId = fetchAndDecodeJSON (gameStatusUrl gameId)

-- B (gameId) -> C (status) -> D (boxscore)
-- takes a gameId and returns IO (Either String GameData)
fetchFinishedBxScore :: Int -> IO (Either String (Maybe L.GameData))
fetchFinishedBxScore gameId = do
    gameStatusResult <- fetchGameStatus gameId
    case gameStatusResult of
        Right gameDataWrapper -> do
            let liveStatusWrapper = gameData gameDataWrapper
            let liveStatus = gameStatus liveStatusWrapper
            if codedGameState liveStatus == "F"
               then do
                   boxscoreResult <- fetchAndDecodeJSON (boxScoreUrl gameId)
                   return $ fmap (Just . assignGameIdToPlayers gameId) boxscoreResult -- *adds gameId attribute to corresponding stats
               else return $ Right Nothing
        Left err -> return $ Left ("Error fetching game status: " ++ err)

-- -- [B] list of gameIds -> C status checks -> [D] list of boxscores
-- fetchFinishedBxScores :: [Int] -> IO (Either String (M.Map Int L.GameData))
fetchFinishedBxScores :: [Int] -> IO (Either String (M.Map Int (Maybe L.GameData)))
fetchFinishedBxScores gameIds = do
    results <- mapConcurrently fetchGame gameIds
    let combinedResults = sequenceA results -- Change the structure from [Either] to Either [..]
    return $ fmap (M.fromList . filter finishedGames) combinedResults
    where
        fetchGame gameId = do
            result <- fetchFinishedBxScore gameId
            return $ fmap (\d -> (gameId, d)) result
        finishedGames (_, Nothing) = False
        finishedGames (_, Just _) = True

-- ## OUTPUT CONVERSION ##
-- [B] list of gameIds -> C status checks -> [D] (list of box scores) -> [E] (list of player data)
fetchFinishedBxScoresToJsonPlayerSeasonStats :: [Int] -> IO (Either String (M.Map Text MI.JsonPlayerData))
fetchFinishedBxScoresToJsonPlayerSeasonStats gameIds = do
    gameDataResult <- fetchFinishedBxScores gameIds
    return $ fmap convertGameDataMapToJsonPlayerSeasonData gameDataResult

-- Fetch and decode utility
fetchAndDecodeJSON :: FromJSON a => String -> IO (Either String a)
fetchAndDecodeJSON url = do
    response <- httpBS (parseRequest_ url)
    return $ eitherDecodeStrict $ getResponseBody response

convertGameDataMapToJsonPlayerSeasonData :: M.Map Int (Maybe L.GameData) -> M.Map Text MI.JsonPlayerData
convertGameDataMapToJsonPlayerSeasonData maybeGameDataMap =
    foldl mergePlayerData M.empty allPlayerDataPairs
  where
    allPlayerDataPairs :: [(Text, MI.JsonPlayerData)]
    allPlayerDataPairs = concatMap gameDataToPlayerDataPairs (mapMaybe id (M.elems maybeGameDataMap))

    gameDataToPlayerDataPairs :: L.GameData -> [(Text, MI.JsonPlayerData)]
    gameDataToPlayerDataPairs gameData =
        let awayPlayers = M.elems $ L.players $ L.away $ L.teams gameData
            homePlayers = M.elems $ L.players $ L.home $ L.teams gameData
            allPlayers = awayPlayers ++ homePlayers
        in map (\player -> (rawStringToText $ MI.playerId (playerToJsonPlayerSeasonData player), playerToJsonPlayerSeasonData player)) allPlayers

    rawStringToText :: Text -> Text
    rawStringToText = T.replace "\\\"" "\"" . T.replace "\\\\" "\\"

    mergePlayerData :: M.Map Text MI.JsonPlayerData -> (Text, MI.JsonPlayerData) -> M.Map Text MI.JsonPlayerData
    mergePlayerData acc (playerId, newPlayerData) =
        let mergedData = case M.lookup playerId acc of
                Just existingPlayerData -> mergeJsonPlayerSeasonData existingPlayerData newPlayerData
                Nothing                 -> newPlayerData
        in M.insert playerId mergedData acc

mergeJsonPlayerSeasonData :: MI.JsonPlayerData -> MI.JsonPlayerData -> MI.JsonPlayerData
mergeJsonPlayerSeasonData existing new =
    MI.JsonPlayerData
        { MI.playerId = MI.playerId existing  -- assuming playerIds are the same, else there's a bigger problem!
        , MI.fullName = MI.fullName existing  -- assuming fullNames are the same
        , MI.stats = M.unionWith mergeJsonSeasonStatsData (MI.stats existing) (MI.stats new)
        }

mergeJsonSeasonStatsData :: MI.JsonStatsData -> MI.JsonStatsData -> MI.JsonStatsData
mergeJsonSeasonStatsData _ new = new

playerToJsonPlayerSeasonData :: L.Player -> MI.JsonPlayerData
playerToJsonPlayerSeasonData p =
    MI.JsonPlayerData
        { MI.playerId = T.pack $ show $ L.personId (L.person p)
        , MI.fullName = L.fullName (L.person p)
        , MI.stats = M.singleton (maybe "" (T.pack . show) (L.gameid p)) (playerToJsonSeasonStatsData p)
        }

playerToJsonSeasonStatsData :: L.Player -> MI.JsonStatsData
playerToJsonSeasonStatsData p =
    MI.JsonStatsData
        { MI.parentTeamId = L.parentTeamId p
        , MI.allPositions = fromMaybe [] (L.allPositions p)
        , MI.statusCode = L.status_code (L.status p)
        , MI.batting = L.batting (L.stats p)
        , MI.pitching = L.pitching (L.stats p)
        }

convertPlayerToJson :: L.Player -> ByteString
convertPlayerToJson = BL.toStrict . encode . playerToJsonPlayerSeasonData

-- unnnecessary for season stats
assignGameIdToPlayers :: Int -> L.GameData -> L.GameData
assignGameIdToPlayers gameId gameData =
    let assignToTeam team = team { L.players = M.map assignToPlayer (players team) }
        assignToPlayer player = player { L.gameid = Just gameId }
    in gameData { L.teams = (teams gameData) { L.away = assignToTeam (L.away (teams gameData)),
                                             L.home = assignToTeam (L.home (teams gameData)) } }

-- ## Output Stuff ##
-- we need to modify this to use the player list to find every single player's stats for the season
-- get player list and put it into an array of tuples with the second element being a Maybe
-- scan the seasonstats and pull only the most recent set of seasonstats into the comprehensive cumulative stat list
- write to a json file where player id's are the keys and the objects are the season stats and other LeaderboardInfo
processDate :: String -> IO ()
processDate date = do
    putStrLn $ "Processing " ++ date
    scheduleResult <- fetchGameScheduleForDate date
    processAndPrintGames scheduleResult
    case scheduleResult of
        Left err -> putStrLn $ "Failed to fetch game schedule: " ++ err
        Right schedule -> do
            let gameIds = extractGameIds schedule
            flattenedPlayersResult <- fetchFinishedBxScoresToJsonPlayerSeasonStats gameIds
            case flattenedPlayersResult of
                Left err -> putStrLn $ "Failed to process JSON: " ++ err
                Right _flattenedPlayers -> do
                    let filename = formatFilename date
                    writeDataToFile filename "appData/stats" _flattenedPlayers

-- Main scraper function tying everything together
scrapeStatsForDateRange :: String -> String -> IO ()
scrapeStatsForDateRange start end = do
    mapM_ processDate (generateDateRange start end)

flattenedPlayersList :: M.Map Text MI.JsonPlayerData -> M.Map Text MI.JsonPlayerData
flattenedPlayersList = id  -- or simply remove this function and use the map directly

-- takes a list of tuples game id's and game data and prints them
printGameData :: Either String (M.Map Int L.GameData) -> IO ()
printGameData gameDataMapEither =
    withEither (return gameDataMapEither) $ \gameDataMap ->
        mapM_ (\(gameId, gameData) -> putStrLn $ show gameId ++ ": " ++ show gameData) (M.toList gameDataMap)

processAndPrintGames :: Either String I.GameSchedule -> IO ()
processAndPrintGames gameScheduleEither =
    withEither (return gameScheduleEither) $ \gameSchedule ->
        if hasGamesForDate gameSchedule then do
            let gameIds = extractGameIds gameSchedule
            _ <- fetchFinishedBxScores gameIds
            return ()
        else putStrLn "No games scheduled for the provided date."

-- takes a season and outputs a roster bytestring of that season
-- fetchActiveRoster :: Int -> IO (Either String I.ActivePlayer)
fetchActiveRoster :: Int -> IO (Either String I.ActiveRoster)
fetchActiveRoster season = fetchAndDecodeJSON (rosterUrl season)

writeRosterToFile :: FilePath -> I.ActiveRoster -> IO ()
writeRosterToFile path roster = do
    -- Original player data encoding
    let playerData = encode (I.people roster)

    -- Compute checksum and get date stamp
    dateStamp <- getCurrentDate
    let checksumValue = computeChecksum playerData
    let fullRoster = I.ActiveRoster (I.people roster) (Just dateStamp) (Just checksumValue)

    -- Encode the full roster including the checksum and date stamp
    let jsonData = encode fullRoster

    -- Write to file
    BL.writeFile path jsonData

-- Edge Cases Handling
-- monadic error handling for fetching and decoding
withEither :: IO (Either String a) -> (a -> IO ()) -> IO ()
withEither action successHandler = do
    result <- action
    case result of
        Left err       -> putStrLn err
        Right dataPacket -> successHandler dataPacket

-- Special Enhancement of fromJSON types that gets called as post-processing in the fetch functions
assignDateToSchedule :: Text -> GameSchedule -> GameSchedule
assignDateToSchedule date schedule =
    let assignToDateEntry entry = entry { games = fmap (V.map assignToDate) (games entry) }
        assignToDate gameID = gameID { game_date = Just date }
    in schedule { dates = map assignToDateEntry (dates schedule) }

-- Takes a schedule bytestring and outputs true if games are happening, false otherwise.
hasGamesForDate :: I.GameSchedule -> Bool
hasGamesForDate schedule = any (isJust . games) (dates schedule)

-- Takes a schedule bytestring and outputs an array of gameId's or errors
extractGameIds :: I.GameSchedule -> [Int]
extractGameIds gameData = concatMap (maybe [] (V.toList . fmap gamePk) . games) (dates gameData)

-- ## FileName Manipulation Stuff
-- Takes a filename, path, and the data to save, then writes to a JSON file at the specified path with the given filename.
writeDataToFile :: FilePath -> FilePath -> M.Map Text MI.JsonPlayerData -> IO ()
writeDataToFile filename path dataToSave = do
    createOutputDirectory path
    let fullpath = path ++ "/" ++ filename
    BL.writeFile fullpath (encode dataToSave)

-- Write Player to JSON File
writePlayerToJsonFile :: FilePath -> L.Player -> IO ()
writePlayerToJsonFile path player = B.writeFile path (convertPlayerToJson player)

-- Takes a date string and formats it as a filename, like "2023_08_22.json".
formatFilename :: String -> String
formatFilename date = replace '-' '_' date ++ ".json"
  where
    replace old new = T.unpack . T.replace (T.pack [old]) (T.pack [new]) . T.pack

-- Create output directory
createOutputDirectory :: FilePath -> IO ()
createOutputDirectory = createDirectoryIfMissing True

-- ## Dates ##
-- Converts a String of format "YYYY-MM-DD" to a Day
stringToDay :: String -> Day
stringToDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

-- Increments the Day by one
incrementDay :: Day -> Day
incrementDay = addDays 1

-- Generates a list of dates from the start to the end
generateDateRange :: String -> String -> [String]
generateDateRange start end = map (formatTime defaultTimeLocale "%Y-%m-%d") dates
  where
    startDate = stringToDay start
    endDate = stringToDay end
    dates = takeWhile (<= endDate) $ iterate incrementDay startDate

-- Generate the API URL for a single day's schedule
scheduleUrl :: String -> String
scheduleUrl date = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date

-- Generate the API URL for live game status
gameStatusUrl :: Int -> String
gameStatusUrl gameId = "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live"

-- Generate the API URL for finished boxscore
boxScoreUrl :: Int -> String
boxScoreUrl gameId = "http://statsapi.mlb.com/api/v1/game/" ++ show gameId ++ "/boxscore"

-- Generate the API URL for specific years rosters
rosterUrl :: Int -> String
rosterUrl season = "https://statsapi.mlb.com/api/v1/sports/1/players?activeStatus=ACTIVE&season=" ++ show season

-- Generate the API URL for specific year's stat leaders in either batting or pitching (not working well)
seasonStatsUrl :: Int -> D.StatType -> String
seasonStatsUrl season statType = "http://statsapi.mlb.com/api/v1/stats?stats=season&sportId=1&season=" ++ show season ++ "&group=" ++ D.statTypeToString statType

computeChecksum :: BL.ByteString -> Text
computeChecksum bs = T.pack . show . hashWith SHA256 $ BL.toStrict bs

getCurrentDate :: IO Text
getCurrentDate = T.pack . formatTime defaultTimeLocale "%Y_%m_%d_%H_%M" <$> getCurrentTime

 -}
-- End of /home/bismuth/git/pelotero-engine/src/Playground.hs

-- Start of /home/bismuth/git/pelotero-engine/src/OfficialTest.hs
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified OfficialRoster as O

-- This function reads the JSON file and decodes it into your OfficialRoster data type.
decodeOfficialRosterFromFile :: FilePath -> IO (Either String O.OfficialRoster)
decodeOfficialRosterFromFile path = eitherDecode <$> BL.readFile path

-- This is your main function that will read, decode, and print the result.
main :: IO ()
main = do
    result <- decodeOfficialRosterFromFile "testFiles/appData/rosters/activePlayers.json"
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right roster -> print roster
-- End of /home/bismuth/git/pelotero-engine/src/OfficialTest.hs

-- Start of /home/bismuth/git/pelotero-engine/src/AutoDraft.hs
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main where

import Control.Monad (foldM, forM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Generics (Generic)

import Data.List (delete, find, findIndex, sortBy, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Config as C
import qualified OfficialRoster as O
import qualified PlayerRanking as PR
import qualified Roster as R
import Validators
import Utility
import Draft

main :: IO ()
main = do
    eitherR1 <- readJson "testFiles/appData/rankings/_087cc1f8c262_.json"
    case eitherR1 of
        Left error -> putStrLn $ "Failed to load rankings 1: " ++ show error
        Right r1 -> do
            eitherR2 <- readJson "testFiles/appData/rankings/_11817bfe52d3_.json"
            case eitherR2 of
                Left error -> putStrLn $ "Failed to load rankings 2: " ++ show error
                Right r2 -> do
                    eitherRoster <- readJson "testFiles/appData/rosters/activePlayers.json"
                    case eitherRoster of
                        Left error -> putStrLn $ "Failed to load rosters: " ++ show error
                        Right roster -> do
                            eitherConfig <- readJson "testFiles/appData/config/config.json"
                            case eitherConfig of
                                Left error -> putStrLn $ "Failed to load config: " ++ show error
                                Right config -> do
                                    let rankingsData = [r1, r2]
                                        op = O.people roster
                                        draftConfig = DraftConfig { cfg = config, officialPlayers = op }

                                    -- Initialize the draft state
                                    initialDraftState <- instantiateDraft config roster rankingsData
                                    
                                    -- Run the draft process
                                    finalDraftState <- draftPlayers draftConfig initialDraftState

                                    -- Extract final rosters and lineups
                                    let finalRostersAndLineups = map (\team -> (R.roster team, R.current_lineup team)) (teams finalDraftState)
                                        (finalRoster1, finalLineup1) = finalRostersAndLineups !! 0
                                        (finalRoster2, finalLineup2) = finalRostersAndLineups !! 1

                                    let teamId1 = C.teamId config !! 0
                                        teamId2 = C.teamId config !! 1
                                        teamId1Short = T.take 12 $ C.unwrapTeamID teamId1
                                        teamId2Short = T.take 12 $ C.unwrapTeamID teamId2

                                    let lgManager1 = createLgManager config teamId1 finalLineup1 finalRoster1
                                        lgManager2 = createLgManager config teamId2 finalLineup2 finalRoster2

                                    -- Analyze the draft results for both teams
                                    putStrLn "\nAnalyzing draft results for validity..."
                                    Validators.analyzeDraftResults config (lgManager1, lgManager2) (r1, r2) 20
                                    -- Write the LgManager instances to JSON files
                                    writeJson (T.unpack $ "testFiles/appData/draftResults/" <> teamId1Short <> "_draft_results_new.json") lgManager1
                                    writeJson (T.unpack $ "testFiles/appData/draftResults/" <> teamId2Short <> "_draft_results_new.json") lgManager2

                                    putStrLn "Draft, analysis, and LgManager serialization completed successfully."
-- End of /home/bismuth/git/pelotero-engine/src/AutoDraft.hs

-- Start of /home/bismuth/git/pelotero-engine/src/ADT/OfficialRoster.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module OfficialRoster where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict (HashMap)
import Data.Scientific (toBoundedInteger)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GHC.Generics

data OfficialRoster = OfficialRoster
    { people :: [OfficialPlayer]
    , dataPulled :: T.Text
    , checksum :: T.Text
    } deriving (Show, Generic)

data OfficialPlayer = OfficialPlayer
    { playerId :: PlayerID
    , useName :: T.Text
    , useLastName :: T.Text
    , nameSlug :: T.Text
    , currentTeam :: Int
    , primaryPosition :: T.Text
    , batSide :: T.Text
    , pitchHand :: T.Text
    , active :: Bool
    } deriving (Show, Eq, Generic)

newtype PlayerID = PlayerID Int deriving (Show, Eq)
newtype PlayerIDstring = PlayerIDstring T.Text deriving (Show, Eq)

-- Convert a PlayerID to Text
playerIDToText :: PlayerID -> T.Text
playerIDToText (PlayerID pid) = T.pack (show pid)

-- Parse a PlayerID from Text, safely
textToPlayerID :: T.Text -> Maybe PlayerID
textToPlayerID txt = case reads (T.unpack txt) :: [(Int, String)] of
    [(pid, "")] -> Just (PlayerID pid) -- Successful parse with no remainder
    _ -> Nothing -- Failed parse

unwrapPlayerId :: PlayerID -> Int
unwrapPlayerId (PlayerID pid) = pid

instance FromJSON OfficialRoster where
    parseJSON :: Value -> Parser OfficialRoster
    parseJSON = withObject "OfficialRoster" $ \v -> do
        checksum <- v .: "checksum"
        dataPulled <- v .: "dataPulled"
        playersObj <- v .: "officialPlayers" :: Parser (HashMap T.Text Value)
        let playersList = HM.elems playersObj
        people <- mapM parseJSON playersList
        return OfficialRoster{people = people, dataPulled = dataPulled, checksum = checksum}

instance ToJSON PlayerID where
    toJSON (PlayerID pid) = toJSON pid


instance FromJSON PlayerID where
    parseJSON = withScientific "PlayerID" $ \n -> do
        case toBoundedInteger n of
            Just pid -> pure (PlayerID pid)
            Nothing -> fail "PlayerID must be an integer"

-- instance FromJSON PlayerID where
--     parseJSON (Number n) = case toBoundedInteger n of
--         Just pid -> pure (PlayerID pid)
--         Nothing  -> fail "PlayerID must be an integer"
--     parseJSON (String s) = case textToPlayerID s of
--         Just pid -> pure pid
--         Nothing  -> fail "PlayerID string must represent an integer"
--     parseJSON _ = fail "PlayerID must be a number or string"

instance FromJSON OfficialPlayer where
    parseJSON = withObject "OfficialPlayer" $ \v -> do
        playerId <- v .: "playerId" -- Directly use FromJSON instance for PlayerID
        useName <- v .: "useName"
        useLastName <- v .: "useLastName"
        nameSlug <- v .: "nameSlug"
        currentTeam <- v .: "currentTeam"
        primaryPosition <- v .: "primaryPosition"
        batSide <- v .: "batSide"
        pitchHand <- v .: "pitchHand"
        active <- v .: "active"
        return OfficialPlayer{..}-- End of /home/bismuth/git/pelotero-engine/src/ADT/OfficialRoster.hs

-- Start of /home/bismuth/git/pelotero-engine/src/ADT/Input_trace.hs
{-# LANGUAGE OverloadedStrings #-}

module Input_trace where

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

-- Top level data type
data GameData = GameData
    { teams :: Teams
    }
    deriving (Show, Eq)

-- JSON instances
instance FromJSON GameData where
    parseJSON = withObject "GameData" $ \v ->
        GameData <$> v .: "teams"

-- Teams data type
data Teams = Teams
    { away :: TeamData
    , home :: TeamData
    }
    deriving (Show, Eq)

instance FromJSON Teams where
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
        pure TeamData{players = validPlayers}

type Players = [(Text, Player)]

-- Player data structure
data Player = Player
    { person :: Person
    , parentTeamId :: Int
    , allPositions :: Maybe [Position]
    , status :: Status
    , stats :: PlayerStats
    }
    deriving (Show, Eq)

instance FromJSON Player where
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
    { personId :: Int
    , fullName :: Text
    }
    deriving (Show, Eq)

instance FromJSON Person where
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
    parseJSON = withObject "Position" $ \v ->
        Position
            <$> v
                .: "code"

data Status = Status
    { status_code :: Text
    }
    deriving (Show, Eq)

instance FromJSON Status where
    parseJSON = withObject "Status" $ \v ->
        Status
            <$> v
                .: "code"

data PlayerStats = PlayerStats
    { batting :: Maybe BattingStats
    , pitching :: Maybe PitchingStats
    }
    deriving (Show, Eq)

instance FromJSON PlayerStats where
    parseJSON = withObject "PlayerStats" $ \v ->
        PlayerStats
            <$> v .:? "batting"
            <*> v .:? "pitching"

data BattingStats = BattingStats
    { bat_gamesPlayed :: Maybe Int
    , bat_flyOuts :: Maybe Int
    , bat_groundOuts :: Maybe Int
    , bat_runs :: Maybe Int
    , bat_doubles :: Maybe Int
    , bat_triples :: Maybe Int
    , bat_homeRuns :: Maybe Int
    , bat_strikeOuts :: Maybe Int
    , bat_baseOnBalls :: Maybe Int
    , bat_intentionalWalks :: Maybe Int
    , bat_hits :: Maybe Int
    , bat_hitByPitch :: Maybe Int
    , bat_atBats :: Maybe Int
    , bat_caughtStealing :: Maybe Int
    , bat_stolenBases :: Maybe Int
    , bat_groundIntoDoublePlay :: Maybe Int
    , bat_groundIntoTriplePlay :: Maybe Int
    , bat_plateAppearances :: Maybe Int
    , bat_totalBases :: Maybe Int
    , bat_rbi :: Maybe Int
    , bat_leftOnBase :: Maybe Int
    , bat_sacBunts :: Maybe Int
    , bat_sacFlies :: Maybe Int
    , bat_catchersInterference :: Maybe Int
    , bat_pickoffs :: Maybe Int
    }
    deriving (Show, Eq)

instance FromJSON BattingStats where
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
    { pit_gamesPlayed :: Maybe Int
    , pit_gamesStarted :: Maybe Int
    , pit_flyOuts :: Maybe Int
    , pit_groundOuts :: Maybe Int
    , pit_airOuts :: Maybe Int
    , pit_runs :: Maybe Int
    , pit_doubles :: Maybe Int
    , pit_triples :: Maybe Int
    , pit_homeRuns :: Maybe Int
    , pit_strikeOuts :: Maybe Int
    , pit_baseOnBalls :: Maybe Int
    , pit_intentionalWalks :: Maybe Int
    , pit_hits :: Maybe Int
    , pit_hitByPitch :: Maybe Int
    , pit_atBats :: Maybe Int
    , pit_caughtStealing :: Maybe Int
    , pit_stolenBases :: Maybe Int
    , pit_numberOfPitches :: Maybe Int
    , pit_inningsPitched :: Maybe Text
    , pit_wins :: Maybe Int
    , pit_losses :: Maybe Int
    , pit_saves :: Maybe Int
    , pit_saveOpportunities :: Maybe Int
    , pit_holds :: Maybe Int
    , pit_blownSaves :: Maybe Int
    , pit_earnedRuns :: Maybe Int
    , pit_battersFaced :: Maybe Int
    , pit_outs :: Maybe Int
    , pit_gamesPitched :: Maybe Int
    , pit_completeGames :: Maybe Int
    , pit_shutouts :: Maybe Int
    , pit_pitchesThrown :: Maybe Int
    , pit_balls :: Maybe Int
    , pit_strikes :: Maybe Int
    , pit_hitBatsmen :: Maybe Int
    , pit_balks :: Maybe Int
    , pit_wildPitches :: Maybe Int
    , pit_pickoffs :: Maybe Int
    , pit_rbi :: Maybe Int
    , pit_gamesFinished :: Maybe Int
    , pit_inheritedRunners :: Maybe Int
    , pit_inheritedRunnersScored :: Maybe Int
    , pit_catchersInterference :: Maybe Int
    , pit_sacBunts :: Maybe Int
    , pit_sacFlies :: Maybe Int
    , pit_passedBall :: Maybe Int
    }
    deriving (Show, Eq)

instance FromJSON PitchingStats where
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
-- End of /home/bismuth/git/pelotero-engine/src/ADT/Input_trace.hs

-- Start of /home/bismuth/git/pelotero-engine/src/ADT/Roster.hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Roster where

-- import Config (Configuration, parseDouble)
import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), ToJSON (..), Result (Success), Value, (.=), object, decode, eitherDecodeStrict, fromJSON, toJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..), withScientific, withText)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified OfficialRoster as O
import qualified Config as C

-- LgManager ADT

data LgManager = LgManager
    { status :: Text
    , commissioner :: Text
    , teamId :: C.TeamID
    , leagueID :: Text
    , current_lineup :: CurrentLineup
    , roster :: Roster
    }
    deriving (Show, Eq)

data CurrentLineup = CurrentLineup
    { cC  :: [O.PlayerID] 
    , b1C :: [O.PlayerID]
    , b2C :: [O.PlayerID]
    , b3C :: [O.PlayerID]
    , ssC :: [O.PlayerID]
    , ofC :: [O.PlayerID]
    , uC  :: [O.PlayerID] 
    , spC :: [O.PlayerID]
    , rpC :: [O.PlayerID]
    }
    deriving (Show, Eq)

data Roster = Roster
    { cR  :: [O.PlayerID]
    , b1R :: [O.PlayerID]
    , b2R :: [O.PlayerID]
    , b3R :: [O.PlayerID]
    , ssR :: [O.PlayerID]
    , ofR :: [O.PlayerID]
    , uR  :: [O.PlayerID]
    , spR :: [O.PlayerID]
    , rpR :: [O.PlayerID]
    }
    deriving (Show, Eq)

-- FromJSON Instances
instance FromJSON LgManager where
    parseJSON :: Value -> Parser LgManager
    parseJSON = withObject "LgManager" $ \v ->
        LgManager
            <$> v .: "status"
            <*> v .: "comissioner"
            <*> v .: "teamId"
            <*> v .: "leagueID"
            <*> v .: "current_lineup"
            <*> v .: "roster"

-- instance FromJSON C.TeamID where
--     parseJSON :: Value -> Parser C.TeamID
--     parseJSON = withText "TeamID" $ \t -> pure (C.TeamID t)

-- instance ToJSON C.TeamID where
--     toJSON (C.TeamID t) = toJSON t

instance FromJSON CurrentLineup where
    parseJSON :: Value -> Parser CurrentLineup
    parseJSON = withObject "CurrentLineup" $ \v ->
        CurrentLineup
            <$> v .: "C"
            <*> v .: "1B"
            <*> v .: "2B"
            <*> v .: "3B"
            <*> v .: "SS"
            <*> v .: "OF"
            <*> v .: "U"
            <*> v .: "SP"
            <*> v .: "RP"

instance FromJSON Roster where
    parseJSON :: Value -> Parser Roster
    parseJSON = withObject "Roster" $ \v ->
        Roster
            <$> v .: "C"
            <*> v .: "1B"
            <*> v .: "2B"
            <*> v .: "3B"
            <*> v .: "SS"
            <*> v .: "OF"
            <*> v .: "U"
            <*> v .: "SP"
            <*> v .: "RP"

-- ToJSON Instances
instance ToJSON LgManager where
    toJSON (LgManager status commissioner teamId leagueID currentLineup roster) =
        object
            [ "status" .= status
            , "commissioner" .= commissioner
            , "teamId" .= teamId
            , "leagueID" .= leagueID
            , "current_lineup" .= currentLineup
            , "roster" .= roster
            ]

instance ToJSON CurrentLineup where
    toJSON (CurrentLineup cC b1C b2C b3C ssC ofC uC spC rpC) =
        object
            [ "C" .= cC
            , "1B" .= b1C
            , "2B" .= b2C
            , "3B" .= b3C
            , "SS" .= ssC
            , "OF" .= ofC
            , "U" .= uC
            , "SP" .= spC
            , "RP" .= rpC
            ]

instance ToJSON Roster where
    toJSON (Roster cR b1R b2R b3R ssR ofR uR spR rpR) =
        object
            [ "C" .= cR
            , "1B" .= b1R
            , "2B" .= b2R
            , "3B" .= b3R
            , "SS" .= ssR
            , "OF" .= ofR
            , "U" .= uR
            , "SP" .= spR
            , "RP" .= rpR
            ]
-- End of /home/bismuth/git/pelotero-engine/src/ADT/Roster.hs

-- Start of /home/bismuth/git/pelotero-engine/src/ADT/Points.hs
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Points where

import Control.Monad (filterM)
import Data.Aeson (
    FromJSON (..),
    Result (Success),
    ToJSON (..),
    Value (..),
    decode,
    eitherDecodeStrict,
    encode,
    fromJSON,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
 )

import qualified Data.Aeson.Key as K
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (writeFile)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import GHC.Arr (array)
import Text.Read (readMaybe)

import qualified Config as C
import qualified Input as I
import qualified Middle as M
import qualified Roster as R
import qualified OfficialRoster as O

-- codify dinstinction between batting/pitching with boolean logic
data StatType = Batting | Pitching deriving (Show, Eq)

data PlayerResults
    = BattingResults (Maybe I.BattingStats)
    | PitchingResults (Maybe I.PitchingStats)
    | NoStats
    deriving (Show, Eq)

-- data type to hold final point totals for a single team, attributing them to each active playerId
data Results = Results
    { cC  :: [(O.PlayerID, Double)]
    , b1C :: [(O.PlayerID, Double)]
    , b2C :: [(O.PlayerID, Double)]
    , b3C :: [(O.PlayerID, Double)]
    , ssC :: [(O.PlayerID, Double)]
    , ofC :: [(O.PlayerID, Double)]
    , uC  :: [(O.PlayerID, Double)]
    , spC :: [(O.PlayerID, Double)]
    , rpC :: [(O.PlayerID, Double)]
    }
    deriving (Show, Eq)

-- data type for storing the top level unsummed points for that player for a given day which may contain many games as a batter or pitching or both
data GmPoints = GmPoints
    { gmpts_Id :: O.PlayerID  -- newly added playerId
    , gmpts_batting :: [Maybe BattingGmPoints]
    , gmpts_pitching :: [Maybe PitchingGmPoints]
    }
    deriving (Show, Eq)

-- we want to be more granular with our calculations for each game's stats by sending the calculated values for each game to this type
data BattingGmPoints = BattingGmPoints
    { gmb_gameId :: Text
    , gmb_total_points :: Double
    , gmb_single :: Double
    , gmb_double :: Double
    , gmb_triple :: Double
    , gmb_homerun :: Double
    , gmb_rbi :: Double
    , gmb_run :: Double
    , gmb_base_on_balls :: Double
    , gmb_stolen_base :: Double
    , gmb_hit_by_pitch :: Double
    , gmb_strikeout :: Double
    , gmb_caught_stealing :: Double
    }
    deriving (Show, Eq)

-- we want to be more granular with our calculations for each game's stats by sending the calculated values for each game to this type
data PitchingGmPoints = PitchingGmPoints
    { gmp_gameId :: Text
    , gmp_total_points :: Double
    , gmp_win :: Double
    , gmp_save :: Double
    , gmp_quality_start :: Double
    , gmp_inning_pitched :: Double
    , gmp_strikeout :: Double
    , gmp_complete_game :: Double
    , gmp_shutout :: Double
    , gmp_base_on_balls :: Double
    , gmp_hits_allowed :: Double
    , gmp_earned_runs :: Double
    , gmp_hit_batsman :: Double
    , gmp_loss :: Double
    }
    deriving (Show, Eq)
-- End of /home/bismuth/git/pelotero-engine/src/ADT/Points.hs

-- Start of /home/bismuth/git/pelotero-engine/src/ADT/PlayerRanking.hs
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
import qualified Config as C
import Data.Scientific (toBoundedInteger)

-- | Represents the top-level ranking data structure.
data RankingData = RankingData
    { teamId        :: C.TeamID
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
type DraftOrderStrategy = Int -> [C.TeamID] -> [C.TeamID]

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
    _ -> serpentineOrderStrategy -- Default-- End of /home/bismuth/git/pelotero-engine/src/ADT/PlayerRanking.hs

-- Start of /home/bismuth/git/pelotero-engine/src/ADT/Leaderboard.hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Leaderboard where

import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), Result (Success), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V

-- A Mofified Version of the Input fromJson stuff.
data SeasonData where
    SeasonData :: {teams :: Teams} -> SeasonData
    deriving (Show, Eq)

instance FromJSON SeasonData where
    parseJSON :: Value -> Parser SeasonData
    parseJSON = withObject "SeasonData" $ \v ->
        SeasonData <$> v .: "teams"

data Teams where
    Teams :: {away :: TeamData, home :: TeamData} -> Teams
    deriving (Show, Eq)

instance FromJSON Teams where
    parseJSON :: Value -> Parser Teams
    parseJSON = withObject "Teams" $ \v ->
        Teams
            <$> v
                .: "away"
            <*> v
                .: "home"

data TeamData where
    TeamData :: {players :: M.Map Text Player} -> TeamData
    deriving (Show, Eq)

instance FromJSON TeamData where
    parseJSON :: Value -> Parser TeamData
    parseJSON = withObject "TeamData" $ \v -> do
        playersMap <- v .: "players"
        validPlayers <- traverse parseJSON playersMap
        pure TeamData{players = validPlayers}

type Players = [(Text, Player)]

data Player where
    Player ::
        { person :: Person
        , status :: Status
        , seasonStats :: PlayerSeasonStats
        } ->
        Player
    deriving (Show, Eq)

instance FromJSON Player where
    parseJSON :: Value -> Parser Player
    parseJSON = withObject "Player" $ \v -> do
        person <- v .: "person"
        status <- v .: "status"
        seasonStats <- v .: "seasonStats"
        return $ Player person status seasonStats

data Person where
    Person :: {personId :: Int, fullName :: Text} -> Person
    deriving (Show, Eq)

instance FromJSON Person where
    parseJSON :: Value -> Parser Person
    parseJSON = withObject "Person" $ \v ->
        Person
            <$> v
                .: "id"
            <*> v
                .: "fullName"

data Status where
    Status :: {status_code :: Text} -> Status
    deriving (Show, Eq)

instance FromJSON Status where
    parseJSON :: Value -> Parser Status
    parseJSON = withObject "Status" $ \v ->
        Status
            <$> v
                .: "code"

data PlayerSeasonStats where
    PlayerSeasonStats ::
        { batting :: Maybe BattingTotals
        , pitching :: Maybe PitchingTotals
        } ->
        PlayerSeasonStats
    deriving (Show, Eq)

instance FromJSON PlayerSeasonStats where
    parseJSON :: Value -> Parser PlayerSeasonStats
    parseJSON = withObject "PlayerSeasonStats" $ \v ->
        PlayerSeasonStats
            <$> v .:? "batting"
            <*> v .:? "pitching"

data BattingTotals where
    BattingTotals ::
        { bat_T_gamesPlayed :: Maybe Int
        , bat_T_flyOuts :: Maybe Int
        , bat_T_groundOuts :: Maybe Int
        , bat_T_runs :: Maybe Int
        , bat_T_doubles :: Maybe Int
        , bat_T_triples :: Maybe Int
        , bat_T_homeRuns :: Maybe Int
        , bat_T_strikeOuts :: Maybe Int
        , bat_T_baseOnBalls :: Maybe Int
        , bat_T_intentionalWalks :: Maybe Int
        , bat_T_hits :: Maybe Int
        , bat_T_hitByPitch :: Maybe Int
        , bat_T_avg :: Maybe Text
        , bat_T_atBats :: Maybe Int
        , bat_T_obp :: Maybe Text
        , bat_T_slg :: Maybe Text
        , bat_T_ops :: Maybe Text
        , bat_T_caughtStealing :: Maybe Int
        , bat_T_stolenBases :: Maybe Int
        , bat_T_stolenBasePercentage :: Maybe Text
        , bat_T_groundIntoDoublePlay :: Maybe Int
        , bat_T_groundIntoTriplePlay :: Maybe Int
        , bat_T_plateAppearances :: Maybe Int
        , bat_T_totalBases :: Maybe Int
        , bat_T_rbi :: Maybe Int
        , bat_T_leftOnBase :: Maybe Int
        , bat_T_sacBunts :: Maybe Int
        , bat_T_sacFlies :: Maybe Int
        , bat_T_babip :: Maybe Text
        , bat_T_catchersInterference :: Maybe Int
        , bat_T_pickoffs :: Maybe Int
        , bat_T_atBatsPerHomeRun :: Maybe Text
        } ->
        BattingTotals
    deriving (Show, Eq)

instance FromJSON BattingTotals where
    parseJSON :: Value -> Parser BattingTotals
    parseJSON = withObject "BattingTotals" $ \v ->
        BattingTotals
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
            <*> v .:? "avg"
            <*> v .:? "atBats"
            <*> v .:? "obp"
            <*> v .:? "slg"
            <*> v .:? "ops"
            <*> v .:? "caughtStealing"
            <*> v .:? "stolenBases"
            <*> v .:? "stolenBasePercentage"
            <*> v .:? "groundIntoDoublePlay"
            <*> v .:? "groundIntoTriplePlay"
            <*> v .:? "plateAppearances"
            <*> v .:? "totalBases"
            <*> v .:? "rbi"
            <*> v .:? "leftOnBase"
            <*> v .:? "sacBunts"
            <*> v .:? "sacFlies"
            <*> v .:? "babip"
            <*> v .:? "catchersInterference"
            <*> v .:? "pickoffs"
            <*> v .:? "atBatsPerHomeRun"

-- Pitching Season Stuff (seasonStats.pitching inside of boxscore)
data PitchingTotals where
    PitchingTotals ::
        { pit_T_gamesPlayed :: Maybe Int
        , pit_T_gamesStarted :: Maybe Int
        , pit_T_groundOuts :: Maybe Int
        , pit_T_airOuts :: Maybe Int
        , pit_T_runs :: Maybe Int
        , pit_T_doubles :: Maybe Int
        , pit_T_triples :: Maybe Int
        , pit_T_homeRuns :: Maybe Int
        , pit_T_strikeOuts :: Maybe Int
        , pit_T_baseOnBalls :: Maybe Int
        , pit_T_intentionalWalks :: Maybe Int
        , pit_T_hits :: Maybe Int
        , pit_T_hitByPitch :: Maybe Int
        , pit_T_atBats :: Maybe Int
        , pit_T_obp :: Maybe Text
        , pit_T_caughtStealing :: Maybe Int
        , pit_T_stolenBases :: Maybe Int
        , pit_T_stolenBasePercentage :: Maybe Text
        , pit_T_numberOfPitches :: Maybe Int
        , pit_T_era :: Maybe Text
        , pit_T_inningsPitched :: Maybe Text
        , pit_T_wins :: Maybe Int
        , pit_T_losses :: Maybe Int
        , pit_T_saves :: Maybe Int
        , pit_T_saveOpportunities :: Maybe Int
        , pit_T_holds :: Maybe Int
        , pit_T_blownSaves :: Maybe Int
        , pit_T_earnedRuns :: Maybe Int
        , pit_T_whip :: Maybe Text
        , pit_T_battersFaced :: Maybe Int
        , pit_T_outs :: Maybe Int
        , pit_T_gamesPitched :: Maybe Int
        , pit_T_completeGames :: Maybe Int
        , pit_T_shutouts :: Maybe Int
        , pit_T_pitchesThrown :: Maybe Int
        , pit_T_balls :: Maybe Int
        , pit_T_strikes :: Maybe Int
        , pit_T_strikePercentage :: Maybe Text
        , pit_T_hitBatsmen :: Maybe Int
        , pit_T_balks :: Maybe Int
        , pit_T_wildPitches :: Maybe Int
        , pit_T_pickoffs :: Maybe Int
        , pit_T_groundOutsToAirouts :: Maybe Text
        , pit_T_rbi :: Maybe Int
        , pit_T_winPercentage :: Maybe Text
        , pit_T_pitchesPerInning :: Maybe Text
        , pit_T_gamesFinished :: Maybe Int
        , pit_T_strikeoutWalkRatio :: Maybe Text
        , pit_T_strikeoutsPer9Inn :: Maybe Text
        , pit_T_walksPer9Inn :: Maybe Text
        , pit_T_hitsPer9Inn :: Maybe Text
        , pit_T_runsScoredPer9 :: Maybe Text
        , pit_T_homeRunsPer9 :: Maybe Text
        , pit_T_inheritedRunners :: Maybe Int
        , pit_T_inheritedRunnersScored :: Maybe Int
        , pit_T_catchersInterference :: Maybe Int
        , pit_T_sacBunts :: Maybe Int
        , pit_T_sacFlies :: Maybe Int
        , pit_T_passedBall :: Maybe Int
        } ->
        PitchingTotals
    deriving (Show, Eq)

instance FromJSON PitchingTotals where
    parseJSON :: Value -> Parser PitchingTotals
    parseJSON = withObject "PitchingTotals" $ \v ->
        PitchingTotals
            <$> v .:? "gamesPlayed"
            <*> v .:? "gamesStarted"
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
            <*> v .:? "obp"
            <*> v .:? "caughtStealing"
            <*> v .:? "stolenBases"
            <*> v .:? "stolenBasePercentage"
            <*> v .:? "numberOfPitches"
            <*> v .:? "era"
            <*> v .:? "inningsPitched"
            <*> v .:? "wins"
            <*> v .:? "losses"
            <*> v .:? "saves"
            <*> v .:? "saveOpportunities"
            <*> v .:? "holds"
            <*> v .:? "blownSaves"
            <*> v .:? "earnedRuns"
            <*> v .:? "whip"
            <*> v .:? "battersFaced"
            <*> v .:? "outs"
            <*> v .:? "gamesPitched"
            <*> v .:? "completeGames"
            <*> v .:? "shutouts"
            <*> v .:? "pitchesThrown"
            <*> v .:? "balls"
            <*> v .:? "strikes"
            <*> v .:? "strikePercentage"
            <*> v .:? "hitBatsmen"
            <*> v .:? "balks"
            <*> v .:? "wildPitches"
            <*> v .:? "pickoffs"
            <*> v .:? "groundOutsToAirouts"
            <*> v .:? "rbi"
            <*> v .:? "winPercentage"
            <*> v .:? "pitchesPerInning"
            <*> v .:? "gamesFinished"
            <*> v .:? "strikeoutWalkRatio"
            <*> v .:? "strikeoutsPer9Inn"
            <*> v .:? "walksPer9Inn"
            <*> v .:? "hitsPer9Inn"
            <*> v .:? "runsScoredPer9"
            <*> v .:? "homeRunsPer9"
            <*> v .:? "inheritedRunners"
            <*> v .:? "inheritedRunnersScored"
            <*> v .:? "catchersInterference"
            <*> v .:? "sacBunts"
            <*> v .:? "sacFlies"
            <*> v .:? "passedBall"
-- End of /home/bismuth/git/pelotero-engine/src/ADT/Leaderboard.hs

-- Start of /home/bismuth/git/pelotero-engine/src/ADT/Config.hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Config where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Aeson (
    FromJSON (..),
    Result (Success),
    ToJSON (..),
    Value,
    decode,
    eitherDecodeStrict,
    fromJSON,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
 )

import Data.Aeson.Types (Parser, Result (..), withScientific, withText)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import qualified Data.Vector as V

-- import qualified Roster as R

-- ## League Configuration ADT ## --

newtype TeamID = TeamID T.Text deriving (Show, Eq, Ord)

unwrapTeamID :: TeamID -> T.Text
unwrapTeamID (TeamID text) = text

data Configuration = Configuration
    { status :: T.Text
    , leagueID :: T.Text
    , point_parameters :: PointParameters
    , draft_parameters :: DraftParameters
    , commissioner :: T.Text
    , teamId :: [TeamID]
    }
    deriving (Show, Eq)

data PointParameters = PointParameters
    { lg_style :: T.Text
    , start_UTC :: T.Text
    , end_UTC :: T.Text
    , lg_battingMults :: BattingMults
    , lg_pitchingMults :: PitchingMults
    , lineup_limits :: LgLineupLmts
    }
    deriving (Show, Eq)

data BattingMults = BattingMults
    { lgb_single :: Double
    , lgb_double :: Double
    , lgb_triple :: Double
    , lgb_homerun :: Double
    , lgb_rbi :: Double
    , lgb_run :: Double
    , lgb_base_on_balls :: Double
    , lgb_stolen_base :: Double
    , lgb_hit_by_pitch :: Double
    , lgb_strikeout :: Double
    , lgb_caught_stealing :: Double
    }
    deriving (Show, Eq)

data PitchingMults = PitchingMults
    { lgp_win :: Double
    , lgp_save :: Double
    , lgp_quality_start :: Double
    , lgp_inning_pitched :: Double
    , lgp_strikeout :: Double
    , lgp_complete_game :: Double
    , lgp_shutout :: Double
    , lgp_base_on_balls :: Double
    , lgp_hits_allowed :: Double
    , lgp_earned_runs :: Double
    , lgp_hit_batsman :: Double
    , lgp_loss :: Double
    }
    deriving (Show, Eq)

data LgLineupLmts = LgLineupLmts
    { lg_catcher :: Int
    , lg_first :: Int
    , lg_second :: Int
    , lg_third :: Int
    , lg_shortstop :: Int
    , lg_outfield :: Int
    , lg_utility :: Int
    , lg_s_pitcher :: Int
    , lg_r_pitcher :: Int
    , lg_max_size :: Int
    }
    deriving (Show, Eq)

type DraftOrder = [(TeamID, Int)] -- (teamId, order in draft)

data DraftParameters = DraftParameters
    { autoDraft :: Bool
    , order :: T.Text -- "serpentine" should be the default
    , autoDraft_UTC :: T.Text
    , draft_limits :: DraftRosterLmts
    }
    deriving (Show, Eq)

data DraftRosterLmts = DraftRosterLmts
    { dr_catcher :: Int
    , dr_first :: Int
    , dr_second :: Int
    , dr_third :: Int
    , dr_shortstop :: Int
    , dr_outfield :: Int
    , dr_utility :: Int
    , dr_s_pitcher :: Int
    , dr_r_pitcher :: Int
    }
    deriving (Show, Eq)

sumDraftRosterLmts :: DraftRosterLmts -> Int
sumDraftRosterLmts lmts = 
    dr_catcher lmts +
    dr_first lmts +
    dr_second lmts +
    dr_third lmts +
    dr_shortstop lmts +
    dr_outfield lmts +
    dr_utility lmts +
    dr_s_pitcher lmts +
    dr_r_pitcher lmts

-- FromJSON Instances
instance FromJSON Configuration where
    parseJSON = withObject "Configuration" $ \v ->
        Configuration
            <$> v
            .: "status"
            <*> v
            .: "leagueID"
            <*> v
            .: "point_parameters"
            <*> v
            .: "draft_parameters"
            <*> v
            .: "commissioner"
            <*> v
            .: "teamId"

instance FromJSON TeamID where
    parseJSON = withText "TeamID" $ pure . TeamID

instance FromJSON PointParameters where
    parseJSON = withObject "PointParameters" $ \v ->
        PointParameters
            <$> v
            .: "style"
            <*> v
            .: "start_UTC"
            <*> v
            .: "end_UTC"
            <*> v
            .: "batting"
            <*> v
            .: "pitching"
            <*> v
            .: "lineup_limits"

parseDouble :: Value -> Parser Double
parseDouble = withText "double" $ \t ->
    case reads (T.unpack t) :: [(Double, String)] of
        [(d, "")] -> return d
        _ -> fail "Could not parse double from string"

instance FromJSON BattingMults where
    parseJSON = withObject "BattingMults" $ \v -> do
        lgb_single <- v .: "single" >>= parseDouble
        lgb_double <- v .: "double" >>= parseDouble
        lgb_triple <- v .: "triple" >>= parseDouble
        lgb_homerun <- v .: "homerun" >>= parseDouble
        lgb_rbi <- v .: "rbi" >>= parseDouble
        lgb_run <- v .: "run" >>= parseDouble
        lgb_base_on_balls <- v .: "base_on_balls" >>= parseDouble
        lgb_stolen_base <- v .: "stolen_base" >>= parseDouble
        lgb_hit_by_pitch <- v .: "hit_by_pitch" >>= parseDouble
        lgb_strikeout <- v .: "strikeout" >>= parseDouble
        lgb_caught_stealing <- v .: "caught_stealing" >>= parseDouble

        return BattingMults{..}

instance FromJSON PitchingMults where
    parseJSON = withObject "PitchingMults" $ \v -> do
        lgp_win <- v .: "win" >>= parseDouble
        lgp_save <- v .: "save" >>= parseDouble
        lgp_quality_start <- v .: "quality_start" >>= parseDouble
        lgp_inning_pitched <- v .: "inning_pitched" >>= parseDouble
        lgp_strikeout <- v .: "strikeout" >>= parseDouble
        lgp_complete_game <- v .: "complete_game" >>= parseDouble
        lgp_shutout <- v .: "shutout" >>= parseDouble
        lgp_base_on_balls <- v .: "base_on_balls" >>= parseDouble
        lgp_hits_allowed <- v .: "hits_allowed" >>= parseDouble
        lgp_earned_runs <- v .: "earned_runs" >>= parseDouble
        lgp_hit_batsman <- v .: "hit_batsman" >>= parseDouble
        lgp_loss <- v .: "loss" >>= parseDouble

        return PitchingMults{..}

instance FromJSON LgLineupLmts where
    parseJSON = withObject "LgLineupLmts" $ \v ->
        LgLineupLmts
            <$> v
            .: "catcher"
            <*> v
            .: "first"
            <*> v
            .: "second"
            <*> v
            .: "third"
            <*> v
            .: "shortstop"
            <*> v
            .: "outfield"
            <*> v
            .: "utility"
            <*> v
            .: "s_pitcher"
            <*> v
            .: "r_pitcher"
            <*> v
            .: "max_size"

instance FromJSON DraftParameters where
    parseJSON :: Value -> Parser DraftParameters
    parseJSON = withObject "DraftParameters" $ \v ->
        DraftParameters
            <$> v
            .: "autoDraft"
            <*> v
            .: "order"
            <*> v
            .: "autoDraft_UTC"
            <*> v
            .: "draft_limits"

instance FromJSON DraftRosterLmts where
    parseJSON :: Value -> Parser DraftRosterLmts
    parseJSON = withObject "DraftRosterLmts" $ \v ->
        DraftRosterLmts
            <$> v
            .: "catcher"
            <*> v
            .: "first"
            <*> v
            .: "second"
            <*> v
            .: "third"
            <*> v
            .: "shortstop"
            <*> v
            .: "outfield"
            <*> v
            .: "utility"
            <*> v
            .: "s_pitcher"
            <*> v
            .: "r_pitcher"

-- ToJSON Instances
instance ToJSON Configuration where
    toJSON :: Configuration -> Value
    toJSON Configuration{..} =
        object
            [ "status" .= status
            , "leagueID" .= leagueID
            , "point_parameters" .= point_parameters
            , "draft_parameters" .= draft_parameters
            , "commissioner" .= commissioner
            , "teamId" .= teamId
            ]

instance ToJSON TeamID where
    toJSON (TeamID t) = toJSON t

instance ToJSON PointParameters where
    toJSON :: PointParameters -> Value
    toJSON PointParameters{..} =
        object
            [ "style" .= lg_style
            , "start_UTC" .= start_UTC
            , "end_UTC" .= end_UTC
            , "batting" .= lg_battingMults
            , "pitching" .= lg_pitchingMults
            , "lineup_limits" .= lineup_limits
            ]

instance ToJSON BattingMults where
    toJSON :: BattingMults -> Value
    toJSON BattingMults{..} =
        object
            [ "single" .= lgb_single
            , "double" .= lgb_double
            , "triple" .= lgb_triple
            , "homerun" .= lgb_homerun
            , "rbi" .= lgb_rbi
            , "run" .= lgb_run
            , "base_on_balls" .= lgb_base_on_balls
            , "stolen_base" .= lgb_stolen_base
            , "hit_by_pitch" .= lgb_hit_by_pitch
            , "strikeout" .= lgb_strikeout
            , "caught_stealing" .= lgb_caught_stealing
            ]

instance ToJSON PitchingMults where
    toJSON :: PitchingMults -> Value
    toJSON PitchingMults{..} =
        object
            [ "win" .= lgp_win
            , "save" .= lgp_save
            , "quality_start" .= lgp_quality_start
            , "inning_pitched" .= lgp_inning_pitched
            , "strikeout" .= lgp_strikeout
            , "complete_game" .= lgp_complete_game
            , "shutout" .= lgp_shutout
            , "base_on_balls" .= lgp_base_on_balls
            , "hits_allowed" .= lgp_hits_allowed
            , "earned_runs" .= lgp_earned_runs
            , "hit_batsman" .= lgp_hit_batsman
            , "loss" .= lgp_loss
            ]

instance ToJSON LgLineupLmts where
    toJSON :: LgLineupLmts -> Value
    toJSON LgLineupLmts{..} =
        object
            [ "catcher" .= lg_catcher
            , "first" .= lg_first
            , "second" .= lg_second
            , "third" .= lg_third
            , "shortstop" .= lg_shortstop
            , "outfield" .= lg_outfield
            , "utility" .= lg_utility
            , "s_pitcher" .= lg_s_pitcher
            , "r_pitcher" .= lg_r_pitcher
            , "max_size" .= lg_max_size
            ]

instance ToJSON DraftParameters where
    toJSON :: DraftParameters -> Value
    toJSON DraftParameters{..} =
        object
            [ "autoDraft" .= autoDraft
            , "autoDraft_UTC" .= autoDraft_UTC
            , "draft_limits" .= draft_limits
            ]

instance ToJSON DraftRosterLmts where
    toJSON :: DraftRosterLmts -> Value
    toJSON DraftRosterLmts{..} =
        object
            [ "catcher" .= dr_catcher
            , "first" .= dr_first
            , "second" .= dr_second
            , "third" .= dr_third
            , "shortstop" .= dr_shortstop
            , "outfield" .= dr_outfield
            , "utility" .= dr_utility
            , "s_pitcher" .= dr_s_pitcher
            , "r_pitcher" .= dr_r_pitcher
            ]-- End of /home/bismuth/git/pelotero-engine/src/ADT/Config.hs

-- Start of /home/bismuth/git/pelotero-engine/src/ADT/Middle.hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Middle where

import Control.Monad (filterM)
import Data.Aeson (
    Result (Success),
    ToJSON (..),
    Value (..),
    encode,
    fromJSON,
    object,
    (.=),
 )
import Data.Aeson.Key as K
import qualified Data.Aeson.Key as K
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (writeFile)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Arr (array)

import qualified Input as I

data JsonPlayerData where
    JsonPlayerData ::
        { playerId :: Text
        , fullName :: Text
        , stats :: M.Map Text JsonStatsData
        } ->
        JsonPlayerData
    deriving (Show, Eq)

data JsonStatsData where
    JsonStatsData ::
        { parentTeamId :: Int
        , allPositions :: [I.Position]
        , statusCode :: Text
        , batting :: Maybe I.BattingStats
        , pitching :: Maybe I.PitchingStats
        } ->
        JsonStatsData
    deriving (Show, Eq)

instance ToJSON JsonPlayerData where
    toJSON :: JsonPlayerData -> Value
    toJSON (JsonPlayerData pid fname sts) =
        object ["fullName" .= fname, "player_id" .= pid, "stats" .= sts]

instance ToJSON JsonStatsData where
    toJSON :: JsonStatsData -> Value
    toJSON (JsonStatsData pId allPos statCode bat pitch) =
        object ["parentTeamId" .= pId, "allPositions" .= allPos, "status" .= statCode, "batting" .= bat, "pitching" .= pitch]

instance ToJSON I.Position where
    toJSON :: I.Position -> Value
    toJSON (I.Position allPositions) =
        let positionValue = read (T.unpack allPositions) :: Int
         in Number (fromIntegral positionValue)

instance ToJSON I.PitchingStats where
    toJSON :: I.PitchingStats -> Value
    toJSON pitStats =
        object $
            catMaybes
                [ fmap ("pit_gamesPlayed" .=) (I.pit_gamesPlayed pitStats)
                , fmap ("pit_gamesStarted" .=) (I.pit_gamesStarted pitStats)
                , fmap ("pit_flyOuts" .=) (I.pit_flyOuts pitStats)
                , fmap ("pit_groundOuts" .=) (I.pit_groundOuts pitStats)
                , fmap ("pit_airOuts" .=) (I.pit_airOuts pitStats)
                , fmap ("pit_runs" .=) (I.pit_runs pitStats)
                , fmap ("pit_doubles" .=) (I.pit_doubles pitStats)
                , fmap ("pit_triples" .=) (I.pit_triples pitStats)
                , fmap ("pit_homeRuns" .=) (I.pit_homeRuns pitStats)
                , fmap ("pit_strikeOuts" .=) (I.pit_strikeOuts pitStats)
                , fmap ("pit_baseOnBalls" .=) (I.pit_baseOnBalls pitStats)
                , fmap ("pit_intentionalWalks" .=) (I.pit_intentionalWalks pitStats)
                , fmap ("pit_hits" .=) (I.pit_hits pitStats)
                , fmap ("pit_hitByPitch" .=) (I.pit_hitByPitch pitStats)
                , fmap ("pit_atBats" .=) (I.pit_atBats pitStats)
                , fmap ("pit_caughtStealing" .=) (I.pit_caughtStealing pitStats)
                , fmap ("pit_stolenBases" .=) (I.pit_stolenBases pitStats)
                , fmap ("pit_numberOfPitches" .=) (I.pit_numberOfPitches pitStats)
                , fmap ("pit_inningsPitched" .=) (I.pit_inningsPitched pitStats)
                , fmap ("pit_wins" .=) (I.pit_wins pitStats)
                , fmap ("pit_losses" .=) (I.pit_losses pitStats)
                , fmap ("pit_saves" .=) (I.pit_saves pitStats)
                , fmap ("pit_saveOpportunities" .=) (I.pit_saveOpportunities pitStats)
                , fmap ("pit_holds" .=) (I.pit_holds pitStats)
                , fmap ("pit_blownSaves" .=) (I.pit_blownSaves pitStats)
                , fmap ("pit_earnedRuns" .=) (I.pit_earnedRuns pitStats)
                , fmap ("pit_battersFaced" .=) (I.pit_battersFaced pitStats)
                , fmap ("pit_outs" .=) (I.pit_outs pitStats)
                , fmap ("pit_gamesPitched" .=) (I.pit_gamesPitched pitStats)
                , fmap ("pit_completeGames" .=) (I.pit_completeGames pitStats)
                , fmap ("pit_shutouts" .=) (I.pit_shutouts pitStats)
                , fmap ("pit_pitchesThrown" .=) (I.pit_pitchesThrown pitStats)
                , fmap ("pit_balls" .=) (I.pit_balls pitStats)
                , fmap ("pit_strikes" .=) (I.pit_strikes pitStats)
                , fmap ("pit_hitBatsmen" .=) (I.pit_hitBatsmen pitStats)
                , fmap ("pit_balks" .=) (I.pit_balks pitStats)
                , fmap ("pit_wildPitches" .=) (I.pit_wildPitches pitStats)
                , fmap ("pit_pickoffs" .=) (I.pit_pickoffs pitStats)
                , fmap ("pit_rbi" .=) (I.pit_rbi pitStats)
                , fmap ("pit_gamesFinished" .=) (I.pit_gamesFinished pitStats)
                , fmap ("pit_inheritedRunners" .=) (I.pit_inheritedRunners pitStats)
                , fmap ("pit_inheritedRunnersScored" .=) (I.pit_inheritedRunnersScored pitStats)
                , fmap ("pit_catchersInterference" .=) (I.pit_catchersInterference pitStats)
                , fmap ("pit_sacBunts" .=) (I.pit_sacBunts pitStats)
                , fmap ("pit_sacFlies" .=) (I.pit_sacFlies pitStats)
                , fmap ("pit_passedBall" .=) (I.pit_passedBall pitStats)
                ]

instance ToJSON I.BattingStats where
    toJSON :: I.BattingStats -> Value
    toJSON batStats =
        object $
            catMaybes
                [ fmap ("bat_gamesPlayed" .=) (I.bat_gamesPlayed batStats)
                , fmap ("bat_flyOuts" .=) (I.bat_flyOuts batStats)
                , fmap ("bat_groundOuts" .=) (I.bat_groundOuts batStats)
                , fmap ("bat_runs" .=) (I.bat_runs batStats)
                , fmap ("bat_doubles" .=) (I.bat_doubles batStats)
                , fmap ("bat_triples" .=) (I.bat_triples batStats)
                , fmap ("bat_homeRuns" .=) (I.bat_homeRuns batStats)
                , fmap ("bat_strikeOuts" .=) (I.bat_strikeOuts batStats)
                , fmap ("bat_baseOnBalls" .=) (I.bat_baseOnBalls batStats)
                , fmap ("bat_intentionalWalks" .=) (I.bat_intentionalWalks batStats)
                , fmap ("bat_hits" .=) (I.bat_hits batStats)
                , fmap ("bat_hitByPitch" .=) (I.bat_hitByPitch batStats)
                , fmap ("bat_atBats" .=) (I.bat_atBats batStats)
                , fmap ("bat_caughtStealing" .=) (I.bat_caughtStealing batStats)
                , fmap ("bat_stolenBases" .=) (I.bat_stolenBases batStats)
                , fmap ("bat_groundIntoDoublePlay" .=) (I.bat_groundIntoDoublePlay batStats)
                , fmap ("bat_groundIntoTriplePlay" .=) (I.bat_groundIntoTriplePlay batStats)
                , fmap ("bat_plateAppearances" .=) (I.bat_plateAppearances batStats)
                , fmap ("bat_totalBases" .=) (I.bat_totalBases batStats)
                , fmap ("bat_rbi" .=) (I.bat_rbi batStats)
                , fmap ("bat_leftOnBase" .=) (I.bat_leftOnBase batStats)
                , fmap ("bat_sacBunts" .=) (I.bat_sacBunts batStats)
                , fmap ("bat_sacFlies" .=) (I.bat_sacFlies batStats)
                , fmap ("bat_catchersInterference" .=) (I.bat_catchersInterference batStats)
                , fmap ("bat_pickoffs" .=) (I.bat_pickoffs batStats)
                ]

-- toJSON for ActiveRoster
instance ToJSON I.ActiveRoster where
    toJSON :: I.ActiveRoster -> Value
    toJSON (I.ActiveRoster people dataPulled checksum) =
        let playerPairs = [(K.fromText (T.pack (show playerId)), playerJSON) | player@(I.ActivePlayer playerId _ _ _ _ _ _ _ _) <- people, let playerJSON = toJSON player]
         in object ["officialPlayers" .= object playerPairs, "dataPulled" .= dataPulled, "checksum" .= checksum]

instance ToJSON I.ActivePlayer where
    toJSON :: I.ActivePlayer -> Value
    toJSON (I.ActivePlayer playerId useName useLastName nameSlug currentTeam primaryPosition batSide pitchHand active) =
        object
            [ "playerId" .= playerId
            , "useName" .= useName
            , "useLastName" .= useLastName
            , "nameSlug" .= nameSlug
            , "currentTeam" .= currentTeam -- Note: No nested object
            , "primaryPosition" .= primaryPosition
            , "batSide" .= batSide
            , "pitchHand" .= pitchHand
            , "active" .= active
            ]
-- End of /home/bismuth/git/pelotero-engine/src/ADT/Middle.hs

-- Start of /home/bismuth/git/pelotero-engine/src/ADT/Input.hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Input where

import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), Result (Success), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V

-- ## Boxscore Stats data type
data GameData where
    GameData :: {teams :: Teams} -> GameData
    deriving (Show, Eq)

data Teams where
    Teams :: {away :: TeamData, home :: TeamData} -> Teams
    deriving (Show, Eq)

data TeamData where
    TeamData :: {players :: M.Map Text Player} -> TeamData
    deriving (Show, Eq)

type Players = [(Text, Player)]

data Player where
    Player ::
        { person :: Person
        , gameid :: Maybe Int
        , parentTeamId :: Int
        , allPositions :: Maybe [Position]
        , status :: Status
        , stats :: PlayerStats
        } ->
        Player
    deriving (Show, Eq)

data Person where
    Person :: {personId :: Int, fullName :: Text} -> Person
    deriving (Show, Eq)

data Position where
    Position :: {pos_code :: Text} -> Position
    deriving (Show, Eq)

data Status where
    Status :: {status_code :: Text} -> Status
    deriving (Show, Eq)

data PlayerStats where
    PlayerStats ::
        { batting :: Maybe BattingStats
        , pitching :: Maybe PitchingStats
        } ->
        PlayerStats
    deriving (Show, Eq)

data BattingStats where
    BattingStats ::
        { bat_gamesPlayed :: Maybe Int
        , bat_flyOuts :: Maybe Int
        , bat_groundOuts :: Maybe Int
        , bat_runs :: Maybe Int
        , bat_doubles :: Maybe Int
        , bat_triples :: Maybe Int
        , bat_homeRuns :: Maybe Int
        , bat_strikeOuts :: Maybe Int
        , bat_baseOnBalls :: Maybe Int
        , bat_intentionalWalks :: Maybe Int
        , bat_hits :: Maybe Int
        , bat_hitByPitch :: Maybe Int
        , bat_atBats :: Maybe Int
        , bat_caughtStealing :: Maybe Int
        , bat_stolenBases :: Maybe Int
        , bat_groundIntoDoublePlay :: Maybe Int
        , bat_groundIntoTriplePlay :: Maybe Int
        , bat_plateAppearances :: Maybe Int
        , bat_totalBases :: Maybe Int
        , bat_rbi :: Maybe Int
        , bat_leftOnBase :: Maybe Int
        , bat_sacBunts :: Maybe Int
        , bat_sacFlies :: Maybe Int
        , bat_catchersInterference :: Maybe Int
        , bat_pickoffs :: Maybe Int
        } ->
        BattingStats
    deriving (Show, Eq)

data PitchingStats where
    PitchingStats ::
        { pit_gamesPlayed :: Maybe Int
        , pit_gamesStarted :: Maybe Int
        , pit_flyOuts :: Maybe Int
        , pit_groundOuts :: Maybe Int
        , pit_airOuts :: Maybe Int
        , pit_runs :: Maybe Int
        , pit_doubles :: Maybe Int
        , pit_triples :: Maybe Int
        , pit_homeRuns :: Maybe Int
        , pit_strikeOuts :: Maybe Int
        , pit_baseOnBalls :: Maybe Int
        , pit_intentionalWalks :: Maybe Int
        , pit_hits :: Maybe Int
        , pit_hitByPitch :: Maybe Int
        , pit_atBats :: Maybe Int
        , pit_caughtStealing :: Maybe Int
        , pit_stolenBases :: Maybe Int
        , pit_numberOfPitches :: Maybe Int
        , pit_inningsPitched :: Maybe Text
        , pit_wins :: Maybe Int
        , pit_losses :: Maybe Int
        , pit_saves :: Maybe Int
        , pit_saveOpportunities :: Maybe Int
        , pit_holds :: Maybe Int
        , pit_blownSaves :: Maybe Int
        , pit_earnedRuns :: Maybe Int
        , pit_battersFaced :: Maybe Int
        , pit_outs :: Maybe Int
        , pit_gamesPitched :: Maybe Int
        , pit_completeGames :: Maybe Int
        , pit_shutouts :: Maybe Int
        , pit_pitchesThrown :: Maybe Int
        , pit_balls :: Maybe Int
        , pit_strikes :: Maybe Int
        , pit_hitBatsmen :: Maybe Int
        , pit_balks :: Maybe Int
        , pit_wildPitches :: Maybe Int
        , pit_pickoffs :: Maybe Int
        , pit_rbi :: Maybe Int
        , pit_gamesFinished :: Maybe Int
        , pit_inheritedRunners :: Maybe Int
        , pit_inheritedRunnersScored :: Maybe Int
        , pit_catchersInterference :: Maybe Int
        , pit_sacBunts :: Maybe Int
        , pit_sacFlies :: Maybe Int
        , pit_passedBall :: Maybe Int
        } ->
        PitchingStats
    deriving (Show, Eq)

-- ## Schedule ADT's ##
data GameSchedule where
    GameSchedule :: {dates :: [DateEntry]} -> GameSchedule
    deriving (Show, Eq)

data DateEntry where
    DateEntry :: {games :: Maybe (V.Vector GameID)} -> DateEntry
    deriving (Show, Eq)

data GameID where
    GameID ::
        { gamePk :: Int
        , game_date :: Maybe Text
        } ->
        GameID
    deriving (Show, Eq)

-- Top level structure for the active roster
data ActiveRoster where
    ActiveRoster ::
        { people :: [ActivePlayer]
        , dataPulled :: Maybe Text
        , checksum :: Maybe Text
        } ->
        ActiveRoster

data ActivePlayer where
    ActivePlayer ::
        { playerId :: Int
        , useName :: Maybe Text
        , useLastName :: Maybe Text
        , nameSlug :: Maybe Text
        , currentTeam :: Maybe Int
        , primaryPosition :: Maybe Text
        , batSide :: Maybe Text
        , pitchHand :: Maybe Text
        , active :: Bool
        } ->
        ActivePlayer
    deriving (Show, Eq)

-- ## Game Status ADT's ##
data LiveGameStatus where
    LiveGameStatus :: {codedGameState :: Text} -> LiveGameStatus
    deriving (Show, Eq)

data LiveGameStatusWrapper where
    LiveGameStatusWrapper :: {gameStatus :: LiveGameStatus} -> LiveGameStatusWrapper
    deriving (Show, Eq)

data LiveGameWrapper where
    LiveGameWrapper :: {gameData :: LiveGameStatusWrapper} -> LiveGameWrapper
    deriving (Show, Eq)

-- ## JSON instances ##
instance FromJSON GameData where
    parseJSON :: Value -> Parser GameData
    parseJSON = withObject "GameData" $ \v ->
        GameData <$> v .: "teams"

instance FromJSON Teams where
    parseJSON :: Value -> Parser Teams
    parseJSON = withObject "Teams" $ \v ->
        Teams
            <$> v
                .: "away"
            <*> v
                .: "home"

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
        pure TeamData{players = validPlayers}

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
        let gameid = Nothing -- skips nonexistent (but necessary) gameId field, which is later added in Scraper.assignGameIdToPlayers
        return $ Player person gameid teamId validPositions status stats

instance FromJSON Person where
    parseJSON :: Value -> Parser Person
    parseJSON = withObject "Person" $ \v ->
        Person
            <$> v
                .: "id"
            <*> v
                .: "fullName"

instance FromJSON Position where
    parseJSON :: Value -> Parser Position
    parseJSON = withObject "Position" $ \v ->
        Position
            <$> v
                .: "code"

instance FromJSON Status where
    parseJSON :: Value -> Parser Status
    parseJSON = withObject "Status" $ \v ->
        Status
            <$> v
                .: "code"

instance FromJSON PlayerStats where
    parseJSON :: Value -> Parser PlayerStats
    parseJSON = withObject "PlayerStats" $ \v ->
        PlayerStats
            <$> v .:? "batting"
            <*> v .:? "pitching"

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

-- ## Schedule Instances
instance FromJSON GameSchedule where
    parseJSON :: Value -> Parser GameSchedule
    parseJSON = withObject "GameSchedule" $ \v ->
        GameSchedule
            <$> v .: "dates"

instance FromJSON DateEntry where
    parseJSON :: Value -> Parser DateEntry
    parseJSON = withObject "DateEntry" $ \v ->
        DateEntry
            <$> v .:? "games"

instance FromJSON GameID where
    parseJSON :: Value -> Parser GameID
    parseJSON = withObject "GameID" $ \v -> do
        gamePk <- v .: "gamePk"
        let game_date = Nothing
        return $ GameID gamePk game_date

-- ##Live Game Status instances##
instance FromJSON LiveGameStatus where
    parseJSON :: Value -> Parser LiveGameStatus
    parseJSON = withObject "LiveGameStatus" $ \v ->
        LiveGameStatus
            <$> v .: "codedGameState"

instance FromJSON LiveGameStatusWrapper where
    parseJSON :: Value -> Parser LiveGameStatusWrapper
    parseJSON = withObject "LiveGameStatusWrapper" $ \v ->
        LiveGameStatusWrapper
            <$> v .: "status"

instance FromJSON LiveGameWrapper where
    parseJSON :: Value -> Parser LiveGameWrapper
    parseJSON = withObject "LiveGameWrapper" $ \v ->
        LiveGameWrapper
            <$> v .: "gameData"

-- ##ROSTER instances##
instance FromJSON ActivePlayer where
    parseJSON :: Value -> Parser ActivePlayer
    parseJSON = withObject "ActivePlayer" $ \v -> do
        playerId <- v .: "id"
        useName <- v .:? "useName"
        useLastName <- v .:? "useLastName"
        nameSlug <- v .:? "nameSlug"
        currentTeam <- v .:? "currentTeam" >>= traverse (.: "id")
        primaryPosition <- v .:? "primaryPosition" >>= traverse (.: "code")
        batSide <- v .:? "batSide" >>= traverse (.: "code")
        pitchHand <- v .:? "pitchHand" >>= traverse (.: "code")
        active <- v .: "active"
        return $ ActivePlayer playerId useName useLastName nameSlug currentTeam primaryPosition batSide pitchHand active

instance FromJSON ActiveRoster where
    parseJSON :: Value -> Parser ActiveRoster
    parseJSON = withObject "ActiveRoster" $ \v -> do
        people <- v .: "people"
        dataPulled <- v .:? "dataPulled"
        checksum <- v .:? "checksum"
        return $ ActiveRoster people dataPulled checksum
-- End of /home/bismuth/git/pelotero-engine/src/ADT/Input.hs

-- Start of /home/bismuth/git/pelotero-engine/src/ADT/Stats.hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Stats where

import Control.Monad (filterM)
import Data.Aeson (
    FromJSON (..),
    Result (Success),
    Value,
    decode,
    eitherDecodeStrict,
    fromJSON,
    withObject,
    (.!=),
    (.:),
    (.:?),
 )

import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V

import qualified Middle as M

-- new types to prevent conflation of gameids and playerids
newtype GameID = GameID Int deriving (Show, Eq)
newtype GameIDstring = GameIDstring Text deriving (Show, Eq)

-- we have these related types defined elsewhere
-- newtype PlayerID = PlayerID Int deriving (Show, Eq)
-- newtype PlayerIDstring = PlayerIDstring T.Text deriving (Show, Eq)

data BattingStats where
    BattingStats ::
        { bat_gamesPlayed :: Maybe Int
        , bat_flyOuts :: Maybe Int
        , bat_groundOuts :: Maybe Int
        , bat_runs :: Maybe Int
        , bat_doubles :: Maybe Int
        , bat_triples :: Maybe Int
        , bat_homeRuns :: Maybe Int
        , bat_strikeOuts :: Maybe Int
        , bat_baseOnBalls :: Maybe Int
        , bat_intentionalWalks :: Maybe Int
        , bat_hits :: Maybe Int
        , bat_hitByPitch :: Maybe Int
        , bat_atBats :: Maybe Int
        , bat_caughtStealing :: Maybe Int
        , bat_stolenBases :: Maybe Int
        , bat_groundIntoDoublePlay :: Maybe Int
        , bat_groundIntoTriplePlay :: Maybe Int
        , bat_plateAppearances :: Maybe Int
        , bat_totalBases :: Maybe Int
        , bat_rbi :: Maybe Int
        , bat_leftOnBase :: Maybe Int
        , bat_sacBunts :: Maybe Int
        , bat_sacFlies :: Maybe Int
        , bat_catchersInterference :: Maybe Int
        , bat_pickoffs :: Maybe Int
        } ->
        BattingStats
    deriving (Show, Eq)

data PitchingStats where
    PitchingStats ::
        { pit_gamesPlayed :: Maybe Int
        , pit_gamesStarted :: Maybe Int
        , pit_flyOuts :: Maybe Int
        , pit_groundOuts :: Maybe Int
        , pit_airOuts :: Maybe Int
        , pit_runs :: Maybe Int
        , pit_doubles :: Maybe Int
        , pit_triples :: Maybe Int
        , pit_homeRuns :: Maybe Int
        , pit_strikeOuts :: Maybe Int
        , pit_baseOnBalls :: Maybe Int
        , pit_intentionalWalks :: Maybe Int
        , pit_hits :: Maybe Int
        , pit_hitByPitch :: Maybe Int
        , pit_atBats :: Maybe Int
        , pit_caughtStealing :: Maybe Int
        , pit_stolenBases :: Maybe Int
        , pit_numberOfPitches :: Maybe Int
        , pit_inningsPitched :: Maybe Text
        , pit_wins :: Maybe Int
        , pit_losses :: Maybe Int
        , pit_saves :: Maybe Int
        , pit_saveOpportunities :: Maybe Int
        , pit_holds :: Maybe Int
        , pit_blownSaves :: Maybe Int
        , pit_earnedRuns :: Maybe Int
        , pit_battersFaced :: Maybe Int
        , pit_outs :: Maybe Int
        , pit_gamesPitched :: Maybe Int
        , pit_completeGames :: Maybe Int
        , pit_shutouts :: Maybe Int
        , pit_pitchesThrown :: Maybe Int
        , pit_balls :: Maybe Int
        , pit_strikes :: Maybe Int
        , pit_hitBatsmen :: Maybe Int
        , pit_balks :: Maybe Int
        , pit_wildPitches :: Maybe Int
        , pit_pickoffs :: Maybe Int
        , pit_rbi :: Maybe Int
        , pit_gamesFinished :: Maybe Int
        , pit_inheritedRunners :: Maybe Int
        , pit_inheritedRunnersScored :: Maybe Int
        , pit_catchersInterference :: Maybe Int
        , pit_sacBunts :: Maybe Int
        , pit_sacFlies :: Maybe Int
        , pit_passedBall :: Maybe Int
        } ->
        PitchingStats
    deriving (Show, Eq)

instance FromJSON M.JsonPlayerData where
    parseJSON :: Value -> Parser M.JsonPlayerData
    parseJSON = withObject "JsonPlayerData" $ \v ->
        M.JsonPlayerData
            <$> v .: "player_id"
            <*> v .: "fullName"
            <*> v .: "stats"

instance FromJSON M.JsonStatsData where
    parseJSON :: Value -> Parser M.JsonStatsData
    parseJSON = withObject "JsonStatsData" $ \v ->
        M.JsonStatsData
            <$> v .: "parentTeamId"
            <*> v .: "allPositions"
            <*> v .: "status"
            <*> v .: "batting"
            <*> v .: "pitching"

instance FromJSON PitchingStats where
    parseJSON :: Value -> Parser PitchingStats
    parseJSON = withObject "PitchingStats" $ \v ->
        PitchingStats
            <$> v .:? "pit_gamesPlayed"
            <*> v .:? "pit_gamesStarted"
            <*> v .:? "pit_flyOuts"
            <*> v .:? "pit_groundOuts"
            <*> v .:? "pit_airOuts"
            <*> v .:? "pit_runs"
            <*> v .:? "pit_doubles"
            <*> v .:? "pit_triples"
            <*> v .:? "pit_homeRuns"
            <*> v .:? "pit_strikeOuts"
            <*> v .:? "pit_baseOnBalls"
            <*> v .:? "pit_intentionalWalks"
            <*> v .:? "pit_hits"
            <*> v .:? "pit_hitByPitch"
            <*> v .:? "pit_atBats"
            <*> v .:? "pit_caughtStealing"
            <*> v .:? "pit_stolenBases"
            <*> v .:? "pit_numberOfPitches"
            <*> v .:? "pit_inningsPitched"
            <*> v .:? "pit_wins"
            <*> v .:? "pit_losses"
            <*> v .:? "pit_saves"
            <*> v .:? "pit_saveOpportunities"
            <*> v .:? "pit_holds"
            <*> v .:? "pit_blownSaves"
            <*> v .:? "pit_earnedRuns"
            <*> v .:? "pit_battersFaced"
            <*> v .:? "pit_outs"
            <*> v .:? "pit_gamesPitched"
            <*> v .:? "pit_completeGames"
            <*> v .:? "pit_shutouts"
            <*> v .:? "pit_pitchesThrown"
            <*> v .:? "pit_balls"
            <*> v .:? "pit_strikes"
            <*> v .:? "pit_hitBatsmen"
            <*> v .:? "pit_balks"
            <*> v .:? "pit_wildPitches"
            <*> v .:? "pit_pickoffs"
            <*> v .:? "pit_rbi"
            <*> v .:? "pit_gamesFinished"
            <*> v .:? "pit_inheritedRunners"
            <*> v .:? "pit_inheritedRunnersScored"
            <*> v .:? "pit_catchersInterference"
            <*> v .:? "pit_sacBunts"
            <*> v .:? "pit_sacFlies"
            <*> v .:? "pit_passedBall"

instance FromJSON BattingStats where
    parseJSON = withObject "BattingStats" $ \v ->
        BattingStats
            <$> v .:? "bat_gamesPlayed"
            <*> v .:? "bat_flyOuts"
            <*> v .:? "bat_groundOuts"
            <*> v .:? "bat_runs"
            <*> v .:? "bat_doubles"
            <*> v .:? "bat_triples"
            <*> v .:? "bat_homeRuns"
            <*> v .:? "bat_strikeOuts"
            <*> v .:? "bat_baseOnBalls"
            <*> v .:? "bat_intentionalWalks"
            <*> v .:? "bat_hits"
            <*> v .:? "bat_hitByPitch"
            <*> v .:? "bat_atBats"
            <*> v .:? "bat_caughtStealing"
            <*> v .:? "bat_stolenBases"
            <*> v .:? "bat_groundIntoDoublePlay"
            <*> v .:? "bat_groundIntoTriplePlay"
            <*> v .:? "bat_plateAppearances"
            <*> v .:? "bat_totalBases"
            <*> v .:? "bat_rbi"
            <*> v .:? "bat_leftOnBase"
            <*> v .:? "bat_sacBunts"
            <*> v .:? "bat_sacFlies"
            <*> v .:? "bat_catchersInterference"
            <*> v .:? "bat_pickoffs"
-- End of /home/bismuth/git/pelotero-engine/src/ADT/Stats.hs

-- Start of /home/bismuth/git/pelotero-engine/src/Head2Head.hs
module Main (main) where

import qualified Config as C
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Middle as M
import PointCalc
import qualified Roster as R
import System.IO (hFlush, stdout)
import Validators

main :: IO ()
main = do
    putStrLn "Testing the Point Calculation Module:"

-- testSuite

-- testSuite :: IO ()
-- testSuite = do
--     config <- readJson "testFiles/prototype_config/config.json" :: IO (Either String C.Configuration)
--     roster <- readJson "testFiles/appData/rosters/team_002.json" :: IO (Either String R.LgManager)
--     playerData <- readJson "appData/stats/2023_09_30.json" :: IO (Either String [M.JsonPlayerData])

--     case (config, roster, playerData) of
--         (Right c, Right r, Right pd) -> do
--             let result = calculatePointsForPlayer c r pd
--             printResult result
--         _ -> putStrLn "Failed to read data."

printResult :: [(Text, Either Text Double)] -> IO ()
printResult [] = return ()
printResult ((playerId, result) : xs) = do
    putStr $ T.unpack playerId ++ ": "
    case result of
        Left error -> putStrLn $ "Error: " ++ T.unpack error
        Right points -> putStrLn $ "Points: " ++ show points
    printResult xs
-- End of /home/bismuth/git/pelotero-engine/src/Head2Head.hs

-- Start of /home/bismuth/git/pelotero-engine/src/TypeGraph.hs
{-# LANGUAGE OverloadedStrings #-}

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy.IO as TLIO
import System.Process (callCommand)

main :: IO ()
main = do
    writeConfigurationGraph
    putStrLn "Configuration graph written to configurationGraph.dot"
    -- Convert .dot to .png using system call
    let dotCommand = "dot -Tpng configurationGraph.dot -o configurationGraph.png"
    callCommand dotCommand
    putStrLn "Configuration graph image written to configurationGraph.png"

-- writeConfigurationGraph :: IO ()
-- writeConfigurationGraph = do
--     let dotGraph = digraph (Str "ConfigurationGraph") $ do
--           -- Nodes with Labels
--           node (pack "Configuration") [Label $ StrLabel "Configuration\nstatus, leagueID, commissioner, teamId"]
--           node (pack "PointParameters") [Label $ StrLabel "PointParameters\nlg_style, start_UTC, end_UTC"]
--           node (pack "BattingMults") [Label $ StrLabel "BattingMults\nlgb_single, lgb_double, ..."]
--           node (pack "PitchingMults") [Label $ StrLabel "PitchingMults\nlgp_win, lgp_save, ..."]
--           node (pack "LgLineupLmts") [Label $ StrLabel "LgLineupLmts\nlg_catcher, lg_first, ..."]
--           node (pack "DraftParameters") [Label $ StrLabel "DraftParameters\nautoDraft, autoDraft_UTC"]
--           node (pack "DraftRosterLmts") [Label $ StrLabel "DraftRosterLmts\ndr_catcher, dr_first, ..."]

--           -- Edges
--           edge (pack "Configuration") (pack "PointParameters") []
--           edge (pack "Configuration") (pack "DraftParameters") []
--           edge (pack "PointParameters") (pack "BattingMults") []
--           edge (pack "PointParameters") (pack "PitchingMults") []
--           edge (pack "PointParameters") (pack "LgLineupLmts") []
--           edge (pack "DraftParameters") (pack "DraftRosterLmts") []

--     TLIO.writeFile "configurationGraph.dot" (printDotGraph dotGraph)

writeConfigurationGraph :: IO ()
writeConfigurationGraph = do
    let dotGraph = digraph (Str "ConfigurationGraph") $ do
          node (pack "Configuration") [Label $ StrLabel "Configuration\nstatus, leagueID, commissioner, teamId"]
          node (pack "PointParameters") [Label $ StrLabel "PointParameters\nlg_style, start_UTC, end_UTC, lg_battingMults, lg_pitchingMults, lineup_limits"]
          node (pack "DraftParameters") [Label $ StrLabel "DraftParameters\nautoDraft, autoDraft_UTC, draft_limits"]
          node (pack "BattingMults") [Label $ StrLabel "BattingMults\nlgb_single, lgb_double, lgb_triple, lgb_homerun, lgb_rbi, lgb_run, lgb_base_on_balls, lgb_stolen_base, lgb_hit_by_pitch, lgb_strikeout, lgb_caught_stealing"]
          node (pack "PitchingMults") [Label $ StrLabel "PitchingMults\nlgp_win, lgp_save, lgp_quality_start, lgp_inning_pitched, lgp_strikeout, lgp_complete_game, lgp_shutout, lgp_base_on_balls, lgp_hits_allowed, lgp_earned_runs, lgp_hit_batsman, lgp_loss"]
          node (pack "LgLineupLmts") [Label $ StrLabel "LgLineupLmts\nlg_catcher, lg_first, lg_second, lg_third, lg_shortstop, lg_outfield, lg_utility, lg_s_pitcher, lg_r_pitcher, lg_max_size"]
          node (pack "DraftRosterLmts") [Label $ StrLabel "DraftRosterLmts\ndr_catcher, dr_first, dr_second, dr_third, dr_shortstop, dr_outfield, dr_utility, dr_s_pitcher, dr_r_pitcher"]

          edge (pack "Configuration") (pack "PointParameters") []
          edge (pack "Configuration") (pack "DraftParameters") []
          edge (pack "PointParameters") (pack "BattingMults") []
          edge (pack "PointParameters") (pack "PitchingMults") []
          edge (pack "PointParameters") (pack "LgLineupLmts") []
          edge (pack "DraftParameters") (pack "DraftRosterLmts") []

    TLIO.writeFile "configurationGraph.dot" (printDotGraph dotGraph)-- End of /home/bismuth/git/pelotero-engine/src/TypeGraph.hs

-- Start of /home/bismuth/git/pelotero-engine/app/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO as TIO
import qualified Pelotero.Prelude as P

main :: IO ()
main = do
  TIO.putStrLn (P.appName <> " " <> P.appVersion)
  TIO.putStrLn "Phase 0 skeleton. Subcommands TBD."-- End of /home/bismuth/git/pelotero-engine/app/Main.hs

-- Start of /home/bismuth/git/pelotero-engine/app/FetchRoster.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Crypto.Hash (SHA256(..), hashWith)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)

import DB.Connection (defaultConfig, initializeDB)
import DB.Migration (runMigrations)
import DB.Player (upsertPlayers, logFetch, getLastFetch, getAllPlayers)
import MLB.Client (fetchActiveRoster, MLBError(..))
import Types.Player (Player, PlayerFetchRecord(..))

main :: IO ()
main = do
  args <- getArgs
  season <- case args of
    (s:_) -> case readMaybe s of
      Just yr -> pure yr
      Nothing -> do
        hPutStrLn stderr "Usage: fetch-rosters <season-year>"
        exitFailure
    _ -> do
      hPutStrLn stderr "Usage: fetch-rosters <season-year>"
      exitFailure

  -- DB setup
  config <- defaultConfig
  pool <- initializeDB config
  runMigrations pool

  -- Check if we already fetched recently
  lastFetch <- getLastFetch pool season
  case lastFetch of
    Just record -> do
      hPutStrLn stderr $ "Last fetch for season " ++ show season
        ++ ": " ++ show (playerCount record) ++ " players"
        ++ " at " ++ show (fetchedAt record)
      hPutStrLn stderr "Proceeding with fresh fetch..."
    Nothing ->
      hPutStrLn stderr $ "No previous fetch for season " ++ show season

  -- Fetch from MLB
  result <- fetchActiveRoster season
  case result of
    Left err -> do
      hPutStrLn stderr $ "MLB API error: " ++ show err
      exitFailure
    Right players -> do
      hPutStrLn stderr $ "Got " ++ show (length players) ++ " players from MLB"

      -- Compute checksum of the response
      let checksum = computeChecksum players

      -- Check if data actually changed
      let unchanged = case lastFetch of
            Just prev -> fetchChecksum prev == checksum
            Nothing   -> False

      if unchanged
        then
          hPutStrLn stderr "Data unchanged since last fetch, skipping upsert."
        else do
          -- Upsert all players
          upsertPlayers pool players
          hPutStrLn stderr $ "Upserted " ++ show (length players) ++ " players"

      -- Log the fetch either way
      logFetch pool season checksum (length players)
      hPutStrLn stderr "Fetch logged. Done."

computeChecksum :: [Player] -> Text
computeChecksum players =
  let bs = BL.toStrict $ encode players
      hash = hashWith SHA256 bs
  in T.pack (show hash)-- End of /home/bismuth/git/pelotero-engine/app/FetchRoster.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/Prelude.hs
{-# LANGUAGE OverloadedStrings #-}

module Pelotero.Prelude
  ( appName
  , appVersion
  ) where

import Data.Text (Text)

appName :: Text
appName = "pelotero-engine"

appVersion :: Text
appVersion = "0.0.10.0"
-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/Prelude.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/MLB/Convert.hs
-- | Wire-to-domain conversion for MLB API responses. The wire types
-- ("Pelotero.MLB.Wire.*") parse the upstream JSON; this module turns them
-- into the cleaner domain types ("Pelotero.Domain.*"), discarding fields we
-- don't model and validating identifiers.
--
-- Conversion is lenient: when the upstream payload is malformed (e.g. an
-- unknown position code, a player ID of zero), we emit a 'ConvertWarning'
-- and substitute a sensible default. The caller decides whether to log,
-- ignore, or escalate. In production we'll wire warnings into katip
-- (Phase 3); for now they're plain values.
--
-- Why not fail-fast? The MLB feed ships partial records constantly —
-- spring-training rosters with no team, two-way players coded as "TWP",
-- pitcher batting lines from rare AL pitcher PA's. Failing the whole sync
-- because one record is shaped oddly would be operationally hostile.
module Pelotero.MLB.Convert
  ( -- * Conversion
    convertPlayer
  , convertPlayers
  , convertSchedule
  , convertBoxscore
    -- * Reporting
  , ConvertWarning(..)
  , renderWarning
  , logWarnings
    -- * Re-exports for convenience
  , BoxscoreEntry(..)
  ) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import System.IO (Handle, hPutStrLn, stderr)
import qualified Data.Text as T
import System.IO (Handle, hPutStrLn, stderr)

import Pelotero.Domain.Game
  ( Game(..)
  , GameSchedule(..)
  )
import Pelotero.Domain.Id
  ( GameId(..)
  , PlayerId(..)
  , TeamId(..)
  )
import Pelotero.Domain.Player
  ( Handedness
  , Player(..)
  , parseHandedness
  )
import Pelotero.Domain.Position (Position, parsePosition)
import Pelotero.Domain.Stats
  ( BattingStats(..)
  , PitchingStats(..)
  , emptyBatting
  , emptyPitching
  )
import qualified Pelotero.MLB.Wire.Boxscore as WB
import qualified Pelotero.MLB.Wire.Player as WP
import qualified Pelotero.MLB.Wire.Schedule as WS

--------------------------------------------------------------------------------
-- Warnings

-- | Things the converter encountered that weren't fatal but the operator
-- probably wants to know about. Designed to be cheap to construct and trivial
-- to render; we'll attach more structured context (trace IDs, request IDs) in
-- Phase 3 once the effects layer is in place.
data ConvertWarning
  = -- | Player record had an ID of @0@ or negative, which MLB never legitimately
    -- emits. The whole record is dropped.
    InvalidPlayerId Int
  | -- | Position code didn't match any of the ten we recognise. The player is
    -- still kept, but with @playerPosition = Nothing@.
    UnknownPosition !Int !Text
  | -- | Bat/pitch handedness wasn't \"L\"/\"R\"/\"S\". Kept with 'Nothing'.
    UnknownHandedness !Int !Text
  | -- | Schedule entry had an unparseable date. Whole entry is dropped.
    InvalidGameDate !Int !Text
  | -- | Schedule entry was missing a team reference. Whole game is dropped.
    MissingTeamRef !Int
  deriving stock (Show, Eq)

-- | Single-line, human-readable rendering for log destinations.
renderWarning :: ConvertWarning -> Text
renderWarning = \case
  InvalidPlayerId pid ->
    "convert: dropped player with invalid id " <> tshow pid
  UnknownPosition pid code ->
    "convert: player " <> tshow pid
      <> " has unknown position code " <> T.pack (show code)
      <> "; setting Nothing"
  UnknownHandedness pid code ->
    "convert: player " <> tshow pid
      <> " has unknown hand code " <> T.pack (show code)
      <> "; setting Nothing"
  InvalidGameDate gid raw ->
    "convert: dropped game " <> tshow gid
      <> " with unparseable date " <> T.pack (show raw)
  MissingTeamRef gid ->
    "convert: dropped game " <> tshow gid <> " missing team reference"

-- | Default sink: render each warning to stderr. Returns immediately if the
-- list is empty so it's safe to call unconditionally.
logWarnings :: [ConvertWarning] -> IO ()
logWarnings = logWarningsTo stderr

-- | Variant that targets an arbitrary 'Handle'. Used by the test suite to
-- capture warnings into a buffer.
logWarningsTo :: Handle -> [ConvertWarning] -> IO ()
logWarningsTo h ws = unless (null ws) $
  mapM_ (\w -> hPutStrLn h (T.unpack (renderWarning w))) ws

--------------------------------------------------------------------------------
-- Players

-- | Convert one wire player. Returns 'Nothing' for records we refuse to admit
-- (currently: @id <= 0@). Warnings collected via the @Writer@-shaped tuple.
convertPlayer :: WP.WirePlayer -> ([ConvertWarning], Maybe Player)
convertPlayer wp
  | WP.wpId wp <= 0 =
      ([InvalidPlayerId (WP.wpId wp)], Nothing)
  | otherwise =
      let pid = WP.wpId wp

          (posWarn, position) = convertPosition pid (WP.wpPrimaryPosition wp)
          (batWarn, batSide)  = convertHand pid (WP.wpBatSide wp)
          (pitWarn, pitchHnd) = convertHand pid (WP.wpPitchHand wp)

          player = Player
            { playerId        = PlayerId pid
            , playerFirstName = orEmpty (WP.wpUseName wp)
            , playerLastName  = orEmpty (WP.wpUseLastName wp)
            , playerNameSlug  = orEmpty (WP.wpNameSlug wp)
            , playerTeamId    = TeamId . WP.wtrId <$> WP.wpCurrentTeam wp
            , playerPosition  = position
            , playerBatSide   = batSide
            , playerPitchHand = pitchHnd
            , playerActive    = WP.wpActive wp
            }
      in (posWarn <> batWarn <> pitWarn, Just player)

-- | Convert a roster envelope. Warnings from each record are concatenated;
-- order is preserved so callers can correlate by position in the output list.
convertPlayers :: WP.WirePlayerEnvelope -> ([ConvertWarning], [Player])
convertPlayers env =
  let results = map convertPlayer (WP.wirePlayers env)
      warns   = concatMap fst results
      players = mapMaybe snd results
  in (warns, players)

convertPosition
  :: Int
  -> Maybe WP.WirePositionRef
  -> ([ConvertWarning], Maybe Position)
convertPosition _   Nothing  = ([], Nothing)
convertPosition pid (Just r) =
  -- Prefer the abbreviation when present (it's the scorer form, "1B"/"DH"),
  -- fall back to the numeric code, and finally give up with a warning.
  case (WP.wprAbbreviation r, WP.wprCode r) of
    (Just abbr, _) | Just p <- parsePosition abbr -> ([], Just p)
    (_, Just code) | Just p <- parsePosition code -> ([], Just p)
    (Just abbr, _) -> ([UnknownPosition pid abbr], Nothing)
    (_, Just code) -> ([UnknownPosition pid code], Nothing)
    _              -> ([], Nothing)

convertHand
  :: Int
  -> Maybe WP.WireHandRef
  -> ([ConvertWarning], Maybe Handedness)
convertHand _   Nothing                            = ([], Nothing)
convertHand _   (Just (WP.WireHandRef Nothing))    = ([], Nothing)
convertHand pid (Just (WP.WireHandRef (Just code))) =
  case parseHandedness code of
    Just h  -> ([], Just h)
    Nothing -> ([UnknownHandedness pid code], Nothing)

--------------------------------------------------------------------------------
-- Schedule

-- | Flatten a wire schedule envelope into a domain 'GameSchedule'. Per-game
-- failures (bad date, missing team) are reported as warnings and the game is
-- dropped; we never raise an exception.
convertSchedule :: WS.WireScheduleEnvelope -> ([ConvertWarning], GameSchedule)
convertSchedule env =
  let (warns, games) = foldr step ([], []) (WS.wseDates env)
  in (warns, GameSchedule games)
  where
    step entry (ws, gs) =
      let (newWs, newGs) = convertDateEntry entry
      in (newWs <> ws, newGs <> gs)

convertDateEntry :: WS.WireDateEntry -> ([ConvertWarning], [Game])
convertDateEntry de =
  case parseDate (WS.wdeDate de) of
    Nothing  -> ([], [])  -- empty/bad dates with no games aren't worth warning about
    Just day ->
      let games = maybe [] id (WS.wdeGames de)
          results = map (convertGame day) games
      in (concatMap fst results, mapMaybe snd results)

convertGame :: Day -> WS.WireGame -> ([ConvertWarning], Maybe Game)
convertGame day wg =
  let gid = WS.wgGamePk wg
      teams = WS.wgTeams wg
      maway = teams >>= WS.wgtAway >>= WS.wgtTeamId
      mhome = teams >>= WS.wgtHome >>= WS.wgtTeamId
  in case (maway, mhome) of
       (Just a, Just h) ->
         ( []
         , Just Game
             { gameId       = GameId gid
             , gameDate     = day
             , gameAwayTeam = TeamId a
             , gameHomeTeam = TeamId h
             }
         )
       _ -> ([MissingTeamRef gid], Nothing)

parseDate :: Text -> Maybe Day
parseDate t = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (T.unpack t)

--------------------------------------------------------------------------------
-- Boxscore

-- | A boxscore yields *many* per-player entries; one per appearance per side.
-- We attach the team's MLB id so callers don't have to thread it back through
-- the structure.
data BoxscoreEntry = BoxscoreEntry
  { boxPlayerId :: PlayerId
  , boxTeamId   :: Maybe TeamId
  , boxBatting  :: BattingStats
  , boxPitching :: PitchingStats
  }
  deriving stock (Show, Eq)

-- | Convert a full boxscore. Currently emits no warnings — the wire format
-- is permissive enough that we can fill in missing pieces with empty stats.
-- We'll add tracing in Phase 3.
convertBoxscore :: WB.WireBoxscore -> ([ConvertWarning], [BoxscoreEntry])
convertBoxscore bs =
  let teams = WB.wbsTeams bs
      away  = boxsideEntries (WB.wbtAway teams)
      home  = boxsideEntries (WB.wbtHome teams)
  in ([], away <> home)

boxsideEntries :: WB.WireBoxTeam -> [BoxscoreEntry]
boxsideEntries side =
  map snd (Map.toList (Map.mapMaybeWithKey toEntry (WB.wbtPlayers side)))
  where
    toEntry _key wp = Just BoxscoreEntry
      { boxPlayerId = PlayerId (WB.wbpPersonId (WB.wbpPerson wp))
      , boxTeamId   = TeamId <$> WB.wbpParentTeamId wp
      , boxBatting  = maybe emptyBatting convertBatting
                        (WB.wbsBatting =<< WB.wbpStats wp)
      , boxPitching = maybe emptyPitching convertPitching
                        (WB.wbsPitching =<< WB.wbpStats wp)
      }

convertBatting :: WB.WireBoxBatting -> BattingStats
convertBatting WB.WireBoxBatting{..} = BattingStats
  { batGamesPlayed          = wbbGamesPlayed
  , batPlateAppearances     = wbbPlateAppearances
  , batAtBats               = wbbAtBats
  , batRuns                 = wbbRuns
  , batHits                 = wbbHits
  , batDoubles              = wbbDoubles
  , batTriples              = wbbTriples
  , batHomeRuns             = wbbHomeRuns
  , batRbi                  = wbbRbi
  , batBaseOnBalls          = wbbBaseOnBalls
  , batIntentionalWalks     = wbbIntentionalWalks
  , batStrikeOuts           = wbbStrikeOuts
  , batStolenBases          = wbbStolenBases
  , batCaughtStealing       = wbbCaughtStealing
  , batHitByPitch           = wbbHitByPitch
  , batSacBunts             = wbbSacBunts
  , batSacFlies             = wbbSacFlies
  , batGroundIntoDoublePlay = wbbGroundIntoDoublePlay
  , batGroundIntoTriplePlay = wbbGroundIntoTriplePlay
  , batLeftOnBase           = wbbLeftOnBase
  , batTotalBases           = wbbTotalBases
  , batFlyOuts              = wbbFlyOuts
  , batGroundOuts           = wbbGroundOuts
  , batCatchersInterference = wbbCatchersInterference
  , batPickoffs             = wbbPickoffs
  }

convertPitching :: WB.WireBoxPitching -> PitchingStats
convertPitching WB.WireBoxPitching{..} = PitchingStats
  { pitGamesPlayed             = wbpGamesPlayed
  , pitGamesStarted            = wbpGamesStarted
  , pitGamesFinished           = wbpGamesFinished
  , pitCompleteGames           = wbpCompleteGames
  , pitShutouts                = wbpShutouts
  , pitWins                    = wbpWins
  , pitLosses                  = wbpLosses
  , pitSaves                   = wbpSaves
  , pitSaveOpportunities       = wbpSaveOpportunities
  , pitHolds                   = wbpHolds
  , pitBlownSaves              = wbpBlownSaves
  , pitInningsPitched          = wbpInningsPitched
  , pitOuts                    = wbpOuts
  , pitBattersFaced            = wbpBattersFaced
  , pitNumberOfPitches         = wbpNumberOfPitches
  , pitStrikes                 = wbpStrikes
  , pitBalls                   = wbpBalls
  , pitHits                    = wbpHits
  , pitDoubles                 = wbpDoubles
  , pitTriples                 = wbpTriples
  , pitHomeRuns                = wbpHomeRuns
  , pitRuns                    = wbpRuns
  , pitEarnedRuns              = wbpEarnedRuns
  , pitStrikeOuts              = wbpStrikeOuts
  , pitBaseOnBalls             = wbpBaseOnBalls
  , pitIntentionalWalks        = wbpIntentionalWalks
  , pitHitBatsmen              = wbpHitBatsmen
  , pitWildPitches             = wbpWildPitches
  , pitBalks                   = wbpBalks
  , pitPickoffs                = wbpPickoffs
  , pitFlyOuts                 = wbpFlyOuts
  , pitGroundOuts              = wbpGroundOuts
  , pitAirOuts                 = wbpAirOuts
  , pitInheritedRunners        = wbpInheritedRunners
  , pitInheritedRunnersScored  = wbpInheritedRunnersScored
  , pitStolenBases             = wbpStolenBases
  , pitCaughtStealing          = wbpCaughtStealing
  , pitAtBats                  = wbpAtBats
  , pitRbi                     = wbpRbi
  , pitSacBunts                = wbpSacBunts
  , pitSacFlies                = wbpSacFlies
  , pitCatchersInterference    = wbpCatchersInterference
  , pitPassedBall              = wbpPassedBall
  }

--------------------------------------------------------------------------------
-- Internal helpers

orEmpty :: Maybe Text -> Text
orEmpty = maybe T.empty id

tshow :: Show a => a -> Text
tshow = T.pack . show-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/MLB/Convert.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/MLB/Wire/Boxscore.hs
-- | Wire shape for @\/api\/v1\/game\/{id}\/boxscore@. We map the deeply-
-- nested JSON onto a flatter structure here; conversion to per-player domain
-- 'Pelotero.Domain.Stats' happens in "Pelotero.MLB.Convert".
module Pelotero.MLB.Wire.Boxscore
  ( WireBoxscore(..)
  , WireBoxTeams(..)
  , WireBoxTeam(..)
  , WireBoxPlayer(..)
  , WireBoxPerson(..)
  , WireBoxStats(..)
  , WireBoxBatting(..)
  , WireBoxPitching(..)
  ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import Data.Map.Strict (Map)
import Data.Text (Text)

newtype WireBoxscore = WireBoxscore
  { wbsTeams :: WireBoxTeams }
  deriving stock (Show, Eq)

instance FromJSON WireBoxscore where
  parseJSON = withObject "WireBoxscore" $ \o ->
    WireBoxscore <$> o .: "teams"

data WireBoxTeams = WireBoxTeams
  { wbtAway :: WireBoxTeam
  , wbtHome :: WireBoxTeam
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxTeams where
  parseJSON = withObject "WireBoxTeams" $ \o -> WireBoxTeams
    <$> o .: "away"
    <*> o .: "home"

-- | A team's slice of the box score. The @players@ map is keyed by
-- @"ID<playerId>"@ in the wire format (e.g. "ID660271"). We strip that prefix
-- in the converter.
newtype WireBoxTeam = WireBoxTeam
  { wbtPlayers :: Map Text WireBoxPlayer }
  deriving stock (Show, Eq)

instance FromJSON WireBoxTeam where
  parseJSON = withObject "WireBoxTeam" $ \o ->
    WireBoxTeam <$> o .: "players"

data WireBoxPlayer = WireBoxPlayer
  { wbpPerson       :: WireBoxPerson
  , wbpParentTeamId :: Maybe Int
  , wbpStats        :: Maybe WireBoxStats
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxPlayer where
  parseJSON = withObject "WireBoxPlayer" $ \o -> WireBoxPlayer
    <$> o .:  "person"
    <*> o .:? "parentTeamId"
    <*> o .:? "stats"

data WireBoxPerson = WireBoxPerson
  { wbpPersonId :: Int
  , wbpFullName :: Maybe Text
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxPerson where
  parseJSON = withObject "WireBoxPerson" $ \o -> WireBoxPerson
    <$> o .:  "id"
    <*> o .:? "fullName"

data WireBoxStats = WireBoxStats
  { wbsBatting  :: Maybe WireBoxBatting
  , wbsPitching :: Maybe WireBoxPitching
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxStats where
  parseJSON = withObject "WireBoxStats" $ \o -> WireBoxStats
    <$> o .:? "batting"
    <*> o .:? "pitching"

-- | Raw batting stats from the boxscore. Every field is 'Maybe Int'; the
-- domain type ('Pelotero.Domain.Stats.BattingStats') has the same shape, so
-- conversion is mostly mechanical.
data WireBoxBatting = WireBoxBatting
  { wbbGamesPlayed          :: Maybe Int
  , wbbPlateAppearances     :: Maybe Int
  , wbbAtBats               :: Maybe Int
  , wbbRuns                 :: Maybe Int
  , wbbHits                 :: Maybe Int
  , wbbDoubles              :: Maybe Int
  , wbbTriples              :: Maybe Int
  , wbbHomeRuns             :: Maybe Int
  , wbbRbi                  :: Maybe Int
  , wbbBaseOnBalls          :: Maybe Int
  , wbbIntentionalWalks     :: Maybe Int
  , wbbStrikeOuts           :: Maybe Int
  , wbbStolenBases          :: Maybe Int
  , wbbCaughtStealing       :: Maybe Int
  , wbbHitByPitch           :: Maybe Int
  , wbbSacBunts             :: Maybe Int
  , wbbSacFlies             :: Maybe Int
  , wbbGroundIntoDoublePlay :: Maybe Int
  , wbbGroundIntoTriplePlay :: Maybe Int
  , wbbLeftOnBase           :: Maybe Int
  , wbbTotalBases           :: Maybe Int
  , wbbFlyOuts              :: Maybe Int
  , wbbGroundOuts           :: Maybe Int
  , wbbCatchersInterference :: Maybe Int
  , wbbPickoffs             :: Maybe Int
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxBatting where
  parseJSON = withObject "WireBoxBatting" $ \o -> WireBoxBatting
    <$> o .:? "gamesPlayed"
    <*> o .:? "plateAppearances"
    <*> o .:? "atBats"
    <*> o .:? "runs"
    <*> o .:? "hits"
    <*> o .:? "doubles"
    <*> o .:? "triples"
    <*> o .:? "homeRuns"
    <*> o .:? "rbi"
    <*> o .:? "baseOnBalls"
    <*> o .:? "intentionalWalks"
    <*> o .:? "strikeOuts"
    <*> o .:? "stolenBases"
    <*> o .:? "caughtStealing"
    <*> o .:? "hitByPitch"
    <*> o .:? "sacBunts"
    <*> o .:? "sacFlies"
    <*> o .:? "groundIntoDoublePlay"
    <*> o .:? "groundIntoTriplePlay"
    <*> o .:? "leftOnBase"
    <*> o .:? "totalBases"
    <*> o .:? "flyOuts"
    <*> o .:? "groundOuts"
    <*> o .:? "catchersInterference"
    <*> o .:? "pickoffs"

data WireBoxPitching = WireBoxPitching
  { wbpGamesPlayed             :: Maybe Int
  , wbpGamesStarted            :: Maybe Int
  , wbpGamesFinished           :: Maybe Int
  , wbpCompleteGames           :: Maybe Int
  , wbpShutouts                :: Maybe Int
  , wbpWins                    :: Maybe Int
  , wbpLosses                  :: Maybe Int
  , wbpSaves                   :: Maybe Int
  , wbpSaveOpportunities       :: Maybe Int
  , wbpHolds                   :: Maybe Int
  , wbpBlownSaves              :: Maybe Int
  , wbpInningsPitched          :: Maybe Text
  , wbpOuts                    :: Maybe Int
  , wbpBattersFaced            :: Maybe Int
  , wbpNumberOfPitches         :: Maybe Int
  , wbpStrikes                 :: Maybe Int
  , wbpBalls                   :: Maybe Int
  , wbpHits                    :: Maybe Int
  , wbpDoubles                 :: Maybe Int
  , wbpTriples                 :: Maybe Int
  , wbpHomeRuns                :: Maybe Int
  , wbpRuns                    :: Maybe Int
  , wbpEarnedRuns              :: Maybe Int
  , wbpStrikeOuts              :: Maybe Int
  , wbpBaseOnBalls             :: Maybe Int
  , wbpIntentionalWalks        :: Maybe Int
  , wbpHitBatsmen              :: Maybe Int
  , wbpWildPitches             :: Maybe Int
  , wbpBalks                   :: Maybe Int
  , wbpPickoffs                :: Maybe Int
  , wbpFlyOuts                 :: Maybe Int
  , wbpGroundOuts              :: Maybe Int
  , wbpAirOuts                 :: Maybe Int
  , wbpInheritedRunners        :: Maybe Int
  , wbpInheritedRunnersScored  :: Maybe Int
  , wbpStolenBases             :: Maybe Int
  , wbpCaughtStealing          :: Maybe Int
  , wbpAtBats                  :: Maybe Int
  , wbpRbi                     :: Maybe Int
  , wbpSacBunts                :: Maybe Int
  , wbpSacFlies                :: Maybe Int
  , wbpCatchersInterference    :: Maybe Int
  , wbpPassedBall              :: Maybe Int
  }
  deriving stock (Show, Eq)

instance FromJSON WireBoxPitching where
  parseJSON = withObject "WireBoxPitching" $ \o -> WireBoxPitching
    <$> o .:? "gamesPlayed"
    <*> o .:? "gamesStarted"
    <*> o .:? "gamesFinished"
    <*> o .:? "completeGames"
    <*> o .:? "shutouts"
    <*> o .:? "wins"
    <*> o .:? "losses"
    <*> o .:? "saves"
    <*> o .:? "saveOpportunities"
    <*> o .:? "holds"
    <*> o .:? "blownSaves"
    <*> o .:? "inningsPitched"
    <*> o .:? "outs"
    <*> o .:? "battersFaced"
    <*> o .:? "numberOfPitches"
    <*> o .:? "strikes"
    <*> o .:? "balls"
    <*> o .:? "hits"
    <*> o .:? "doubles"
    <*> o .:? "triples"
    <*> o .:? "homeRuns"
    <*> o .:? "runs"
    <*> o .:? "earnedRuns"
    <*> o .:? "strikeOuts"
    <*> o .:? "baseOnBalls"
    <*> o .:? "intentionalWalks"
    <*> o .:? "hitBatsmen"
    <*> o .:? "wildPitches"
    <*> o .:? "balks"
    <*> o .:? "pickoffs"
    <*> o .:? "flyOuts"
    <*> o .:? "groundOuts"
    <*> o .:? "airOuts"
    <*> o .:? "inheritedRunners"
    <*> o .:? "inheritedRunnersScored"
    <*> o .:? "stolenBases"
    <*> o .:? "caughtStealing"
    <*> o .:? "atBats"
    <*> o .:? "rbi"
    <*> o .:? "sacBunts"
    <*> o .:? "sacFlies"
    <*> o .:? "catchersInterference"
    <*> o .:? "passedBall"-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/MLB/Wire/Boxscore.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/MLB/Wire/Player.hs
-- | The on-the-wire shape of MLB's @\/api\/v1\/sports\/1\/players@ endpoint.
-- These types exist solely to parse the JSON; they're never used for business
-- logic. Conversion to "Pelotero.Domain.Player" happens in
-- "Pelotero.MLB.Convert".
module Pelotero.MLB.Wire.Player
  ( WirePlayerEnvelope(..)
  , WirePlayer(..)
  , WireTeamRef(..)
  , WirePositionRef(..)
  , WireHandRef(..)
  ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import Data.Text (Text)

-- | Top-level response: @{ "people": [ ... ] }@.
newtype WirePlayerEnvelope = WirePlayerEnvelope
  { wirePlayers :: [WirePlayer] }
  deriving stock (Show, Eq)

instance FromJSON WirePlayerEnvelope where
  parseJSON = withObject "WirePlayerEnvelope" $ \o ->
    WirePlayerEnvelope <$> o .: "people"

-- | A single player record from the roster feed. Every field except @id@ and
-- @active@ is optional in practice — MLB ships partial records during early
-- spring training.
data WirePlayer = WirePlayer
  { wpId              :: Int
  , wpUseName         :: Maybe Text
  , wpUseLastName     :: Maybe Text
  , wpNameSlug        :: Maybe Text
  , wpCurrentTeam     :: Maybe WireTeamRef
  , wpPrimaryPosition :: Maybe WirePositionRef
  , wpBatSide         :: Maybe WireHandRef
  , wpPitchHand       :: Maybe WireHandRef
  , wpActive          :: Bool
  }
  deriving stock (Show, Eq)

instance FromJSON WirePlayer where
  parseJSON = withObject "WirePlayer" $ \o -> WirePlayer
    <$> o .:  "id"
    <*> o .:? "useName"
    <*> o .:? "useLastName"
    <*> o .:? "nameSlug"
    <*> o .:? "currentTeam"
    <*> o .:? "primaryPosition"
    <*> o .:? "batSide"
    <*> o .:? "pitchHand"
    <*> o .:  "active"

-- | Embedded team reference: @{ "id": 117, "name": "Houston Astros", ... }@.
data WireTeamRef = WireTeamRef
  { wtrId   :: Int
  , wtrName :: Maybe Text
  }
  deriving stock (Show, Eq)

instance FromJSON WireTeamRef where
  parseJSON = withObject "WireTeamRef" $ \o -> WireTeamRef
    <$> o .:  "id"
    <*> o .:? "name"

-- | Embedded position reference: @{ "code": "5", "abbreviation": "3B", ... }@.
data WirePositionRef = WirePositionRef
  { wprCode         :: Maybe Text
  , wprAbbreviation :: Maybe Text
  }
  deriving stock (Show, Eq)

instance FromJSON WirePositionRef where
  parseJSON = withObject "WirePositionRef" $ \o -> WirePositionRef
    <$> o .:? "code"
    <*> o .:? "abbreviation"

-- | Embedded hand reference: @{ "code": "L", "description": "Left" }@.
newtype WireHandRef = WireHandRef
  { whrCode :: Maybe Text }
  deriving stock (Show, Eq)

instance FromJSON WireHandRef where
  parseJSON = withObject "WireHandRef" $ \o -> WireHandRef
    <$> o .:? "code"-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/MLB/Wire/Player.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/MLB/Wire/Schedule.hs
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
      Just t  -> WireGameTeam <$> withObject "team" (\to -> to .:? "id") t-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/MLB/Wire/Schedule.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/MLB/ConvertSpec.hs
module Pelotero.MLB.ConvertSpec (spec) where

import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString as BS
import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  , shouldContain
  , shouldSatisfy
  )

import Pelotero.Domain.Game (GameSchedule(..), Game(..))
import Pelotero.Domain.Id (GameId(..), PlayerId(..), TeamId(..))
import Pelotero.Domain.Player (Player(..))
import Pelotero.Domain.Position (Position(..))
import Pelotero.MLB.Convert
  ( ConvertWarning(..)
  , convertBoxscore
  , convertPlayers
  , convertSchedule
  )
import qualified Pelotero.MLB.Wire.Boxscore as WB
import qualified Pelotero.MLB.Wire.Player as WP
import qualified Pelotero.MLB.Wire.Schedule as WS

spec :: Spec
spec = do
  describe "convertPlayers" $ do
    it "converts a clean roster sample with no warnings" $ do
      env <- decodeFixture "test/fixtures/players-clean.json"
      let (warns, players) = convertPlayers env
      warns `shouldBe` []
      length players `shouldBe` 2
      map playerId players `shouldBe` [PlayerId 660271, PlayerId 545361]
      map playerPosition players
        `shouldBe` [Just Pitcher, Just CenterField]
      map playerActive players `shouldBe` [True, True]

    it "drops invalid IDs and warns about unknown positions" $ do
      env <- decodeFixture "test/fixtures/players-dirty.json"
      let (warns, players) = convertPlayers env
      length players `shouldBe` 1                             -- the 0-id is dropped
      warns `shouldContain` [InvalidPlayerId 0]
      warns `shouldSatisfy` any isUnknownPos
      playerPosition (head players) `shouldBe` Nothing        -- "TWP" doesn't parse
      where
        isUnknownPos UnknownPosition{} = True
        isUnknownPos _                 = False

  describe "convertSchedule" $ do
    it "flattens dates and games" $ do
      env <- decodeFixture "test/fixtures/schedule.json"
      let (warns, GameSchedule games) = convertSchedule env
      warns `shouldBe` []
      length games `shouldBe` 2
      map gameId games `shouldBe` [GameId 778001, GameId 778002]
      map gameAwayTeam games `shouldBe` [TeamId 117, TeamId 110]
      map gameHomeTeam games `shouldBe` [TeamId 121, TeamId 145]

  describe "convertBoxscore" $ do
    it "produces one entry per appearance" $ do
      bs <- decodeFixture "test/fixtures/boxscore.json"
      let (warns, entries) = convertBoxscore bs
      warns `shouldBe` []
      length entries `shouldBe` 2

decodeFixture :: forall a. (Eq a, Show a, FromJSONFixture a) => FilePath -> IO a
decodeFixture path = do
  bs <- BS.readFile path
  case decodeFix bs of
    Right v  -> pure v
    Left err -> error ("fixture " <> path <> ": " <> err)

-- | Tiny class so we can decode each fixture into the right wire type. Avoids
-- a separate top-level `decode` per call site without leaking the decode
-- function names into every test case.
class FromJSONFixture a where
  decodeFix :: BS.ByteString -> Either String a

instance FromJSONFixture WP.WirePlayerEnvelope where
  decodeFix = eitherDecodeStrict

instance FromJSONFixture WS.WireScheduleEnvelope where
  decodeFix = eitherDecodeStrict

instance FromJSONFixture WB.WireBoxscore where
  decodeFix = eitherDecodeStrict-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/MLB/ConvertSpec.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Position.hs
-- | Defensive positions in baseball. Exhaustive — MLB defines exactly these
-- ten codes. If the MLB API ships something we don't recognise, that's a
-- parse error at the wire boundary, not a silent fallback into 'Position'.
module Pelotero.Domain.Position
  ( Position(..)
  , parsePosition
  , renderPosition
  , isPitcher
  , isInfielder
  , isOutfielder
  ) where

import Data.Text (Text)

-- | A defensive position. Codes match MLB's official numbering for batting
-- positions, plus DH and a synthetic "Pitcher" cover (MLB distinguishes SP/RP
-- only via separate tables; the position itself is just \"P\").
data Position
  = Pitcher        -- ^ MLB code "1"  / scorer "P"
  | Catcher        -- ^ MLB code "2"  / scorer "C"
  | FirstBase      -- ^ MLB code "3"  / scorer "1B"
  | SecondBase     -- ^ MLB code "4"  / scorer "2B"
  | ThirdBase      -- ^ MLB code "5"  / scorer "3B"
  | Shortstop      -- ^ MLB code "6"  / scorer "SS"
  | LeftField      -- ^ MLB code "7"  / scorer "LF"
  | CenterField    -- ^ MLB code "8"  / scorer "CF"
  | RightField     -- ^ MLB code "9"  / scorer "RF"
  | DesignatedHitter -- ^ MLB code "10" / scorer "DH"
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Parse a position from MLB's coded representation. Accepts both the
-- numeric form ("1".."10") and the scorer form ("P", "C", "1B", ..., "DH").
-- "TWP" (two-way player, e.g. Ohtani's primary position) is intentionally not
-- handled here; it requires a different domain model and is out of scope for
-- Phase 1.
parsePosition :: Text -> Maybe Position
parsePosition = \case
  "1"  -> Just Pitcher
  "2"  -> Just Catcher
  "3"  -> Just FirstBase
  "4"  -> Just SecondBase
  "5"  -> Just ThirdBase
  "6"  -> Just Shortstop
  "7"  -> Just LeftField
  "8"  -> Just CenterField
  "9"  -> Just RightField
  "10" -> Just DesignatedHitter
  "P"  -> Just Pitcher
  "C"  -> Just Catcher
  "1B" -> Just FirstBase
  "2B" -> Just SecondBase
  "3B" -> Just ThirdBase
  "SS" -> Just Shortstop
  "LF" -> Just LeftField
  "CF" -> Just CenterField
  "RF" -> Just RightField
  "DH" -> Just DesignatedHitter
  _    -> Nothing

-- | Canonical text rendering — the scorer form. Matches what shows up on a
-- box score.
renderPosition :: Position -> Text
renderPosition = \case
  Pitcher          -> "P"
  Catcher          -> "C"
  FirstBase        -> "1B"
  SecondBase       -> "2B"
  ThirdBase        -> "3B"
  Shortstop        -> "SS"
  LeftField        -> "LF"
  CenterField      -> "CF"
  RightField       -> "RF"
  DesignatedHitter -> "DH"

isPitcher :: Position -> Bool
isPitcher Pitcher = True
isPitcher _       = False

isInfielder :: Position -> Bool
isInfielder = \case
  FirstBase  -> True
  SecondBase -> True
  ThirdBase  -> True
  Shortstop  -> True
  Catcher    -> True
  _          -> False

isOutfielder :: Position -> Bool
isOutfielder = \case
  LeftField   -> True
  CenterField -> True
  RightField  -> True
  _           -> False-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Position.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Game.hs
-- | Domain representation of a scheduled or completed MLB game.
module Pelotero.Domain.Game
  ( Game(..)
  , GameSchedule(..)
  ) where

import Data.Time.Calendar (Day)

import Pelotero.Domain.Id (GameId, TeamId)

-- | A single scheduled game. We keep only what's needed to drive stat
-- collection: when, who, and (eventually) whether it's final.
data Game = Game
  { gameId       :: GameId
  , gameDate     :: Day
  , gameAwayTeam :: TeamId
  , gameHomeTeam :: TeamId
  }
  deriving stock (Show, Eq)

-- | A day's slate of games. The empty slate (off-day, all-star break) is
-- represented by an empty list — not 'Nothing' — so callers don't have to
-- handle "scheduled but no games".
newtype GameSchedule = GameSchedule
  { unGameSchedule :: [Game] }
  deriving stock (Show, Eq)-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Game.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Team.hs
-- | Domain representation of an MLB team. We track only the identity and
-- enough denormalised text to render lineups and box scores; we don't pull in
-- venues, divisions, or league affiliations at this stage.
module Pelotero.Domain.Team
  ( Team(..)
  ) where

import Data.Text (Text)

import Pelotero.Domain.Id (TeamId)

data Team = Team
  { teamId           :: TeamId
  , teamName         :: Text  -- ^ "Houston Astros"
  , teamAbbreviation :: Text  -- ^ "HOU"
  , teamLocationName :: Text  -- ^ "Houston"
  }
  deriving stock (Show, Eq)-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Team.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/PositionSpec.hs
module Pelotero.Domain.PositionSpec (spec) where

import Hedgehog (Gen, forAll, property, tripping, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Pelotero.Domain.Position
  ( Position(..)
  , isInfielder
  , isOutfielder
  , isPitcher
  , parsePosition
  , renderPosition
  )

spec :: Spec
spec = do
  describe "parsePosition / renderPosition" $ do
    it "round-trips for every Position" $ hedgehog $ property $ do
      p <- forAll genPosition
      tripping p renderPosition parsePosition

    it "accepts both numeric and scorer codes for one fixed point" $ do
      parsePosition "5"  `shouldBe` Just ThirdBase
      parsePosition "3B" `shouldBe` Just ThirdBase
      parsePosition "10" `shouldBe` Just DesignatedHitter
      parsePosition "DH" `shouldBe` Just DesignatedHitter

    it "rejects unknown codes" $ do
      parsePosition ""    `shouldBe` Nothing
      parsePosition "TWP" `shouldBe` Nothing
      parsePosition "11"  `shouldBe` Nothing

  describe "classification predicates" $ do
    it "Pitcher is a pitcher and nothing else" $ do
      isPitcher Pitcher       `shouldBe` True
      isInfielder Pitcher     `shouldBe` False
      isOutfielder Pitcher    `shouldBe` False

    it "Catcher is an infielder" $
      isInfielder Catcher `shouldBe` True

    it "outfielders are outfielders, not infielders" $ do
      isOutfielder LeftField   `shouldBe` True
      isOutfielder CenterField `shouldBe` True
      isOutfielder RightField  `shouldBe` True
      isInfielder LeftField    `shouldBe` False

    it "DH is none of the three" $ do
      isPitcher    DesignatedHitter `shouldBe` False
      isInfielder  DesignatedHitter `shouldBe` False
      isOutfielder DesignatedHitter `shouldBe` False

genPosition :: Gen Position
genPosition = Gen.element [minBound .. maxBound]-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/PositionSpec.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Player.hs
-- | Domain representation of an MLB player. Kept separate from the wire
-- shape ("Pelotero.MLB.Wire.Player") so the domain can evolve independently
-- and the wire layer stays free of business logic.
module Pelotero.Domain.Player
  ( Player(..)
  , Handedness(..)
  , parseHandedness
  , renderHandedness
  ) where

import Data.Text (Text)

import Pelotero.Domain.Id (PlayerId, TeamId)
import Pelotero.Domain.Position (Position)

-- | A roster entry. Stable identity (PlayerId), denormalised display name,
-- current MLB team, primary position, and handedness for batting and
-- pitching. Active flag from upstream — keep it; we filter on it elsewhere.
data Player = Player
  { playerId        :: PlayerId
  , playerFirstName :: Text       -- ^ MLB's "useName"
  , playerLastName  :: Text       -- ^ MLB's "useLastName"
  , playerNameSlug  :: Text       -- ^ URL-safe identifier
  , playerTeamId    :: Maybe TeamId
  , playerPosition  :: Maybe Position
  , playerBatSide   :: Maybe Handedness
  , playerPitchHand :: Maybe Handedness
  , playerActive    :: Bool
  }
  deriving stock (Show, Eq)

-- | Batter or pitcher hand. MLB also reports "S" for switch-hitters.
data Handedness = LeftHanded | RightHanded | Switch
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Parse from MLB's single-letter code. "L"/"R"/"S" are well-formed;
-- anything else returns Nothing.
parseHandedness :: Text -> Maybe Handedness
parseHandedness = \case
  "L" -> Just LeftHanded
  "R" -> Just RightHanded
  "S" -> Just Switch
  _   -> Nothing

renderHandedness :: Handedness -> Text
renderHandedness = \case
  LeftHanded  -> "L"
  RightHanded -> "R"
  Switch      -> "S"-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Player.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Id.hs
-- | Strongly-typed identifiers. Newtypes only — no smart constructors at this
-- layer. Validation (rejecting nonsense like @PlayerId 0@) happens at the wire
-- boundary in "Pelotero.MLB.Convert".
module Pelotero.Domain.Id
  ( PlayerId(..)
  , TeamId(..)
  , GameId(..)
  , SeasonYear(..)
  ) where

-- | MLB player ID. The MLB Stats API uses integers (e.g. 660271 for Shohei
-- Ohtani). We keep that representation.
newtype PlayerId = PlayerId { unPlayerId :: Int }
  deriving stock (Show, Eq, Ord)

-- | MLB team ID. Distinct from a fantasy league's team identifier (which we'll
-- introduce later as a different type). The MLB API uses small integers
-- (e.g. 117 for Houston Astros).
newtype TeamId = TeamId { unTeamId :: Int }
  deriving stock (Show, Eq, Ord)

-- | MLB game ID, sometimes called "gamePk" upstream.
newtype GameId = GameId { unGameId :: Int }
  deriving stock (Show, Eq, Ord)

-- | A baseball season, identified by year (e.g. @SeasonYear 2025@).
newtype SeasonYear = SeasonYear { unSeasonYear :: Int }
  deriving stock (Show, Eq, Ord)-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Id.hs

-- Start of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Stats.hs
-- | Per-game player statistics. 'Maybe Int' fields distinguish "stat absent"
-- (e.g. a pitcher's batting line on a day they didn't bat) from "stat present
-- and zero" (a hitter who went 0-for-3).
module Pelotero.Domain.Stats
  ( BattingStats(..)
  , PitchingStats(..)
  , emptyBatting
  , emptyPitching
  ) where

import Data.Text (Text)

-- | Per-game batting line. Fields are 'Maybe' to preserve "did this player
-- bat at all?" information from the upstream feed.
data BattingStats = BattingStats
  { batGamesPlayed          :: Maybe Int
  , batPlateAppearances     :: Maybe Int
  , batAtBats               :: Maybe Int
  , batRuns                 :: Maybe Int
  , batHits                 :: Maybe Int
  , batDoubles              :: Maybe Int
  , batTriples              :: Maybe Int
  , batHomeRuns             :: Maybe Int
  , batRbi                  :: Maybe Int
  , batBaseOnBalls          :: Maybe Int
  , batIntentionalWalks     :: Maybe Int
  , batStrikeOuts           :: Maybe Int
  , batStolenBases          :: Maybe Int
  , batCaughtStealing       :: Maybe Int
  , batHitByPitch           :: Maybe Int
  , batSacBunts             :: Maybe Int
  , batSacFlies             :: Maybe Int
  , batGroundIntoDoublePlay :: Maybe Int
  , batGroundIntoTriplePlay :: Maybe Int
  , batLeftOnBase           :: Maybe Int
  , batTotalBases           :: Maybe Int
  , batFlyOuts              :: Maybe Int
  , batGroundOuts           :: Maybe Int
  , batCatchersInterference :: Maybe Int
  , batPickoffs             :: Maybe Int
  }
  deriving stock (Show, Eq)

-- | A batting line with every field absent. Useful as a parser default.
emptyBatting :: BattingStats
emptyBatting = BattingStats
  { batGamesPlayed          = Nothing
  , batPlateAppearances     = Nothing
  , batAtBats               = Nothing
  , batRuns                 = Nothing
  , batHits                 = Nothing
  , batDoubles              = Nothing
  , batTriples              = Nothing
  , batHomeRuns             = Nothing
  , batRbi                  = Nothing
  , batBaseOnBalls          = Nothing
  , batIntentionalWalks     = Nothing
  , batStrikeOuts           = Nothing
  , batStolenBases          = Nothing
  , batCaughtStealing       = Nothing
  , batHitByPitch           = Nothing
  , batSacBunts             = Nothing
  , batSacFlies             = Nothing
  , batGroundIntoDoublePlay = Nothing
  , batGroundIntoTriplePlay = Nothing
  , batLeftOnBase           = Nothing
  , batTotalBases           = Nothing
  , batFlyOuts              = Nothing
  , batGroundOuts           = Nothing
  , batCatchersInterference = Nothing
  , batPickoffs             = Nothing
  }

-- | Per-game pitching line. 'pitInningsPitched' is text because MLB reports
-- it as a fractional string ("6.2" = six and two-thirds innings) which is
-- *not* a decimal — converting blindly to Double silently corrupts data.
-- Callers that need numeric IP should parse via a dedicated function.
data PitchingStats = PitchingStats
  { pitGamesPlayed             :: Maybe Int
  , pitGamesStarted            :: Maybe Int
  , pitGamesFinished           :: Maybe Int
  , pitCompleteGames           :: Maybe Int
  , pitShutouts                :: Maybe Int
  , pitWins                    :: Maybe Int
  , pitLosses                  :: Maybe Int
  , pitSaves                   :: Maybe Int
  , pitSaveOpportunities       :: Maybe Int
  , pitHolds                   :: Maybe Int
  , pitBlownSaves              :: Maybe Int
  , pitInningsPitched          :: Maybe Text  -- e.g. "6.2"
  , pitOuts                    :: Maybe Int
  , pitBattersFaced            :: Maybe Int
  , pitNumberOfPitches         :: Maybe Int
  , pitStrikes                 :: Maybe Int
  , pitBalls                   :: Maybe Int
  , pitHits                    :: Maybe Int
  , pitDoubles                 :: Maybe Int
  , pitTriples                 :: Maybe Int
  , pitHomeRuns                :: Maybe Int
  , pitRuns                    :: Maybe Int
  , pitEarnedRuns              :: Maybe Int
  , pitStrikeOuts              :: Maybe Int
  , pitBaseOnBalls             :: Maybe Int
  , pitIntentionalWalks        :: Maybe Int
  , pitHitBatsmen              :: Maybe Int
  , pitWildPitches             :: Maybe Int
  , pitBalks                   :: Maybe Int
  , pitPickoffs                :: Maybe Int
  , pitFlyOuts                 :: Maybe Int
  , pitGroundOuts              :: Maybe Int
  , pitAirOuts                 :: Maybe Int
  , pitInheritedRunners        :: Maybe Int
  , pitInheritedRunnersScored  :: Maybe Int
  , pitStolenBases             :: Maybe Int
  , pitCaughtStealing          :: Maybe Int
  , pitAtBats                  :: Maybe Int
  , pitRbi                     :: Maybe Int
  , pitSacBunts                :: Maybe Int
  , pitSacFlies                :: Maybe Int
  , pitCatchersInterference    :: Maybe Int
  , pitPassedBall              :: Maybe Int
  }
  deriving stock (Show, Eq)

emptyPitching :: PitchingStats
emptyPitching = PitchingStats
  { pitGamesPlayed             = Nothing
  , pitGamesStarted            = Nothing
  , pitGamesFinished           = Nothing
  , pitCompleteGames           = Nothing
  , pitShutouts                = Nothing
  , pitWins                    = Nothing
  , pitLosses                  = Nothing
  , pitSaves                   = Nothing
  , pitSaveOpportunities       = Nothing
  , pitHolds                   = Nothing
  , pitBlownSaves              = Nothing
  , pitInningsPitched          = Nothing
  , pitOuts                    = Nothing
  , pitBattersFaced            = Nothing
  , pitNumberOfPitches         = Nothing
  , pitStrikes                 = Nothing
  , pitBalls                   = Nothing
  , pitHits                    = Nothing
  , pitDoubles                 = Nothing
  , pitTriples                 = Nothing
  , pitHomeRuns                = Nothing
  , pitRuns                    = Nothing
  , pitEarnedRuns              = Nothing
  , pitStrikeOuts              = Nothing
  , pitBaseOnBalls             = Nothing
  , pitIntentionalWalks        = Nothing
  , pitHitBatsmen              = Nothing
  , pitWildPitches             = Nothing
  , pitBalks                   = Nothing
  , pitPickoffs                = Nothing
  , pitFlyOuts                 = Nothing
  , pitGroundOuts              = Nothing
  , pitAirOuts                 = Nothing
  , pitInheritedRunners        = Nothing
  , pitInheritedRunnersScored  = Nothing
  , pitStolenBases             = Nothing
  , pitCaughtStealing          = Nothing
  , pitAtBats                  = Nothing
  , pitRbi                     = Nothing
  , pitSacBunts                = Nothing
  , pitSacFlies                = Nothing
  , pitCatchersInterference    = Nothing
  , pitPassedBall              = Nothing
  }-- End of /home/bismuth/git/pelotero-engine/lib/Pelotero/Domain/Stats.hs

