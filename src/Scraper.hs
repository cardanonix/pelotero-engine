{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Scraper
    ( fetchGameScheduleForDate
    -- , dateToEpoch
    , hasGamesForDate
    , extractGameIds
    , processAndPrintGames
    , flattenGameData
    ) where

import Network.HTTP.Simple
import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Maybe (isJust, fromMaybe, maybeToList)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Aeson ((.:?))
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (id)

data GameSchedule = GameSchedule {
    dates :: [DateEntry]
} deriving (Show, Eq)

data DateEntry = DateEntry {
    games :: Maybe (V.Vector Game)
} deriving (Show, Eq)

data Game = Game {
    gamePk :: Int
} deriving (Show, Eq)

data Player = Player {
    person :: Person,
    allPositions :: Maybe [Position],
    parentTeamId :: Int,
    status :: Status,
    stats :: Stats
} deriving (Generic, FromJSON)

data Person = Person {
    id :: Int,
    fullName :: String
} deriving (Generic, FromJSON)

data Position = Position {
    posCode :: Int
} deriving (Generic, FromJSON)

data Status = Status {
    statusCode :: String
} deriving (Generic, FromJSON)

data Stats = Stats {
    batting :: Maybe Batting,
    pitching :: Maybe Pitching
} deriving (Generic, FromJSON)

data Batting = Batting {
            bat_gamesPlayed :: Int --1,
          , bat_flyOuts :: Int --1,
          , bat_groundOuts :: Int --1,
          , bat_runs :: Int --0,
          , bat_doubles :: Int --0,
          , bat_triples :: Int --0,
          , bat_homeRuns :: Int --0,
          , bat_strikeOuts :: Int --1,
          , bat_baseOnBalls :: Int --0,
          , bat_intentionalWalks :: Int --0,
          , bat_hits :: Int --0,
          , bat_hitByPitch :: Int --0,
          , bat_atBats :: Int --4,
          , bat_caughtStealing :: Int --0,
          , bat_stolenBases :: Int --0,
          , bat_groundIntoDoublePlay :: Int --0,
          , bat_groundIntoTriplePlay :: Int --0,
          , bat_plateAppearances :: Int --4,
          , bat_totalBases :: Int --0,
          , bat_rbi :: Int --0,
          , bat_leftOnBase :: Int --4,
          , bat_sacBunts :: Int --0,
          , bat_sacFlies :: Int --0,
          , bat_catchersInterference :: Int --0,
          , bat_pickoffs :: Int --0
          -- The following are meant to be discarded
          , bat_note :: Maybe String
          , bat_summary :: Maybe String
          , bat_stolenBasePercentage :: Maybe String
          , bat_atBatsPerHomeRun :: Maybe String
} deriving Generic


instance FromJSON Batting where
    parseJSON = genericParseJSON defaultOptions { 
        fieldLabelModifier = \str -> if take 4 str == "bat_" then drop 4 str else str
    }


data Pitching = Pitching {
            pit_gamesPlayed :: Int --1,
          , pit_gamesStarted :: Int --1,
          , pit_flyOuts :: Int --4,
          , pit_groundOuts :: Int --1,
          , pit_airOuts :: Int --6,
          , pit_runs :: Int --7,
          , pit_doubles :: Int --2,
          , pit_triples :: Int --0,
          , pit_homeRuns :: Int --2,
          , pit_strikeOuts :: Int --4,
          , pit_baseOnBalls :: Int --3,
          , pit_intentionalWalks :: Int --0,
          , pit_hits :: Int --8,
          , pit_hitByPitch :: Int --0,
          , pit_atBats :: Int --18,
          , pit_caughtStealing :: Int --0,
          , pit_stolenBases :: Int --0,
          , pit_numberOfPitches :: Int --82,
          , pit_inningsPitched :: String --"3.2",
          , pit_wins :: Int --0,
          , pit_losses :: Int --1,
          , pit_saves :: Int --0,
          , pit_saveOpportunities :: Int --0,
          , pit_holds :: Int --0,
          , pit_blownSaves :: Int --0,
          , pit_earnedRuns :: Int --7,
          , pit_battersFaced :: Int --22,
          , pit_outs :: Int --11,
          , pit_gamesPitched :: Int --1,
          , pit_completeGames :: Int --0,
          , pit_shutouts :: Int --0,
          , pit_pitchesThrown :: Int --82,
          , pit_balls :: Int --34,
          , pit_strikes :: Int --48,
          , pit_hitBatsmen :: Int --0,
          , pit_balks :: Int --0,
          , pit_wildPitches :: Int --1,
          , pit_pickoffs :: Int --0,
          , pit_rbi :: Int --6,
          , pit_gamesFinished :: Int --0,
          , pit_inheritedRunners :: Int --0,
          , pit_inheritedRunnersScored :: Int --0,
          , pit_catchersInterference :: Int --0,
          , pit_sacBunts :: Int --0,
          , pit_sacFlies :: Int --1,
          , pit_passedBall :: Int --0
          -- The following are meant to be discarded
          , pit_note :: Maybe String
          , pit_summary :: Maybe String
          , pit_stolenBasePercentage :: Maybe String
          , pit_strikePercentage :: Maybe String
          , pit_homeRunsPer9 :: Maybe String
          , pit_runsScoredPer9 :: Maybe String
} deriving Generic

instance FromJSON Pitching where
    parseJSON = genericParseJSON defaultOptions { 
        fieldLabelModifier = \str -> if take 4 str == "pit_" then drop 4 str else str
    }

instance FromJSON GameSchedule where
    parseJSON = withObject "GameSchedule" $ \v -> GameSchedule
        <$> v .: "dates"

instance FromJSON DateEntry where
    parseJSON = withObject "DateEntry" $ \v -> DateEntry
        <$> v .:? "games"

instance FromJSON Game where
    parseJSON = withObject "Game" $ \v -> Game
        <$> v .: "gamePk"

-- takes a date string "YYYY-MM-DD" and outputs a schedule bytestring of that day schdule
fetchGameScheduleForDate :: String -> IO ByteString
fetchGameScheduleForDate date = do
    let apiUrl = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date
    response <- httpBS (parseRequest_ apiUrl)
    return $ getResponseBody response

-- takes a schedule bytestring and outputs true if games are happening or Nothing is not
hasGamesForDate :: ByteString -> Maybe Bool
hasGamesForDate jsonData =
    case eitherDecodeStrict jsonData :: Either String GameSchedule of
        Right schedule -> Just $ any (isJust . games) (dates schedule)
        Left _ -> Nothing

-- takes a schedule bytestring and outputs an array of gameId's or errors
extractGameIds :: ByteString -> Either String [Int]
extractGameIds jsonData = 
    case eitherDecodeStrict jsonData :: Either String GameSchedule of
        Right gameData -> Right $ concatMap (maybe [] (V.toList . fmap gamePk) . games) (dates gameData)
        Left e -> Left e

-- string that indicates the status of the game
data GameStatus = GameStatus {
    codedGameState :: Text
} deriving (Show, Eq)

data GameDataWrapper = GameDataWrapper {
    gameData :: GameStatus
} deriving (Show, Eq)

data PlayerInfo = PlayerInfo {
    pInfoId :: Int,
    pInfoFullName :: String,
    pInfoStats :: PlayerStats
}

instance FromJSON PlayerInfo where
    parseJSON = genericParseJSON defaultOptions { 
        fieldLabelModifier = \str -> if take 1 str == "p" then drop 1 str else str
    }

data PlayerStats = PlayerStats {
    playergameId :: Int,
    playerparentTeamId :: Int,
    playerallPositions :: [Int],
    playerstatus :: String,
    playerbatting :: Maybe Batting,
    playerpitching :: Maybe Pitching
}

instance FromJSON PlayerStats where
    parseJSON = genericParseJSON defaultOptions { 
        fieldLabelModifier = \str -> if take 6 str == "player" then drop 6 str else str
    }

instance FromJSON GameStatus where
    parseJSON = withObject "GameStatus" $ \v -> GameStatus
        <$> v .: "codedGameState"

instance FromJSON GameDataWrapper where
    parseJSON = withObject "GameDataWrapper" $ \v -> GameDataWrapper
        <$> v .: "gameData"

-- takes a game ID and outputs the live coded status of that game
fetchGameStatus :: Int -> IO (Either String GameDataWrapper)
fetchGameStatus gameId = do
    let apiUrl = "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live"
    response <- httpBS (parseRequest_ apiUrl)
    return $ eitherDecodeStrict $ getResponseBody response

-- takes a game id and outputs a box score bytestring
fetchFinishedBxScore :: Int -> IO ByteString
fetchFinishedBxScore gameId = do
    gameStatusJson <- httpBS (parseRequest_ $ "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live")
    let gameStatus = eitherDecodeStrict (getResponseBody gameStatusJson) :: Either String GameDataWrapper
    case gameStatus of
        Right gameDataWrapper -> 
            if codedGameState (gameData gameDataWrapper) == "F"
            then do
                -- this is where the box score gets imported
                fullGameData <- httpBS (parseRequest_ $ "http://statsapi.mlb.com/api/v1/game/" ++ show gameId ++ "/boxscore")
                return $ getResponseBody fullGameData
            else return BL.empty
        Left _ -> return BL.empty

--maps fetchFinishedBxScore over an array of game id's and returns IO (M.Map Int ByteString)
processGameIds :: [Int] -> IO (M.Map Int ByteString)
processGameIds gameIds = do
    gameDataResponses <- mapM fetchFinishedBxScore gameIds
    return $ M.fromList $ zip gameIds gameDataResponses

-- 
printProcessedGameData :: M.Map Int ByteString -> IO ()
printProcessedGameData gameDataMap =
    mapM_ (\(gameId, gameData) -> putStrLn $ show gameId ++ show gameData) (M.toList gameDataMap)

-- print the schedule bytestring
processAndPrintGames :: ByteString -> IO ()
processAndPrintGames gameSchedule = do
    let gameIdsResult = extractGameIds gameSchedule
    case gameIdsResult of
        Left errMsg -> putStrLn $ "Error extracting game IDs: " ++ errMsg
        Right gameIds -> do
            gameDataMap <- processGameIds gameIds
            printProcessedGameData gameDataMap

type GameData = BL.ByteString

flattenGameData :: BL.ByteString -> Int -> Either String BL.ByteString
flattenGameData gameData gameId = do
    -- Step 1: Decode the gameData
    decodedData <- eitherDecodeStrict gameData :: Either String GameData

    -- Step 2: Transform this data structure.
    let teams = [teamPlayers | team <- [awayTeam decodedData, homeTeam decodedData],
                              teamPlayers <- maybeToList (players team)]
        transformed = map (transformPlayer gameId) teams

    -- Step 3: Encode the new representation back to JSON.
    return $ encode transformed
  where
    transformPlayer :: Int -> Player -> M.Map String PlayerInfo
    transformPlayer gid player = 
        let personId = show $ id $ person player
            newStats = PlayerStats {
                playergameId = gid,
                playerparentTeamId = parentTeamId player,
                playerallPositions = map posCode $ fromMaybe [] (allPositions player),
                playerstatus = statusCode $ status player,
                playerbatting = removeUnwantedBattingFields <$> batting (stats player),
                playerpitching = removeUnwantedPitchingFields <$> pitching (stats player)
            }
        in M.singleton personId (PlayerInfo (id $ person player) (fullName $ person player) newStats)

    removeUnwantedBattingFields :: Batting -> Batting
    removeUnwantedBattingFields bat = 
        bat { bat_note = Nothing
            , bat_summary = Nothing
            , bat_stolenBasePercentage = Nothing
            , bat_atBatsPerHomeRun = Nothing
            }

    removeUnwantedPitchingFields :: Pitching -> Pitching
    removeUnwantedPitchingFields pitch =
        pitch { pit_note = Nothing
            , pit_summary = Nothing
            , pit_stolenBasePercentage = Nothing
            , pit_strikePercentage = Nothing
            , pit_homeRunsPer9 = Nothing
            , pit_runsScoredPer9 = Nothing
            }

