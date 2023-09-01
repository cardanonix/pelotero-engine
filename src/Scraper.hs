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
import Data.Aeson
import qualified Data.Vector as V
import Data.Maybe (isJust)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Aeson ((.:?))
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

          , bat_note :: String
          , bat_summary :: String
          , bat_stolenBasePercentage :: Double
          , bat_atBatsPerHomeRun :: Double
} deriving (Generic, FromJSON)

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
          , pit_inningsPitched :: Double --"3.2",
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

          , pit_note :: String
          , pit_summary :: String
          , pit_stolenBasePercentage :: Double
          , pit_strikePercentage :: Double
          , pit_homeRunsPer9 :: Double
          , pit_runsScoredPer9 :: Double
} deriving (Generic, FromJSON)

instance FromJSON GameSchedule where
    parseJSON = withObject "GameSchedule" $ \v -> GameSchedule
        <$> v .: "dates"

instance FromJSON DateEntry where
    parseJSON = withObject "DateEntry" $ \v -> DateEntry
        <$> v .:? "games"

instance FromJSON Game where
    parseJSON = withObject "Game" $ \v -> Game
        <$> v .: "gamePk"

fetchGameScheduleForDate :: String -> IO ByteString
fetchGameScheduleForDate date = do
    let apiUrl = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date
    response <- httpBS (parseRequest_ apiUrl)
    return $ getResponseBody response

hasGamesForDate :: ByteString -> Maybe Bool
hasGamesForDate jsonData =
    case eitherDecodeStrict jsonData :: Either String GameSchedule of
        Right schedule -> Just $ any (isJust . games) (dates schedule)
        Left _ -> Nothing

extractGameIds :: ByteString -> Either String [Int]
extractGameIds jsonData = 
    case eitherDecodeStrict jsonData :: Either String GameSchedule of
        Right gameData -> Right $ concatMap (maybe [] (V.toList . fmap gamePk) . games) (dates gameData)
        Left e -> Left e
 
data GameStatus = GameStatus {
    codedGameState :: Text
} deriving (Show, Eq)

data GameDataWrapper = GameDataWrapper {
    gameData :: GameStatus
} deriving (Show, Eq)

instance FromJSON GameStatus where
    parseJSON = withObject "GameStatus" $ \v -> GameStatus
        <$> v .: "codedGameState"

instance FromJSON GameDataWrapper where
    parseJSON = withObject "GameDataWrapper" $ \v -> GameDataWrapper
        <$> v .: "gameData"

instance FromJSON Batting where
    parseJSON = genericParseJSON defaultOptions { 
        fieldLabelModifier = \str -> if take 4 str == "bat_" then drop 4 str else str
    }

instance FromJSON Pitching where
    parseJSON = genericParseJSON defaultOptions { 
        fieldLabelModifier = \str -> if take 4 str == "pit_" then drop 4 str else str
    }

fetchGameStatus :: Int -> IO (Either String GameDataWrapper)
fetchGameStatus gameId = do
    let apiUrl = "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live"
    response <- httpBS (parseRequest_ apiUrl)
    return $ eitherDecodeStrict $ getResponseBody response

processAndPrintGames :: ByteString -> IO ()
processAndPrintGames gameSchedule = do
    let gameIdsResult = extractGameIds gameSchedule
    case gameIdsResult of
        Left errMsg -> putStrLn $ "Error extracting game IDs: " ++ errMsg
        Right gameIds -> do
            gameDataMap <- processGameIds gameIds
            printProcessedGameData gameDataMap

printProcessedGameData :: M.Map Int ByteString -> IO ()
printProcessedGameData gameDataMap =
    mapM_ (\(gameId, gameData) -> putStrLn $ show gameId ++ show gameData) (M.toList gameDataMap)

processGameIds :: [Int] -> IO (M.Map Int ByteString)
processGameIds gameIds = do
    gameDataResponses <- mapM fetchGameDataForFinishedGame gameIds
    return $ M.fromList $ zip gameIds gameDataResponses

fetchGameDataForFinishedGame :: Int -> IO ByteString
fetchGameDataForFinishedGame gameId = do
    gameStatusJson <- httpBS (parseRequest_ $ "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live")
    let gameStatus = decodeStrict (getResponseBody gameStatusJson) :: Maybe Value
    -- Here, you should extract the game status from the JSON, but for brevity, we'll assume every game has status "F"
    -- TODO: Use appropriate library function to extract 'codedGameState' from 'gameStatusJson'
    case gameStatus of
        Just (String status) -> 
            if status == "F"
            then httpBS (parseRequest_ $ "http://statsapi.mlb.com/api/v1/game/" ++ show gameId ++ "/boxscore") >>= return . getResponseBody
            else return ""
        _ -> return ""

flattenGameData :: BL.ByteString -> Int -> Either String BL.ByteString
flattenGameData gameData gameId = do
    -- Step 1: Decode the gameData
    decodedData <- eitherDecode gameData :: Either String GameData

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
                gameId = gid,
                parentTeamId = parentTeamId player,
                allPositions = map posCode $ fromMaybe [] (allPositions player),
                status = statusCode $ status player,
                batting = removeUnwantedBattingFields <$> batting (stats player),
                pitching = removeUnwantedPitchingFields <$> pitching (stats player)
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

