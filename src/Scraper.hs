{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE GADTs #-}

module Scraper
    ( fetchGameScheduleForDate
    -- , dateToEpoch
    , hasGamesForDate
    , extractGameIds
    , processAndPrintGames
    -- , flattenGameData
    ) where

import Network.HTTP.Simple
    ( parseRequest_, getResponseBody, httpBS )
import Data.Time ()
import Data.Time.Clock.POSIX ()
import Data.ByteString (ByteString, empty)
import qualified Data.Vector as V
import Data.Maybe (isJust, fromMaybe, maybeToList)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Aeson
    ( eitherDecode,
      eitherDecodeStrict,
      encode,
      (.:),
      (.:?),
      genericParseJSON,
      withObject,
      defaultOptions,
      FromJSON(parseJSON),
      Options(fieldLabelModifier) )
import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (id)

data GameSchedule where
  GameSchedule :: {dates :: [DateEntry]} -> GameSchedule
  deriving (Show, Eq)

data DateEntry where
  DateEntry :: {games :: Maybe (V.Vector Game)} -> DateEntry
  deriving (Show, Eq)

data Game where
  Game :: {gamePk :: Int} -> Game
  deriving (Show, Eq)

data Player = Player {
    person :: Person,
    allPositions :: Maybe [Position],
    parentTeamId :: Int,
    status :: Status,
    stats :: Stats
} deriving (Generic, Data.Aeson.FromJSON)

data Person = Person {
    id :: Int,
    fullName :: String
} deriving (Generic, Data.Aeson.FromJSON)

data Position where
  Position :: {posCode :: Int} -> Position
  deriving (Generic, Data.Aeson.FromJSON)

data Status where
  Status :: {statusCode :: String} -> Status
  deriving (Generic, Data.Aeson.FromJSON)

data Stats = Stats {
    batting :: Maybe Batting,
    pitching :: Maybe Pitching
} deriving (Generic, Data.Aeson.FromJSON)

data Batting where
  Batting :: {bat_gamesPlayed :: Int,
                bat_flyOuts :: Int,
                bat_groundOuts :: Int,
                bat_runs :: Int,
                bat_doubles :: Int,
                bat_triples :: Int,
                bat_homeRuns :: Int,
                bat_strikeOuts :: Int,
                bat_baseOnBalls :: Int,
                bat_intentionalWalks :: Int,
                bat_hits :: Int,
                bat_hitByPitch :: Int,
                bat_atBats :: Int,
                bat_caughtStealing :: Int,
                bat_stolenBases :: Int,
                bat_groundIntoDoublePlay :: Int,
                bat_groundIntoTriplePlay :: Int,
                bat_plateAppearances :: Int,
                bat_totalBases :: Int,
                bat_rbi :: Int,
                bat_leftOnBase :: Int,
                bat_sacBunts :: Int,
                bat_sacFlies :: Int,
                bat_catchersInterference :: Int,
                bat_pickoffs :: Int,
                bat_note :: Maybe String,
                bat_summary :: Maybe String,
                bat_stolenBasePercentage :: Maybe String,
                bat_atBatsPerHomeRun :: Maybe String}
               -> Batting
  deriving Generic


instance Data.Aeson.FromJSON Batting where
    parseJSON = Data.Aeson.genericParseJSON Data.Aeson.defaultOptions {
        fieldLabelModifier = \str -> if take 4 str == "bat_" then drop 4 str else str
    }


data Pitching where
  Pitching :: {pit_gamesPlayed :: Int,
                 pit_gamesStarted :: Int,
                 pit_flyOuts :: Int,
                 pit_groundOuts :: Int,
                 pit_airOuts :: Int,
                 pit_runs :: Int,
                 pit_doubles :: Int,
                 pit_triples :: Int,
                 pit_homeRuns :: Int,
                 pit_strikeOuts :: Int,
                 pit_baseOnBalls :: Int,
                 pit_intentionalWalks :: Int,
                 pit_hits :: Int,
                 pit_hitByPitch :: Int,
                 pit_atBats :: Int,
                 pit_caughtStealing :: Int,
                 pit_stolenBases :: Int,
                 pit_numberOfPitches :: Int,
                 pit_inningsPitched :: String,
                 pit_wins :: Int,
                 pit_losses :: Int,
                 pit_saves :: Int,
                 pit_saveOpportunities :: Int,
                 pit_holds :: Int,
                 pit_blownSaves :: Int,
                 pit_earnedRuns :: Int,
                 pit_battersFaced :: Int,
                 pit_outs :: Int,
                 pit_gamesPitched :: Int,
                 pit_completeGames :: Int,
                 pit_shutouts :: Int,
                 pit_pitchesThrown :: Int,
                 pit_balls :: Int,
                 pit_strikes :: Int,
                 pit_hitBatsmen :: Int,
                 pit_balks :: Int,
                 pit_wildPitches :: Int,
                 pit_pickoffs :: Int,
                 pit_rbi :: Int,
                 pit_gamesFinished :: Int,
                 pit_inheritedRunners :: Int,
                 pit_inheritedRunnersScored :: Int,
                 pit_catchersInterference :: Int,
                 pit_sacBunts :: Int,
                 pit_sacFlies :: Int,
                 pit_passedBall :: Int,
                 pit_note :: Maybe String,
                 pit_summary :: Maybe String,
                 pit_stolenBasePercentage :: Maybe String,
                 pit_strikePercentage :: Maybe String,
                 pit_homeRunsPer9 :: Maybe String,
                 pit_runsScoredPer9 :: Maybe String}
                -> Pitching
  deriving Generic

instance Data.Aeson.FromJSON Pitching where
    parseJSON = Data.Aeson.genericParseJSON Data.Aeson.defaultOptions {
        fieldLabelModifier = \str -> if take 4 str == "pit_" then drop 4 str else str
    }

instance Data.Aeson.FromJSON GameSchedule where
    parseJSON = Data.Aeson.withObject "GameSchedule" $ \v -> GameSchedule
        <$> v Data.Aeson..: "dates"

instance Data.Aeson.FromJSON DateEntry where
    parseJSON = Data.Aeson.withObject "DateEntry" $ \v -> DateEntry
        <$> v Data.Aeson..:? "games"

instance Data.Aeson.FromJSON Game where
    parseJSON = Data.Aeson.withObject "Game" $ \v -> Game
        <$> v Data.Aeson..: "gamePk"

-- takes a date string "YYYY-MM-DD" and outputs a schedule bytestring of that day schdule
fetchGameScheduleForDate :: String -> IO ByteString
fetchGameScheduleForDate date = do
    let apiUrl = "https://statsapi.mlb.com/api/v1/schedule/games/?language=en&sportId=1&startDate=" ++ date ++ "&endDate=" ++ date
    response <- httpBS (parseRequest_ apiUrl)
    return $ getResponseBody response

-- takes a schedule bytestring and outputs true if games are happening or Nothing is not
hasGamesForDate :: ByteString -> Maybe Bool
hasGamesForDate jsonData =
    case Data.Aeson.eitherDecodeStrict jsonData :: Either String GameSchedule of
        Right schedule -> Just $ any (isJust . games) (dates schedule)
        Left _ -> Nothing

-- takes a schedule bytestring and outputs an array of gameId's or errors
extractGameIds :: ByteString -> Either String [Int]
extractGameIds jsonData =
    case Data.Aeson.eitherDecodeStrict jsonData :: Either String GameSchedule of
        Right gameData -> Right $ concatMap (maybe [] (V.toList . fmap gamePk) . games) (dates gameData)
        Left e -> Left e

-- string that indicates the status of the game
data GameStatus where
  GameStatus :: {codedGameState :: Text} -> GameStatus
  deriving (Show, Eq)

data GameDataWrapper where
  GameDataWrapper :: {gameData :: GameStatus} -> GameDataWrapper
  deriving (Show, Eq)

data PlayerInfo where
  PlayerInfo :: {pInfoId :: Int,
                   pInfoFullName :: String,
                   pInfoStats :: PlayerStats}
                  -> PlayerInfo

-- instance Data.Aeson.FromJSON PlayerInfo where
--     parseJSON = Data.Aeson.genericParseJSON Data.Aeson.defaultOptions {
--         fieldLabelModifier = \str -> if take 1 str == "p" then drop 1 str else str
--     }

data PlayerStats = PlayerStats {
    playergameId :: Int,
    playerparentTeamId :: Int,
    playerallPositions :: [Int],
    playerstatus :: String,
    playerbatting :: Maybe Batting,
    playerpitching :: Maybe Pitching
}
--good stuff
-- instance Data.Aeson.FromJSON PlayerStats where
--     parseJSON = Data.Aeson.genericParseJSON Data.Aeson.defaultOptions {
--         fieldLabelModifier = \str -> if take 6 str == "player" then drop 6 str else str
--     }

instance Data.Aeson.FromJSON GameStatus where
    parseJSON = Data.Aeson.withObject "GameStatus" $ \v -> GameStatus
        <$> v Data.Aeson..: "codedGameState"

instance Data.Aeson.FromJSON GameDataWrapper where
    parseJSON = Data.Aeson.withObject "GameDataWrapper" $ \v -> GameDataWrapper
        <$> v Data.Aeson..: "gameData"

-- takes a game ID and outputs the live coded status of that game
fetchGameStatus :: Int -> IO (Either String GameDataWrapper)
fetchGameStatus gameId = do
    let apiUrl = "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live"
    response <- httpBS (parseRequest_ apiUrl)
    return $ Data.Aeson.eitherDecodeStrict $ getResponseBody response

-- takes a game id and outputs a box score bytestring
fetchFinishedBxScore :: Int -> IO ByteString
fetchFinishedBxScore gameId = do
    gameStatusJson <- httpBS (parseRequest_ $ "https://statsapi.mlb.com//api/v1.1/game/" ++ show gameId ++ "/feed/live")
    let gameStatus = Data.Aeson.eitherDecodeStrict (getResponseBody gameStatusJson) :: Either String GameDataWrapper
    case gameStatus of
        Right gameDataWrapper ->
            if codedGameState (gameData gameDataWrapper) == "F"
            then do
                -- this is where the box score gets imported
                fullGameData <- httpBS (parseRequest_ $ "http://statsapi.mlb.com/api/v1/game/" ++ show gameId ++ "/boxscore")
                return $ getResponseBody fullGameData
            else return empty
        Left _ -> return empty

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

-- type GameData = ByteString

-- flattenGameData :: BL.ByteString -> Int -> Either String BL.ByteString
-- flattenGameData gameData gameId = do
--     -- Step 1: Decode the gameData
--     decodedData <- Data.Aeson.eitherDecode gameData :: Either String GameData

--     -- Step 2: Transform this data structure.
--     let teams = [teamPlayers | team <- [awayTeam decodedData, homeTeam decodedData],
--                               teamPlayers <- maybeToList (players team)]
--         transformed = map (transformPlayer gameId) teams

--     -- Step 3: Encode the new representation back to JSON.
--     return $ Data.Aeson.encode transformed
--   where
--     transformPlayer :: Int -> Player -> M.Map String PlayerInfo
--     transformPlayer gid player =
--         let personId = show $ id $ person player
--             newStats = PlayerStats {
--                 playergameId = gid,
--                 playerparentTeamId = parentTeamId player,
--                 playerallPositions = maybe [] (map posCode) (allPositions player),
--                 playerstatus = statusCode $ status player,
--                 playerbatting = removeUnwantedBattingFields <$> batting (stats player),
--                 playerpitching = removeUnwantedPitchingFields <$> pitching (stats player)
--             }
--         in M.singleton personId (PlayerInfo (id $ person player) (fullName $ person player) newStats)

--     removeUnwantedBattingFields :: Batting -> Batting
--     removeUnwantedBattingFields bat =
--         bat { bat_note = Nothing
--             , bat_summary = Nothing
--             , bat_stolenBasePercentage = Nothing
--             , bat_atBatsPerHomeRun = Nothing
--             }

--     removeUnwantedPitchingFields :: Pitching -> Pitching
--     removeUnwantedPitchingFields pitch =
--         pitch { pit_note = Nothing
--             , pit_summary = Nothing
--             , pit_stolenBasePercentage = Nothing
--             , pit_strikePercentage = Nothing
--             , pit_homeRunsPer9 = Nothing
--             , pit_runsScoredPer9 = Nothing
--             }

