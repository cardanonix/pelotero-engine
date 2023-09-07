{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Data.ByteString ( ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString as B (readFile)
import Data.Aeson (decode, Result(Success), FromJSON(..), Value, (.:), (.:?), (.!=), fromJSON, withObject, eitherDecodeStrict)
import Data.Aeson.Types (Parser, Result(..))
import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShowM)


-- Top level data type
data GameData = GameData
    { teams :: Teams
    } deriving (Show, Eq)

-- Teams data type
data Teams = Teams
    { away :: TeamData
    , home :: TeamData
    } deriving (Show, Eq)

instance FromJSON Teams where
    parseJSON = withObject "Teams" $ \v -> Teams
        <$> v .: "away"
        <*> v .: "home"

data TeamData = TeamData
    { players :: M.Map Text Player
    } deriving (Show, Eq)

hasValidPositions :: Value -> Bool
hasValidPositions val = case fromJSON val :: Result Player of
    Success player -> case allPositions player of
                        Just positions -> not (null positions)
                        Nothing -> False
    _ -> False

instance FromJSON TeamData where
    parseJSON = withObject "TeamData" $ \v -> do
        playersMap <- v .: "players" :: Parser (M.Map Text Value)

        -- Print out the parsed players before filtering
        let parsedPlayers = map (\(k, v) -> (k, fromJSON v :: Result Player)) (M.toList playersMap)
        traceShowM parsedPlayers
        
        let maybePlayersList = map (\(k, v) -> 
                case fromJSON v of
                    Success player -> 
                        if hasValidPositions v then Just (k, player) else Nothing
                    _ -> Nothing) (M.toList playersMap)

        let validPlayers = M.fromList $ catMaybes maybePlayersList
        pure TeamData { players = validPlayers }

type Players = [(Text, Player)]

-- Player data structure
data Player = Player
    { person          :: Person
    , parentTeamId    :: Int
    , allPositions    :: Maybe [Position]
    , status          :: Status
    , stats           :: PlayerStats
    } deriving (Show, Eq)

instance FromJSON Player where
    parseJSON = withObject "Player" $ \v -> do
        person <- v .: "person"
        teamId <- v .: "parentTeamId"
        positions <- v .:? "allPositions" .!= []
        -- Since we're filtering at TeamData level, we don't need the condition here
        status <- v .: "status"
        stats <- v .: "stats"
        return $ Player person teamId (Just positions) status stats


data Person = Person
    { personId   :: Int
    , fullName :: Text
    } deriving (Show, Eq)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v ->
        Person <$> v .: "id"
               <*> v .: "fullName"

data Position = Position
    { pos_code :: Text
    } deriving (Show, Eq)

instance FromJSON Position where
    parseJSON = withObject "Position" $ \v -> Position
        <$> v .: "code"

data Status = Status
    { status_code :: Text
    } deriving (Show, Eq)

instance FromJSON Status where
    parseJSON = withObject "Status" $ \v -> Status
        <$> v .: "code"

data PlayerStats = PlayerStats
    { batting  :: Maybe BattingStats
    , pitching :: Maybe PitchingStats
    } deriving (Show, Eq)

instance FromJSON PlayerStats where
    parseJSON = withObject "PlayerStats" $ \v -> PlayerStats
        <$> v .:? "batting"
        <*> v .:? "pitching"

data BattingStats = BattingStats
    { bat_gamesPlayed           :: Int
    , bat_flyOuts               :: Int
    , bat_groundOuts            :: Int
    , bat_runs                  :: Int
    , bat_doubles               :: Int
    , bat_triples               :: Int
    , bat_homeRuns              :: Int
    , bat_strikeOuts            :: Int
    , bat_baseOnBalls           :: Int
    , bat_intentionalWalks      :: Int
    , bat_hits                  :: Int
    , bat_hitByPitch            :: Int
    , bat_atBats                :: Int
    , bat_caughtStealing        :: Int
    , bat_stolenBases           :: Int
    , bat_groundIntoDoublePlay  :: Int
    , bat_groundIntoTriplePlay  :: Int
    , bat_plateAppearances      :: Int
    , bat_totalBases            :: Int
    , bat_rbi                   :: Int
    , bat_leftOnBase            :: Int
    , bat_sacBunts              :: Int
    , bat_sacFlies              :: Int
    , bat_catchersInterference  :: Int
    , bat_pickoffs              :: Int
    , bat_note                  :: Text
    , bat_summary               :: Text
    , bat_stolenBasePercentage  :: Double
    , bat_atBatsPerHomeRun      :: Double
    } deriving (Show, Eq)

instance FromJSON BattingStats where
    parseJSON = withObject "BattingStats" $ \v -> BattingStats
        <$> v .: "gamesPlayed"
        <*> v .: "flyOuts"
        <*> v .: "groundOuts"
        <*> v .: "runs"
        <*> v .: "doubles"
        <*> v .: "triples"
        <*> v .: "homeRuns"
        <*> v .: "strikeOuts"
        <*> v .: "baseOnBalls"
        <*> v .: "intentionalWalks"
        <*> v .: "hits"
        <*> v .: "hitByPitch"
        <*> v .: "atBats"
        <*> v .: "caughtStealing"
        <*> v .: "stolenBases"
        <*> v .: "groundIntoDoublePlay"
        <*> v .: "groundIntoTriplePlay"
        <*> v .: "plateAppearances"
        <*> v .: "totalBases"
        <*> v .: "rbi"
        <*> v .: "leftOnBase"
        <*> v .: "sacBunts"
        <*> v .: "sacFlies"
        <*> v .: "catchersInterference"
        <*> v .: "pickoffs"
        <*> v .: "note"
        <*> v .: "summary"
        <*> v .: "stolenBasePercentage"
        <*> v .: "atBatsPerHomeRun"

data PitchingStats = PitchingStats
    { pit_summary               :: Text
    , pit_gamesPlayed           :: Int
    , pit_gamesStarted          :: Int
    , pit_flyOuts               :: Int
    , pit_groundOuts            :: Int
    , pit_airOuts               :: Int
    , pit_runs                  :: Int
    , pit_doubles               :: Int
    , pit_triples               :: Int
    , pit_homeRuns              :: Int
    , pit_strikeOuts            :: Int
    , pit_baseOnBalls           :: Int
    , pit_intentionalWalks      :: Int
    , pit_hits                  :: Int
    , pit_hitByPitch            :: Int
    , pit_atBats                :: Int
    , pit_caughtStealing        :: Int
    , pit_stolenBases           :: Int
    , pit_stolenBasePercentage  :: Text
    , pit_numberOfPitches       :: Int
    , pit_inningsPitched        :: Text
    , pit_wins                  :: Int
    , pit_losses                :: Int
    , pit_saves                 :: Int
    , pit_saveOpportunities     :: Int
    , pit_holds                 :: Int
    , pit_blownSaves            :: Int
    , pit_earnedRuns            :: Int
    , pit_battersFaced          :: Int
    , pit_outs                  :: Int
    , pit_gamesPitched          :: Int
    , pit_completeGames         :: Int
    , pit_shutouts              :: Int
    , pit_pitchesThrown         :: Int
    , pit_balls                 :: Int
    , pit_strikes               :: Int
    , pit_strikePercentage      :: Text
    , pit_hitBatsmen            :: Int
    , pit_balks                 :: Int
    , pit_wildPitches           :: Int
    , pit_pickoffs              :: Int
    , pit_rbi                   :: Int
    , pit_gamesFinished         :: Int
    , pit_runsScoredPer9        :: Text
    , pit_homeRunsPer9          :: Text
    , pit_inheritedRunners      :: Int
    , pit_inheritedRunnersScored:: Int
    , pit_catchersInterference  :: Int
    , pit_sacBunts              :: Int
    , pit_sacFlies              :: Int
    , pit_passedBall            :: Int
    } deriving (Show, Eq)

instance FromJSON PitchingStats where
    parseJSON = withObject "PitchingStats" $ \v -> PitchingStats
        <$> v .: "summary"
        <*> v .: "gamesPlayed"
        <*> v .: "gamesStarted"
        <*> v .: "flyOuts"
        <*> v .: "groundOuts"
        <*> v .: "airOuts"
        <*> v .: "runs"
        <*> v .: "doubles"
        <*> v .: "triples"
        <*> v .: "homeRuns"
        <*> v .: "strikeOuts"
        <*> v .: "baseOnBalls"
        <*> v .: "intentionalWalks"
        <*> v .: "hits"
        <*> v .: "hitByPitch"
        <*> v .: "atBats"
        <*> v .: "caughtStealing"
        <*> v .: "stolenBases"
        <*> v .: "stolenBasePercentage"
        <*> v .: "numberOfPitches"
        <*> v .: "inningsPitched"
        <*> v .: "wins"
        <*> v .: "losses"
        <*> v .: "saves"
        <*> v .: "saveOpportunities"
        <*> v .: "holds"
        <*> v .: "blownSaves"
        <*> v .: "earnedRuns"
        <*> v .: "battersFaced"
        <*> v .: "outs"
        <*> v .: "gamesPitched"
        <*> v .: "completeGames"
        <*> v .: "shutouts"
        <*> v .: "pitchesThrown"
        <*> v .: "balls"
        <*> v .: "strikes"
        <*> v .: "strikePercentage"
        <*> v .: "hitBatsmen"
        <*> v .: "balks"
        <*> v .: "wildPitches"
        <*> v .: "pickoffs"
        <*> v .: "rbi"
        <*> v .: "gamesFinished"
        <*> v .: "runsScoredPer9"
        <*> v .: "homeRunsPer9"
        <*> v .: "inheritedRunners"
        <*> v .: "inheritedRunnersScored"
        <*> v .: "catchersInterference"
        <*> v .: "sacBunts"
        <*> v .: "sacFlies"
        <*> v .: "passedBall"

-- JSON instances
instance FromJSON GameData where
    parseJSON = withObject "GameData" $ \v ->
        GameData <$> v .: "teams"

sampleJSON :: ByteString
sampleJSON = "{ \"teams\": { \"away\": { \"players\": {} }, \"home\": { \"players\": {} } } }"

main :: IO ()
main = do
    jsonData <- B.readFile "testFiles/716896_boxscore.json"
    let parsedResult = eitherDecodeStrict jsonData :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
    handPicked <- B.readFile "testFiles/shortened.json"
    let parsedResult = eitherDecodeStrict handPicked :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData