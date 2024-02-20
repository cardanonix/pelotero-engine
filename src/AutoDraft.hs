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
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (sortOn, find)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShowM)
import System.Environment (getArgs)

import qualified Config as C
import qualified OfficialRoster as O
import qualified Roster as R

-- Utility Functions
readJson :: FromJSON a => FilePath -> IO (Maybe a)
readJson filePath = decode <$> BL.readFile filePath

writeJson :: ToJSON a => FilePath -> a -> IO ()
writeJson filePath = BL.writeFile filePath . encode

-- Main Drafting Logic
draftPlayers :: RankingData -> RankingData -> [OfficialPlayer] -> C.Configuration -> (R.Roster, R.Roster)
draftPlayers rankingData1 rankingData2 officialPlayers config = (finalRoster1, finalRoster2)
  where
    rankedPlayers1 = rankAndFilterPlayers (R.rankings rankingData1) officialPlayers
    rankedPlayers2 = rankAndFilterPlayers (R.rankings rankingData2) officialPlayers
    -- Example placeholder for distributing players; Implement according to your draft logic and config
    (finalRoster1, finalRoster2) = distributePlayers rankedPlayers1 rankedPlayers2 R.emptyRoster R.emptyRoster config


rankAndFilterPlayers :: [PlayerRanking] -> [OfficialPlayer] -> [(OfficialPlayer, Int)]
rankAndFilterPlayers playerRankings officialPlayers = mapMaybe mapRank officialPlayers
  where
    mapRank player = do
      ranking <- find (\r -> playerId r == opPlayerId player) playerRankings
      return (player, rank ranking)


distributePlayers :: [(O.OfficialPlayer, Int)] -> [(O.OfficialPlayer, Int)] -> R.Roster -> R.Roster -> C.Configuration -> (R.Roster, R.Roster)
distributePlayers rankedPlayers1 rankedPlayers2 initialRoster1 initialRoster2 config =
    foldl (distributePlayer config) (initialRoster1, initialRoster2, True) (mergePlayers rankedPlayers1 rankedPlayers2)


emptyRoster :: Roster
emptyRoster = Roster [] [] [] [] [] [] [] [] []

addToRoster :: O.OfficialPlayer -> R.Roster -> R.Roster
addToRoster player roster = case O.primaryPosition player of
  "C" -> roster { cR = useName player : cR roster }
  "1B" -> roster { b1R = useName player : b1R roster }
  "2B" -> roster { b2R = useName player : b2R roster }
  "3B" -> roster { b3R = useName player : b3R roster }
  "SS" -> roster { ssR = useName player : ssR roster }
  "OF" -> roster { ofR = useName player : ofR roster }
  "U" -> roster { uR = useName player : uR roster }
  "SP" -> roster { spR = useName player : spR roster }
  "RP" -> roster { rpR = useName player : rpR roster }
  _ -> roster -- TODO: add more detailed logic

addToRoster :: C.Configuration -> O.OfficialPlayer -> R.Roster -> R.Roster
addToRoster cfg player roster =
    let playerIdStr = show $ O.opPlayerId player  -- Convert player ID to String
    in if not (playerIdExists playerIdStr roster)
       then addIfUnderLimit (O.primaryPosition player) playerIdStr cfg roster
       else roster

playerIdExists :: String -> R.Roster -> Bool
playerIdExists pid roster =
    pid `elem` concat [R.cR roster, R.b1R roster, R.b2R roster, R.b3R roster, R.ssR roster, R.ofR roster, R.uR roster, R.spR roster, R.rpR roster]

addIfUnderLimit :: Text -> String -> C.Configuration -> R.Roster -> R.Roster
addIfUnderLimit position playerId cfg roster = 
    let posLimit = lookupLimit position cfg
        currentCount = countPlayers position roster
    in if currentCount < posLimit
       then updateRoster position playerId roster
       else roster

addPlayer :: Text -> Text -> C.Configuration -> R.Roster -> R.Roster
addPlayer posName playerName cfg roster = 
    let posLimit = lookupLimit posName (C.draftLimits cfg)
        currentCount = countPlayers posName roster 
    in if currentCount < posLimit
       then updateRoster posName playerName roster
       else roster

lookupLimit :: Text -> C.Configuration -> Int
lookupLimit posName cfg =
    let draftLimits = draft_limits (draft_parameters cfg)
    in case posName of
        "catcher"   -> dr_catcher draftLimits
        "first"     -> dr_first draftLimits
        "second"    -> dr_second draftLimits
        "third"     -> dr_third draftLimits
        "shortstop" -> dr_shortstop draftLimits
        "outfield"  -> dr_outfield draftLimits
        "utility"   -> dr_utility draftLimits
        "s_pitcher" -> dr_s_pitcher draftLimits
        "r_pitcher" -> dr_r_pitcher draftLimits
        _           -> 0  -- Default case for unhandled positions

-- Placeholder for countPlayers function
countPlayers :: Text -> R.Roster -> Int
countPlayers posName roster =
    case posName of
        "catcher"   -> length (R.cR roster)
        "first"     -> length (R.b1R roster)
        "second"    -> length (R.b2R roster)
        "third"     -> length (R.b3R roster)
        "shortstop" -> length (R.ssR roster)
        "outfield"  -> length (R.ofR roster)
        "utility"   -> length (R.uR roster)
        "s_pitcher" -> length (R.spR roster)
        "r_pitcher" -> length (R.rpR roster)
        _           -> 0

-- Placeholder for updateRoster function
updateRoster :: Text -> String -> R.Roster -> R.Roster
updateRoster posName playerId roster =
    case posName of
        "C"  -> roster {R.cR = addUnique playerId (R.cR roster)}
        "1B" -> roster {R.b1R = addUnique playerId (R.b1R roster)}
        "2B" -> roster {R.b2R = addUnique playerId (R.b2R roster)}
        "3B" -> roster {R.b3R = addUnique playerId (R.b3R roster)}
        "SS" -> roster {R.ssR = addUnique playerId (R.ssR roster)}
        "OF" -> roster {R.ofR = addUnique playerId (R.ofR roster)}
        "U"  -> roster {R.uR = addUnique playerId (R.uR roster)}
        "SP" -> roster {R.spR = addUnique playerId (R.spR roster)}
        "RP" -> roster {R.rpR = addUnique playerId (R.rpR roster)}
        _    -> roster  -- No update for unhandled positions

addUnique :: String -> [String] -> [String]
addUnique pid pids = if pid `elem` pids then pids else pid : pids

-- Main Function
main :: IO ()
main = do
    rankingData1 <- readJson "appData/head2head/team001_rankings.json" :: IO (Maybe RankingData)
    rankingData2 <- readJson "appData/head2head/team002_rankings.json" :: IO (Maybe RankingData)
    officialRoster <- readJson "appData/rosters/activePlayers.json" :: IO (Maybe O.OfficialRoster)

    case (rankingData1, rankingData2, officialRoster) of
        (Just rd1, Just rd2, Just or) -> do
            let roster1 = draftPlayers (rankings rd1) (people or)
            let roster2 = draftPlayers (rankings rd2) (people or)
            writeJson "appData/draftedroster001.json" roster1
            writeJson "appData/draftedroster002.json" roster2
        _ -> putStrLn "Failed to load data"

{- 
data RankingData = RankingData
    { teamId        :: Text
    , dataChecksum  :: Text
    , lastUpdated   :: UTCTime
    , rankings      :: [PlayerRanking]  -- relevant data found here
    } deriving (Show, Eq, Generic)

data PlayerRanking = PlayerRanking
    { playerId :: Int
    , rank     :: Int
    } deriving (Show, Eq, Generic) 

data OfficialRoster = OfficialRoster
    { people :: [OfficialPlayer]
    , dataPulled :: Text
    , checksum :: Text
    }
    deriving (Show)

data OfficialPlayer = OfficialPlayer
    { playerId :: Int               -- relevant data here
    , useName :: Text
    , useLastName :: Text
    , nameSlug :: Text
    , currentTeam :: Int
    , primaryPosition :: Text       -- relevant data here
    , batSide :: Text
    , pitchHand :: Text
    , active :: Bool                -- relevant data here
    }
    deriving (Show, Eq) 
    
data DraftParameters = DraftParameters
    { autoDraft :: Bool
    , autoDraft_UTC :: Text
    , draft_limits :: DraftRoster -- relevant date found here
    }
    deriving (Show, Eq)   

data DraftRoster = DraftRoster
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

data LgManager = LgManager
    { status :: Text
    , commissioner :: Text
    , teamId :: Text
    , leagueID :: Text
    , current_lineup :: CurrentLineup
    , roster :: Roster
    }
    deriving (Show, Eq)

data CurrentLineup = CurrentLineup
    { cC :: Text
    , b1C :: Text
    , b2C :: Text
    , b3C :: Text
    , ssC :: Text
    , ofC :: [Text]
    , uC :: Text
    , spC :: [Text]
    , rpC :: [Text]
    }
    deriving (Show, Eq)

data Roster = Roster
    { cR :: [Text]
    , b1R :: [Text]
    , b2R :: [Text]
    , b3R :: [Text]
    , ssR :: [Text]
    , ofR :: [Text]
    , uR :: [Text]
    , spR :: [Text]
    , rpR :: [Text]
    }
    deriving (Show, Eq) 

 -}