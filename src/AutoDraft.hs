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


-- You will need to implement logic to evenly distribute players according to their position and rankings
distributePlayers :: [(O.OfficialPlayer, Int)] -> [(O.OfficialPlayer, Int)] -> R.Roster -> R.Roster -> C.Configuration -> (R.Roster, R.Roster)
distributePlayers rankedPlayers1 rankedPlayers2 roster1 roster2 =
  -- Implement the distribution logic here, using the player's position and rank to assign them to each roster
  -- This is a placeholder; actual implementation will depend on your specific requirements
  (roster1, roster2)

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
  _ -> roster -- we should handle unexpected positions or add more detailed logic

-- Main Function
main :: IO ()
main = do
    -- Load and parse JSON data
    rankingData1 <- readJson "appData/head2head/team001_rankings.json" :: IO (Maybe RankingData)
    rankingData2 <- readJson "appData/head2head/team002_rankings.json" :: IO (Maybe RankingData)
    officialRoster <- readJson "appData/rosters/activePlayers.json" :: IO (Maybe OfficialRoster)

    case (rankingData1, rankingData2, officialRoster) of
        (Just rd1, Just rd2, Just or) -> do
            let roster1 = draftPlayers (rankings rd1) (people or)
            let roster2 = draftPlayers (rankings rd2) (people or)
            writeJson "appData/draftedroster001.json" roster1
            writeJson "appData/draftedroster002.json" roster2
        _ -> putStrLn "Failed to load data"


-- main :: IO ()
-- main = do
--     let rankingOne = "appData/head2head/team001_rankings.json"
--     let rankingTwo = "appData/head2head/team002_rankings.json"
--         {- 
--                 data RankingData = RankingData
--                     { teamId        :: Text
--                     , dataChecksum  :: Text
--                     , lastUpdated   :: UTCTime
--                     , rankings      :: [PlayerRanking]  -- relevant data found here
--                     } deriving (Show, Eq, Generic)

--                 data PlayerRanking = PlayerRanking
--                     { playerId :: Int
--                     , rank     :: Int
--                     } deriving (Show, Eq, Generic) 
--         -}
--     let activeRoster = "appData/rosters/activePlayers.json"
--         {- 
--             data OfficialRoster = OfficialRoster
--                 { people :: [OfficialPlayer]
--                 , dataPulled :: Text
--                 , checksum :: Text
--                 }
--                 deriving (Show)

--             data OfficialPlayer = OfficialPlayer
--                 { playerId :: Int               -- relevant data here
--                 , useName :: Text
--                 , useLastName :: Text
--                 , nameSlug :: Text
--                 , currentTeam :: Int
--                 , primaryPosition :: Text       -- relevant data here
--                 , batSide :: Text
--                 , pitchHand :: Text
--                 , active :: Bool                -- relevant data here
--                 }
--                 deriving (Show, Eq) 
--         -}
--     draftConfig <- readJson "testFiles/prototype_config/config.json" :: IO (Either String C.Configuration)
--         {-       
--                 data DraftParameters = DraftParameters
--                     { autoDraft :: Bool
--                     , autoDraft_UTC :: Text
--                     , draft_limits :: DraftRoster -- relevant date found here
--                     }
--                     deriving (Show, Eq)   

--                 data DraftRoster = DraftRoster
--                     { dr_catcher :: Int
--                     , dr_first :: Int
--                     , dr_second :: Int
--                     , dr_third :: Int
--                     , dr_shortstop :: Int
--                     , dr_outfield :: Int
--                     , dr_utility :: Int
--                     , dr_s_pitcher :: Int
--                     , dr_r_pitcher :: Int
--                     }
--                     deriving (Show, Eq)
--         -}
--     -- using the relevant data, we then automatically fill the rosters with the players preventing cases of duplicate players on the rosters
--     draftedRosterOne :: IO (Either String R.Roster) -> writeJson "appData/draftedroster001.json" 
--     draftedRosterTwo :: IO (Either String R.Roster) -> writeJson "appData/draftedroster002.json" 
--         {- 
--                 -- LgManager ADT
--                 data LgManager = LgManager
--                     { status :: Text
--                     , commissioner :: Text
--                     , teamId :: Text
--                     , leagueID :: Text
--                     , current_lineup :: CurrentLineup
--                     , roster :: Roster
--                     }
--                     deriving (Show, Eq)

--                 data CurrentLineup = CurrentLineup
--                     { cC :: Text
--                     , b1C :: Text
--                     , b2C :: Text
--                     , b3C :: Text
--                     , ssC :: Text
--                     , ofC :: [Text]
--                     , uC :: Text
--                     , spC :: [Text]
--                     , rpC :: [Text]
--                     }
--                     deriving (Show, Eq)

--                 data Roster = Roster
--                     { cR :: [Text]
--                     , b1R :: [Text]
--                     , b2R :: [Text]
--                     , b3R :: [Text]
--                     , ssR :: [Text]
--                     , ofR :: [Text]
--                     , uR :: [Text]
--                     , spR :: [Text]
--                     , rpR :: [Text]
--                     }
--                     deriving (Show, Eq) 
--         -}

