{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}

module Utility where

import Crypto.Hash ( SHA256(SHA256), hashWith )
import Crypto.Random (getRandomBytes)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson
    ( FromJSON(parseJSON),
      ToJSON(toJSON),
      eitherDecodeStrict,
      decode,
      encode,
      withObject,
      (.:),
      FromJSON(..),
      Result(Success),
      ToJSON(..),
      Value,
      decode,
      eitherDecodeStrict,
      fromJSON,
      withObject,
      (.!=),
      (.:),
      (.:?),
      FromJSON(..),
      Result(Success),
      Value,
      decode,
      eitherDecodeStrict,
      fromJSON,
      withObject,
      (.!=),
      (.:),
      (.:?),
      FromJSON,
      ToJSON,
      encode,
      parseJSON,
      withObject,
      (.:),
      (.=) )
import Network.HTTP.Simple (
    getResponseBody,
    httpBS,
    parseRequest_
 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Time
    ( Day,
      addDays,
      defaultTimeLocale,
      diffDays,
      formatTime,
      parseTimeOrError,
      Day,
      addDays,
      defaultTimeLocale,
      diffDays,
      formatTime,
      parseTimeOrError,
      parseTimeM )
import Data.Time.Clock
import Data.Time.Clock.POSIX ()
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Control.Monad ( filterM, forM, filterM, forM, forM_ )
import Data.Aeson.Types (Parser, Result (..), Pair)
import Data.ByteString ( ByteString, ByteString )
import Data.List ( find, delete, nub, (\\), delete )
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (foldl', forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as Text
import Debug.Trace (traceShow, traceShowM)

import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Exception (catch, IOException)

import GHC.Generics (Generic )

import System.Random ( randomR, newStdGen, StdGen )
import qualified Data.HashMap.Strict as HM
import qualified Config as C
import qualified Input as I
import qualified OfficialRoster as O
import qualified Points as P
import qualified Roster as R
import qualified PlayerRanking as PR
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.Text.Encoding as T


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
        "P"  -> "pitcher"
        "C"  -> "catcher"
        "1B" -> "first"
        "2B" -> "second"
        "3B" -> "third"
        "SS" -> "shortstop"
        "LF" -> "outfield"
        "CF" -> "outfield"
        "RF" -> "outfield"
        "DH" -> "utility"
        _    -> "Unknown"

-- generates a list of LgManager for each team ID provided in lgMembers.
mkLgManagers :: C.Configuration -> [R.LgManager]
mkLgManagers config = map (\teamId -> mkSingleLgManager config (C.commissioner config) (C.leagueID config) teamId) (C.lgMembers config)

-- helper function creates a single LgManager, given the teamId and other details.
mkSingleLgManager :: C.Configuration -> Text -> Text -> Text -> R.LgManager
mkSingleLgManager config commissioner leagueID teamId = R.LgManager
  { R.status = "active"
  , R.commissioner = commissioner
  , R.teamId = teamId
  , R.leagueID = leagueID
  , R.current_lineup = mkEmptyLineup
  , R.roster = mkEmptyRoster
  }

-- | Generates a list of LgManager for each team ID provided in the filtered list of lgMembers.
mkLgManagersWithFilter :: C.Configuration -> [T.Text] -> [R.LgManager]
mkLgManagersWithFilter config validTeamIds =
    map (\teamId -> mkSingleLgManager config (C.commissioner config) (C.leagueID config) teamId) validTeamIds


-- Creates an empty roster with no players
mkEmptyRoster :: R.Roster
mkEmptyRoster = R.Roster [] [] [] [] [] [] [] [] []

-- Creates an empty lineup with no players
mkEmptyLineup :: R.CurrentLineup
mkEmptyLineup = R.CurrentLineup [] [] [] [] [] [] [] [] []


createLgManager :: C.Configuration -> T.Text -> R.CurrentLineup -> R.Roster -> R.LgManager
createLgManager config teamId
  = R.LgManager
      (C.status config) (C.commissioner config) teamId
      (C.leagueID config)

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
  randomBytes <- generateRandomBytes 32  -- Generating 32 bytes for the SHA256 input
  let hash = hashWith SHA256 randomBytes  -- Hashing the random bytes with SHA256
  return $ T.decodeUtf8 $ convertToBase Base16 hash  -- Convert the hash to Text (hexadecimal representation)

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
removeAt n xs = let (left, x:right) = splitAt n xs in (x, left ++ right)

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

writeJson :: ToJSON a => FilePath -> a -> IO ()
writeJson filePath = BL.writeFile filePath . encode

-- Reads and parses all ranking JSON files into data structures
-- Verify if a player is in the official roster
isPlayerInOfficialRoster :: O.PlayerID -> [O.OfficialPlayer] -> Bool
isPlayerInOfficialRoster playerId
  = any (\ p -> O.playerId p == playerId)

-- positionTextToRosterPosition :: T.Text -> R.Roster -> O.OfficialPlayer -> R.Roster
-- positionTextToRosterPosition position roster player =
--   -- Implementation depends on how you're managing roster updates
--   undefined

-- Filter function to retain only those rankings where the teamId matches any lgMember
filterInvalidRankings :: [T.Text] -> [PR.RankingData] -> [PR.RankingData]
filterInvalidRankings lgMembers rankings =
  filter (\ranking -> PR.teamId ranking `elem` lgMembers) rankings

-- Utility function to filter out teams without rankings
filterValidTeams :: [T.Text] -> [PR.RankingData] -> [T.Text]
filterValidTeams lgMembers rankings =
  let validTeamIds = map PR.teamId rankings
  in filter (`elem` validTeamIds) lgMembers

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

