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
  return $ zip draftOrderTeams [1 ..]