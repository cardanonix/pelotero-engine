{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Utility where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (
    FromJSON (parseJSON),
    ToJSON (toJSON),
    eitherDecodeStrict, decode, encode, withObject, (.:)
 )
import Network.HTTP.Simple (
    getResponseBody,
    httpBS,
    parseRequest_
 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
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
import Data.Aeson (FromJSON (..), Result (Success), ToJSON (..), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import Data.List ( find, delete, nub, (\\), delete )
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (foldl', forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShow, traceShowM)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (FromJSON (..), Result (Success), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))

import GHC.Generics (Generic )
import Data.Text (Text)

import System.Random (randomR, newStdGen, StdGen)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON, encode, parseJSON, withObject, (.:))
import qualified Data.HashMap.Strict as HM
import qualified Config as C
-- import qualified GHC.Generics as R
import qualified Input as I
import OfficialRoster as O
import qualified OfficialRoster as O
import qualified Points as P
import qualified Roster as R
import qualified Ranking as PR

-- Helper function to remove an element at a specific index
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = let (left, x:right) = splitAt n xs in (x, left ++ right)


-- pure function to generate a random number (and a new generator)
randomIntGen :: (Int, Int) -> StdGen -> (Int, StdGen)
randomIntGen range gen = randomR range gen

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

-- Edge Cases Handling
-- monadic error handling for fetching and decoding
withEither :: IO (Either String a) -> (a -> IO ()) -> IO ()
withEither action successHandler = do
    result <- action
    case result of
        Left err -> putStrLn err
        Right dataPacket -> successHandler dataPacket

readJson :: (FromJSON a) => FilePath -> IO (Either String a)
readJson filePath = eitherDecodeStrict <$> B.readFile filePath

writeJson :: ToJSON a => FilePath -> a -> IO ()
writeJson filePath = BL.writeFile filePath . encode

-- Fetch and decode utility
fetchAndDecodeJSON :: (FromJSON a) => String -> IO (Either String a)
fetchAndDecodeJSON url = do
    response <- httpBS (parseRequest_ url)
    return $ eitherDecodeStrict $ getResponseBody response

computeChecksum :: BL.ByteString -> Text
computeChecksum bs = T.pack . show . hashWith SHA256 $ BL.toStrict bs

getCurrentDate :: IO Text
getCurrentDate = T.pack . formatTime defaultTimeLocale "%Y_%m_%d_%H_%M" <$> getCurrentTime

getCurrentFormattedTime :: IO String
getCurrentFormattedTime = do
    currentTime <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" currentTime
    return formattedTime
