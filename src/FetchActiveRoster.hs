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
import Input
import Middle
import Scraper
import System.Environment (getArgs)

main :: IO ()
main = do
    let rosterPath = "appData/rosters/activePlayers.json"
    activeRoster <- fetchActiveRoster 2023 -- assuming 2023 is the season you're fetching for
    case activeRoster of
        Left err -> putStrLn $ "Failed to fetch active roster: " ++ err
        Right rosterData -> writeRosterToFile rosterPath rosterData
