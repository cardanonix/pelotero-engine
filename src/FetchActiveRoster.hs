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

import Data.Aeson.Types ( Parser, Result(..) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 ( pack)
import System.Environment ( getArgs)
import Data.Aeson
import Data.Csv (ToNamedRecord, namedRecord, (.=))
import Control.Monad (mzero)
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Csv as Csv
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Debug.Trace (traceShowM)
import Input
import Middle
import Scraper
import Conversion


import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
<<<<<<< HEAD
    args <- getArgs
    case args of
        (yearStr:_) -> case readMaybe yearStr of
            Just year -> processYear year
            Nothing -> putStrLn "Error: First argument must be a year (integer)."
        _ -> putStrLn "Error: Please provide a year as the first argument."

processYear :: Int -> IO ()
processYear year = do
    let rosterPath = "appData/rosters/" ++ show year ++ "_activePlayers.json"
    activeRoster <- fetchActiveRoster year -- now using the year parameter
=======
    let rosterPath = "appData/rosters/activePlayers.json"
    activeRoster <- fetchActiveRoster 2023
>>>>>>> 33bdd0f (submitting something just to test code.)
    case activeRoster of
        Left err -> putStrLn $ "Failed to fetch active roster: " ++ err
        Right rosterData -> writeRosterToFile rosterPath rosterData

    jsonData <- BL.readFile rosterPath

    -- Parse JSON data
    let decodedData = eitherDecode jsonData :: Either String PlayersFile

    case decodedData of
        Left err -> putStrLn err
        Right parsedData -> do
            -- Convert HashMap to List
            let playersList = HM.elems $ officialPlayers parsedData
            
            -- Convert to CSV
            let csvData = Csv.encodeDefaultOrderedByName playersList
            let csvPath = "appData/rosters/" ++ show year ++ "_activePlayers.csv"
            BL.writeFile csvPath csvData
