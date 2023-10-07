module Main (main) where

import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), Result (Success), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShowM)
import Input_trace (GameData)

main :: IO ()
main = do
    jsonData <- B.readFile "testFiles/mlb/boxscore_716896.json"
    let parsedResult = eitherDecodeStrict jsonData :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
    handPicked <- B.readFile "testFiles/shortened.json"
    let parsedResult = eitherDecodeStrict handPicked :: Either String GameData
    case parsedResult of
        Left err -> putStrLn $ "Failed to parse JSON: " ++ err
        Right gameData -> print gameData
