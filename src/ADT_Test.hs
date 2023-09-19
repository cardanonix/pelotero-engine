module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Data.ByteString ( ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Aeson (decode, Result(Success), FromJSON(..), Value, (.:), (.:?), (.!=), fromJSON, withObject, eitherDecodeStrict)
import Data.Aeson.Types (Parser, Result(..))
import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShowM)
import ADT_Input_trace ( GameData )

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