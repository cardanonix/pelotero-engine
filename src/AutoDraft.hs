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
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShowM)
import System.Environment (getArgs)


main :: IO ()
main = do
    let rankingOne = "appData/head2head/team001_rankings.json"
    let rankingTwo = "appData/head2head/team002_rankings.json"
    let activeRoster = "appData/rosters/activePlayers.json"
    -- load roster limits   