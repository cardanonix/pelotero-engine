{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified OfficialRoster as O

-- This function reads the JSON file and decodes it into your OfficialRoster data type.
decodeOfficialRosterFromFile :: FilePath -> IO (Either String O.OfficialRoster)
decodeOfficialRosterFromFile path = eitherDecode <$> BL.readFile path

-- This is your main function that will read, decode, and print the result.
main :: IO ()
main = do
    result <- decodeOfficialRosterFromFile "testFiles/appData/rosters/activePlayers.json"
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right roster -> print roster
