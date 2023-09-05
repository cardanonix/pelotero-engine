{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict

data Entry = Entry
  { id :: String
  , name :: String
  , location :: String
  }
  deriving Show

newtype Entries = Entries [Entry] deriving Show

instance FromJSON Entries where
  parseJSON x = Entries <$> (parseJSON x >>= mapM parseEntry . toList)

parseEntry :: (String, Value) -> Parser Entry
parseEntry (i, v) =
  withObject "entry body" (\ o ->
    Entry i <$> o .: "name" <*> o .: "location")
    v

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "testFiles/testfile2.json" :: IO (Either String Entries)
  case result of
    Left err -> putStrLn $ "Error decoding file: " ++ err
    Right entries -> print entries