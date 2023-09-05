{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict

data Entry = Entry
  { id :: String
  , name :: String
  , location :: String
  }
  deriving Show

instance FromJSON [Entry] where
  parseJSON x =
    parseJSON x >>= mapM parseEntry . toList

parseEntry :: (String, Value) -> Parser Entry
parseEntry (i, v) =
  withObject "entry body" (\ o ->
    Entry i <$> o .: "name" <*> o .: "location")
    v