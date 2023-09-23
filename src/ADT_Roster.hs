{-# LANGUAGE OverloadedStrings, GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module ADT_Roster where

import Control.Monad (filterM)
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Result (Success), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..), withScientific, withText)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text ( Text, Text )
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Scientific (toBoundedInteger)
import ADT_Config (parseDouble, Configuration)

-- LgManager ADT
data LgManager = LgManager
  { status         :: Text
  , commissioner   :: Text
  , teamId         :: Text
  , leagueID       :: Text
  , current_lineup :: CurrentLineup
  , roster         :: Roster
  } deriving (Show, Eq)

data CurrentLineup = CurrentLineup
  { cC  :: Text
  , b1C :: Text
  , b2C  :: Text
  , b3C  :: Text
  , ssC  :: Text
  , ofC  :: [Text]
  , uC   :: Text
  , spC  :: [Text]
  , rpC  :: [Text]
  } deriving (Show, Eq)

data Roster = Roster
  { cR  :: [Text]
  , b1R :: [Text]
  , b2R :: [Text]
  , b3R :: [Text]
  , ssR :: [Text]
  , ofR :: [Text]
  , uR  :: [Text]
  , spR :: [Text]
  , rpR :: [Text]
  } deriving (Show, Eq)

-- FromJSON Instances
instance FromJSON LgManager where
  parseJSON = withObject "LgManager" $ \v ->
    LgManager <$> v .: "status"
              <*> v .: "comissioner"
              <*> v .: "teamId"
              <*> v .: "leagueID"
              <*> v .: "current_lineup"
              <*> v .: "roster"

instance FromJSON CurrentLineup where
  parseJSON = withObject "CurrentLineup" $ \v ->
    CurrentLineup <$> v .: "C"
                  <*> v .: "1B"
                  <*> v .: "2B"
                  <*> v .: "3B"
                  <*> v .: "SS"
                  <*> v .: "OF"
                  <*> v .: "U"
                  <*> v .: "SP"
                  <*> v .: "RP"

instance FromJSON Roster where
  parseJSON = withObject "Roster" $ \v ->
    Roster <$> v .: "C"
           <*> v .: "1B"
           <*> v .: "2B"
           <*> v .: "3B"
           <*> v .: "SS"
           <*> v .: "OF"
           <*> v .: "U"
           <*> v .: "SP"
           <*> v .: "RP"