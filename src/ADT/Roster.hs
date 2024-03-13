{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Roster where

import Config (Configuration, parseDouble)
import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), ToJSON (..), Result (Success), Value, (.=), object, decode, eitherDecodeStrict, fromJSON, toJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..), withScientific, withText)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

-- LgManager ADT
data LgManager = LgManager
    { status :: Text
    , commissioner :: Text
    , teamId :: Text
    , leagueID :: Text
    , current_lineup :: CurrentLineup
    , roster :: Roster
    }
    deriving (Show, Eq)

data CurrentLineup = CurrentLineup
    { cC :: Text
    , b1C :: Text
    , b2C :: Text
    , b3C :: Text
    , ssC :: Text
    , ofC :: [Text]
    , uC :: Text
    , spC :: [Text]
    , rpC :: [Text]
    }
    deriving (Show, Eq)

data Roster = Roster
    { cR :: [Text]
    , b1R :: [Text]
    , b2R :: [Text]
    , b3R :: [Text]
    , ssR :: [Text]
    , ofR :: [Text]
    , uR :: [Text]
    , spR :: [Text]
    , rpR :: [Text]
    }
    deriving (Show, Eq)

-- Creates an empty roster with no players
makeEmptyRoster :: Roster
makeEmptyRoster = Roster [] [] [] [] [] [] [] [] []

-- FromJSON Instances
instance FromJSON LgManager where
    parseJSON :: Value -> Parser LgManager
    parseJSON = withObject "LgManager" $ \v ->
        LgManager
            <$> v .: "status"
            <*> v .: "comissioner"
            <*> v .: "teamId"
            <*> v .: "leagueID"
            <*> v .: "current_lineup"
            <*> v .: "roster"

instance FromJSON CurrentLineup where
    parseJSON :: Value -> Parser CurrentLineup
    parseJSON = withObject "CurrentLineup" $ \v ->
        CurrentLineup
            <$> v .: "C"
            <*> v .: "1B"
            <*> v .: "2B"
            <*> v .: "3B"
            <*> v .: "SS"
            <*> v .: "OF"
            <*> v .: "U"
            <*> v .: "SP"
            <*> v .: "RP"

instance FromJSON Roster where
    parseJSON :: Value -> Parser Roster
    parseJSON = withObject "Roster" $ \v ->
        Roster
            <$> v .: "C"
            <*> v .: "1B"
            <*> v .: "2B"
            <*> v .: "3B"
            <*> v .: "SS"
            <*> v .: "OF"
            <*> v .: "U"
            <*> v .: "SP"
            <*> v .: "RP"

-- ToJSON Instances
instance ToJSON LgManager where
    toJSON (LgManager status commissioner teamId leagueID currentLineup roster) =
        object
            [ "status" .= status
            , "commissioner" .= commissioner
            , "teamId" .= teamId
            , "leagueID" .= leagueID
            , "current_lineup" .= currentLineup
            , "roster" .= roster
            ]

instance ToJSON CurrentLineup where
    toJSON (CurrentLineup cC b1C b2C b3C ssC ofC uC spC rpC) =
        object
            [ "C" .= cC
            , "1B" .= b1C
            , "2B" .= b2C
            , "3B" .= b3C
            , "SS" .= ssC
            , "OF" .= ofC
            , "U" .= uC
            , "SP" .= spC
            , "RP" .= rpC
            ]

instance ToJSON Roster where
    toJSON (Roster cR b1R b2R b3R ssR ofR uR spR rpR) =
        object
            [ "C" .= cR
            , "1B" .= b1R
            , "2B" .= b2R
            , "3B" .= b3R
            , "SS" .= ssR
            , "OF" .= ofR
            , "U" .= uR
            , "SP" .= spR
            , "RP" .= rpR
            ]
