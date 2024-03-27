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
import qualified OfficialRoster as O

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
    { cC  :: [O.PlayerID] 
    , b1C :: [O.PlayerID]
    , b2C :: [O.PlayerID]
    , b3C :: [O.PlayerID]
    , ssC :: [O.PlayerID]
    , ofC :: [O.PlayerID]
    , uC  :: [O.PlayerID] 
    , spC :: [O.PlayerID]
    , rpC :: [O.PlayerID]
    }
    deriving (Show, Eq)

data Roster = Roster
    { cR  :: [O.PlayerID]
    , b1R :: [O.PlayerID]
    , b2R :: [O.PlayerID]
    , b3R :: [O.PlayerID]
    , ssR :: [O.PlayerID]
    , ofR :: [O.PlayerID]
    , uR  :: [O.PlayerID]
    , spR :: [O.PlayerID]
    , rpR :: [O.PlayerID]
    }
    deriving (Show, Eq)

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
