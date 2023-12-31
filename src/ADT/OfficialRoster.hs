{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module OfficialRoster where

import Control.Monad (filterM, forM)
import Data.Aeson (FromJSON (..), Result (Success), ToJSON (..), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..), Value (..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (nub, (\\))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Debug.Trace (traceShow, traceShowM)

import qualified Data.Aeson as A
import Data.Foldable (foldl', forM_)

-- import qualified Data.Map.Strict as HM
-- import qualified Data.Map.Strict as M

import qualified Data.Aeson.KeyMap as AK
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified GHC.Generics as R

instance FromJSON OfficialRoster where
    parseJSON :: Value -> Parser OfficialRoster
    parseJSON = withObject "OfficialRoster" $ \v -> do
        -- Extracting the meta information
        dataPulled <- v .: "dataPulled"
        checksum <- v .: "checksum"

        -- Parsing players
        peopleObject <- v .: "officialPlayers"
        players <- forM (AK.keys peopleObject) $ \playerKey -> do
            case AK.lookup playerKey peopleObject of
                Nothing -> fail $ "Key not found: " ++ (show playerKey)
                Just playerValue -> parseJSON playerValue

        return $ OfficialRoster players dataPulled checksum

instance FromJSON OfficialPlayer where
    parseJSON :: Value -> Parser OfficialPlayer
    parseJSON = withObject "OfficialPlayer" $ \v -> do
        playerId <- v .: "playerId"
        useName <- v .: "useName"
        useLastName <- v .: "useLastName"
        nameSlug <- v .: "nameSlug"
        currentTeam <- v .: "currentTeam"
        primaryPosition <- v .: "primaryPosition"
        batSide <- v .: "batSide"
        pitchHand <- v .: "pitchHand"
        active <- v .: "active"
        return $ OfficialPlayer playerId useName useLastName nameSlug currentTeam primaryPosition batSide pitchHand active

data OfficialRoster = OfficialRoster
    { people :: [OfficialPlayer]
    , dataPulled :: Text
    , checksum :: Text
    }
    deriving (Show)

data OfficialPlayer = OfficialPlayer
    { playerId :: Int
    , useName :: Text
    , useLastName :: Text
    , nameSlug :: Text
    , currentTeam :: Int
    , primaryPosition :: Text
    , batSide :: Text
    , pitchHand :: Text
    , active :: Bool
    }
    deriving (Show, Eq)
