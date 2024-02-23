{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OfficialRoster where

import Data.Aeson (FromJSON, ToJSON, withObject, (.:), object, (.=))
import Data.Aeson.KeyMap (toList)
import Data.Text
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (..), Result (Success), ToJSON (..), Value, decode, eitherDecodeStrict, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Result (..), Value (..))
import Data.HashMap.Strict

data OfficialRoster = OfficialRoster
    { people :: [OfficialPlayer]
    , dataPulled :: Text
    , checksum :: Text
    } deriving (Show)

-- data OfficialRoster = OfficialRoster
--     { people :: [OfficialPlayer]
--     , dataPulled :: Text
--     , checksum :: Text
--     }
--     deriving (Show)

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
    } deriving (Show, Eq, Generic)

-- data OfficialPlayer = OfficialPlayer
--     { playerId :: Int
--     , useName :: Text
--     , useLastName :: Text
--     , nameSlug :: Text
--     , currentTeam :: Int
--     , primaryPosition :: Text
--     , batSide :: Text
--     , pitchHand :: Text
--     , active :: Bool
--     }
--     deriving (Show, Eq)

instance FromJSON OfficialRoster where
    parseJSON = withObject "OfficialRoster" $ \v -> do
        checksum <- v .: "checksum"
        dataPulled <- v .: "dataPulled"
        playersObj <- v .: "officialPlayers"
        let playersList = elems playersObj
        return OfficialRoster{people=playersList, ..}

-- instance FromJSON OfficialRoster where
--     parseJSON :: Value -> Parser OfficialRoster
--     parseJSON = withObject "OfficialRoster" $ \v -> do
--         -- Extracting the meta information
--         dataPulled <- v .: "dataPulled"
--         checksum <- v .: "checksum"

--         -- Parsing players
--         peopleObject <- v .: "officialPlayers"
--         players <- forM (AK.keys peopleObject) $ \playerKey -> do
--             case AK.lookup playerKey peopleObject of
--                 Nothing -> fail $ "Key not found: " ++ show playerKey
--                 Just playerValue -> parseJSON playerValue

--         return $ OfficialRoster players dataPulled checksum

instance FromJSON OfficialPlayer where
    parseJSON = withObject "OfficialPlayer" $ \v -> do
        playerId <- v .: "id"
        useName <- v .: "useName"
        useLastName <- v .: "useLastName"
        nameSlug <- v .: "nameSlug"
        currentTeam <- v .: "currentTeam"
        primaryPosition <- v .: "primaryPosition"
        batSide <- v .: "batSide"
        pitchHand <- v .: "pitchHand"
        active <- v .: "active"
        return OfficialPlayer{..}

-- instance FromJSON OfficialPlayer where
--     parseJSON :: Value -> Parser OfficialPlayer
--     parseJSON = withObject "OfficialPlayer" $ \v -> do
--         playerId <- v .: "playerId"
--         useName <- v .: "useName"
--         useLastName <- v .: "useLastName"
--         nameSlug <- v .: "nameSlug"
--         currentTeam <- v .: "currentTeam"
--         primaryPosition <- v .: "primaryPosition"
--         batSide <- v .: "batSide"
--         pitchHand <- v .: "pitchHand"
--         active <- v .: "active"
--         return $ OfficialPlayer playerId useName useLastName nameSlug currentTeam primaryPosition batSide pitchHand active
