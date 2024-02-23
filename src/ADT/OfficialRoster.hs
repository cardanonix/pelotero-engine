{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OfficialRoster where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text
import GHC.Generics

data OfficialRoster = OfficialRoster
    { people :: [OfficialPlayer]
    , dataPulled :: Text
    , checksum :: Text
    } deriving (Show, Generic)

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

instance FromJSON OfficialRoster where
    parseJSON = withObject "OfficialRoster" $ \v -> do
        checksum <- v .: "checksum"
        dataPulled <- v .: "dataPulled"
        playersObj <- v .: "officialPlayers" :: Parser (HashMap Text Value)
        let playersList = HM.elems playersObj
        people <- mapM parseJSON playersList
        return OfficialRoster{people = people, dataPulled = dataPulled, checksum = checksum}

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
