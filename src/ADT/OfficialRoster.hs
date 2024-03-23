{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module OfficialRoster where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GHC.Generics

data OfficialRoster = OfficialRoster
    { people :: [OfficialPlayer]
    , dataPulled :: T.Text
    , checksum :: T.Text
    } deriving (Show, Generic)

data OfficialPlayer = OfficialPlayer
    { playerId :: PlayerID
    , useName :: T.Text
    , useLastName :: T.Text
    , nameSlug :: T.Text
    , currentTeam :: Int
    , primaryPosition :: T.Text
    , batSide :: T.Text
    , pitchHand :: T.Text
    , active :: Bool
    } deriving (Show, Eq, Generic)

-- data OfficialPlayer = OfficialPlayer
--     { playerId :: Int
--     , useName :: T.Text
--     , useLastName :: T.Text
--     , nameSlug :: T.Text
--     , currentTeam :: Int
--     , primaryPosition :: T.Text
--     , batSide :: T.Text
--     , pitchHand :: T.Text
--     , active :: Bool
--     } deriving (Show, Eq, Generic)

newtype PlayerID = PlayerID Int deriving (Show, Eq)
newtype PlayerIDstring = PlayerIDstring T.Text deriving (Show, Eq)

-- Convert a PlayerID to Text
playerIDToText :: PlayerID -> T.Text
playerIDToText (PlayerID pid) = T.pack (show pid)

-- Parse a PlayerID from Text, safely
textToPlayerID :: T.Text -> Maybe PlayerID
textToPlayerID txt = case reads (T.unpack txt) :: [(Int, String)] of
    [(pid, "")] -> Just (PlayerID pid) -- Successful parse with no remainder
    _ -> Nothing -- Failed parse

instance FromJSON OfficialRoster where
    parseJSON :: Value -> Parser OfficialRoster
    parseJSON = withObject "OfficialRoster" $ \v -> do
        checksum <- v .: "checksum"
        dataPulled <- v .: "dataPulled"
        playersObj <- v .: "officialPlayers" :: Parser (HashMap T.Text Value)
        let playersList = HM.elems playersObj
        people <- mapM parseJSON playersList
        return OfficialRoster{people = people, dataPulled = dataPulled, checksum = checksum}

instance FromJSON OfficialPlayer where
    parseJSON = withObject "OfficialPlayer" $ \v -> do
        -- Parse the player ID, accommodating both direct integer IDs and stringified IDs.
        pidText <- v .: "id" :: Parser T.Text
        let maybePlayerId = textToPlayerID pidText
        playerId <- case maybePlayerId of
                      Just pid -> return pid
                      Nothing -> fail "Invalid player ID format"
        
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
--         playerId <- v .: "id"
--         useName <- v .: "useName"
--         useLastName <- v .: "useLastName"
--         nameSlug <- v .: "nameSlug"
--         currentTeam <- v .: "currentTeam"
--         primaryPosition <- v .: "primaryPosition"
--         batSide <- v .: "batSide"
--         pitchHand <- v .: "pitchHand"
--         active <- v .: "active"
--         return OfficialPlayer{..}
