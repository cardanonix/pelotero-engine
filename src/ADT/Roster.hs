{-# LANGUAGE OverloadedStrings, GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Roster where

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
import Config (parseDouble, Configuration)

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

findPlayerPosition :: Text -> LgManager -> Either Text Text
findPlayerPosition playerName mgr =
    case currentPlayerPosition of
        Just pos -> Right pos
        Nothing  -> Left "Player not found in current lineup."
    where
        lineup = current_lineup mgr
        currentPlayerPosition
          | playerName == cC lineup = Just "C"
          | playerName == b1C lineup = Just "1B"
          | playerName == b2C lineup = Just "2B"
          | playerName == b3C lineup = Just "3B"
          | playerName == ssC lineup = Just "SS"
          | playerName == uC lineup = Just "U"
          | playerName `elem` ofC lineup = Just "OF"
          | playerName `elem` spC lineup = Just "SP"
          | playerName `elem` rpC lineup = Just "RP"

batterOrPitcher :: Text -> LgManager -> Either Text Text
batterOrPitcher playerName mgr 
    | isBatter   = Right "Batter"
    | isPitcher  = Right "Pitcher"
    | otherwise  = Left "Player not found in current lineup."
    where
        lineup = current_lineup mgr
        batterPositions = [cC lineup, b1C lineup, b2C lineup, b3C lineup, ssC lineup, uC lineup] ++ ofC lineup
        pitcherPositions = spC lineup ++ rpC lineup

        isBatter = playerName `elem` batterPositions
        isPitcher = playerName `elem` pitcherPositions