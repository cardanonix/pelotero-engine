{-# LANGUAGE OverloadedStrings #-}

module Conversion where
import Data.Aeson ( FromJSON(parseJSON), Value(Object), (.:) )
import Data.Csv (ToNamedRecord, namedRecord, (.=))
import qualified Data.Csv as Csv
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import Control.Monad (mzero)

-- Player structure
data Player = Player
    { active :: Bool
    , batSide :: String
    , currentTeam :: Int
    , nameSlug :: String
    , pitchHand :: String
    , playerId :: Int
    , primaryPosition :: String
    , useLastName :: String
    , useName :: String
    } deriving (Show)

-- Define the JSON structure for the entire file
newtype PlayersFile
  = PlayersFile {officialPlayers :: HM.HashMap String Player}

instance FromJSON Player where
    parseJSON (Object v) =
        Player <$> v .: "active"
               <*> v .: "batSide"
               <*> v .: "currentTeam"
               <*> v .: "nameSlug"
               <*> v .: "pitchHand"
               <*> v .: "playerId"
               <*> v .: "primaryPosition"
               <*> v .: "useLastName"
               <*> v .: "useName"
    parseJSON _ = mzero

instance Csv.ToNamedRecord Player where
    toNamedRecord p = Csv.namedRecord
        [ TE.encodeUtf8 "active" Csv..= boolToString (active p)
        , TE.encodeUtf8 "batSide" Csv..= batSide p
        , TE.encodeUtf8 "currentTeam" Csv..= currentTeam p
        , TE.encodeUtf8 "nameSlug" Csv..= nameSlug p
        , TE.encodeUtf8 "pitchHand" Csv..= pitchHand p
        , TE.encodeUtf8 "playerId" Csv..= playerId p
        , TE.encodeUtf8 "primaryPosition" Csv..= primaryPosition p
        , TE.encodeUtf8 "useLastName" Csv..= useLastName p
        , TE.encodeUtf8 "useName" Csv..= useName p
        ]

instance Csv.DefaultOrdered Player where
    headerOrder _ = Csv.header
        [ "active"
        , "batSide"
        , "currentTeam"
        , "nameSlug"
        , "pitchHand"
        , "playerId"
        , "primaryPosition"
        , "useLastName"
        , "useName"
        ]

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

instance FromJSON PlayersFile where
    parseJSON (Object v) = PlayersFile <$> v .: "officialPlayers"
    parseJSON _ = mzero