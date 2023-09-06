{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Control.Monad

flattenGameData :: FilePath -> Text -> IO (Maybe Value)
flattenGameData filePath gameId = do
    content <- BL.readFile filePath
    case eitherDecode content of
        Left err -> do
            putStrLn $ "Error decoding JSON: " ++ err
            return Nothing
        Right val -> return $ parseMaybe (parser gameId) val
  where
    parser :: Text -> Value -> Parser Value
    parser gid = withObject "root" $ \root -> do
        awayPlayers <- root .: "teams" >>= (.: "away") >>= (.: "players")
        homePlayers <- root .: "teams" >>= (.: "home") >>= (.: "players")
        let allPlayers = HM.union awayPlayers homePlayers

        HM.foldlWithKey' flattenPlayer (pure HM.empty) allPlayers

    flattenPlayer :: Parser (HM.HashMap Text Value) -> Text -> Value -> Parser (HM.HashMap Text Value)
    flattenPlayer acc key val = do
        accMap <- acc
        parsedVal <- withObject "player" flatten val
        return $ HM.insert key parsedVal accMap

    flatten :: Object -> Parser Value
    flatten obj = do
        person <- obj .: "person"
        playerId <- person .: "id"
        fullName <- person .: "fullName"
        parentTeamId <- obj .: "parentTeamId"
        allPositions <- fromMaybe [] <$> obj .:? "allPositions"
        let positionCodes = mapMaybe (HM.lookup "code" >=> toBoundedInteger) allPositions
        status <- obj .: "status" >>= (.: "code")
        batting <- customDel ["note", "summary", "stolenBasePercentage", "atBatsPerHomeRun"] <$> obj .:? "stats" >>= (.:? "batting")
        pitching <- customDel ["note", "summary", "stolenBasePercentage", "strikePercentage", "homeRunsPer9", "runsScoredPer9"] <$> obj .:? "stats" >>= (.:? "pitching")

        return . Object $ HM.fromList [("id", Number (fromIntegral playerId)), ("fullName", String fullName), ("stats", Object $ HM.fromList [(gameId, Object $ HM.fromList [("parentTeamId", Number (fromIntegral parentTeamId)), ("allPositions", Array (V.fromList $ fmap Number positionCodes)), ("status", String status), ("batting", batting), ("pitching", pitching)])])]

    customDel :: [Text] -> Maybe Value -> Maybe Value
    customDel keys (Just (Object obj)) = Just . Object $ HM.foldlWithKey' (\acc k v -> if k `elem` keys then acc else HM.insert k v acc) HM.empty obj
    customDel _ val = val
