{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MLB.Client
  ( fetchActiveRoster
  , MLBError(..)
  ) where

import Control.Exception (try, SomeException, evaluate)
import qualified Data.ByteString as B
import Data.Aeson (eitherDecodeStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Simple
    ( parseRequest_
    , httpBS
    , getResponseBody
    , getResponseStatusCode
    )
import System.IO (hPutStrLn, stderr)
import Types.Player (Player)
import MLB.Parse (MLBRosterResponse, mlbResponseToPlayers)

data MLBError
  = HttpError String
  | ParseError String
  | ApiError Int String
  deriving (Show)

rosterUrl :: Int -> String
rosterUrl season =
  "https://statsapi.mlb.com/api/v1/sports/1/players?activeStatus=ACTIVE&season="
    ++ show season

fetchActiveRoster :: Int -> IO (Either MLBError [Player])
fetchActiveRoster season = do
  hPutStrLn stderr $ "Fetching active roster for season " ++ show season ++ "..."
  result <- try $ do
    let req = parseRequest_ (rosterUrl season)
    response <- httpBS req
    let status = getResponseStatusCode response
        body   = getResponseBody response
    -- Force evaluation so exceptions surface here
    _ <- evaluate (B.length body)
    pure (status, body)

  case result of
    Left (e :: SomeException) ->
      pure $ Left $ HttpError (show e)
    Right (status, body)
      | status /= 200 ->
          pure $ Left $ ApiError status ("HTTP " ++ show status)
      | otherwise ->
          case eitherDecodeStrict body of
            Left err ->
              pure $ Left $ ParseError err
            Right roster -> do
              let players = mlbResponseToPlayers roster
              hPutStrLn stderr $ "Parsed " ++ show (length players) ++ " players"
              pure $ Right players