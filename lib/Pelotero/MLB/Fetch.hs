{-# LANGUAGE TypeOperators #-}

module Pelotero.MLB.Fetch
  ( FetchedRosters(..)
  , FetchedSchedule(..)
  , fetchRosters
  , fetchSchedule
  , fetchBoxscoreRaw
  ) where

import qualified Crypto.Hash.SHA256        as SHA256
import qualified Data.Aeson                as Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as B16
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Simple       as Simple
import qualified Network.HTTP.Types.Status as Status

import qualified Pelotero.Domain.Game     as DGame
import qualified Pelotero.Domain.Player   as DPlayer
import qualified Pelotero.Domain.Team     as DTeam
import           Pelotero.Domain.Id       (TeamId(..))
import qualified Pelotero.MLB.Convert     as Convert
import qualified Pelotero.MLB.Urls        as Urls
import qualified Pelotero.MLB.Wire.Team   as WT

data FetchedRosters = FetchedRosters
  { frTeams      :: ![DTeam.Team]
  , frPlayers    :: ![DPlayer.Player]
  , frWarnings   :: ![Convert.ConvertWarning]
  , frPayloadSha :: !T.Text
  }

fetchRosters :: Int -> IO (Either String FetchedRosters)
fetchRosters season = do
  teamsBody   <- fetchUrl (Urls.teamsUrl season)
  playersBody <- fetchUrl (Urls.rosterUrl season)
  case (teamsBody, playersBody) of
    (Left err, _) -> pure (Left err)
    (_, Left err) -> pure (Left err)
    (Right tb, Right pb) ->
      case Aeson.eitherDecodeStrict (LBS.toStrict tb) of
        Left err -> pure (Left ("Teams parse failure: " <> err))
        Right teamsEnv ->
          case Aeson.eitherDecodeStrict (LBS.toStrict pb) of
            Left err -> pure (Left ("Players parse failure: " <> err))
            Right playersEnv -> do
              let wireTeams              = WT.wireTeams teamsEnv
                  (playerWarns, players) = Convert.convertPlayers playersEnv
                  domainTeams            = map wireToDomainTeam wireTeams
                  sha = computeSha (LBS.toStrict tb <> LBS.toStrict pb)
              pure $ Right FetchedRosters
                { frTeams      = domainTeams
                , frPlayers    = players
                , frWarnings   = playerWarns
                , frPayloadSha = sha
                }

wireToDomainTeam :: WT.WireTeam -> DTeam.Team
wireToDomainTeam wt = DTeam.Team
  { DTeam.teamId           = TeamId (WT.wtId wt)
  , DTeam.teamName         = WT.wtName wt
  , DTeam.teamAbbreviation = WT.wtAbbreviation wt
  , DTeam.teamLocationName = maybe T.empty id (WT.wtLocationName wt)
  }

data FetchedSchedule = FetchedSchedule
  { fsGames      :: ![DGame.Game]
  , fsWarnings   :: ![Convert.ConvertWarning]
  , fsPayloadSha :: !T.Text
  }

fetchSchedule :: String -> String -> IO (Either String FetchedSchedule)
fetchSchedule startDate endDate = do
  body <- fetchUrl (Urls.scheduleUrl startDate endDate)
  case body of
    Left err -> pure (Left err)
    Right lb -> do
      let strict = LBS.toStrict lb
      case Aeson.eitherDecodeStrict strict of
        Left err -> pure (Left ("Schedule parse failure: " <> err))
        Right env -> do
          let (warnings, schedule) = Convert.convertSchedule env
          pure $ Right FetchedSchedule
            { fsGames      = DGame.unGameSchedule schedule
            , fsWarnings   = warnings
            , fsPayloadSha = computeSha strict
            }

fetchBoxscoreRaw :: Int -> IO (Either String BS.ByteString)
fetchBoxscoreRaw gamePk = do
  body <- fetchUrl (Urls.boxscoreUrl gamePk)
  case body of
    Left err -> pure (Left err)
    Right lb -> pure (Right (LBS.toStrict lb))

fetchUrl :: String -> IO (Either String LBS.ByteString)
fetchUrl url = do
  req  <- HTTP.parseRequest url
  resp <- Simple.httpLBS req
  let st = HTTP.responseStatus resp
  if Status.statusIsSuccessful st
    then pure (Right (HTTP.responseBody resp))
    else pure (Left ("HTTP " <> show (Status.statusCode st) <> " from " <> url))

computeSha :: BS.ByteString -> T.Text
computeSha = TE.decodeUtf8 . B16.encode . SHA256.hash