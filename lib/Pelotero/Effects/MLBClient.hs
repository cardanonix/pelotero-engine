{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs             #-}

module Pelotero.Effects.MLBClient
  ( MLBClient(..)
  , fetchRosters
  , fetchSchedule
  , fetchBoxscoreRaw

  , runMLBClientHTTP

  , MLBFixture(..)
  , defaultFixture
  , runMLBClientFixture
  ) where

import           Control.Exception      (try)
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           System.FilePath        ((</>))

import           Effectful              (Effect, IOE, Dispatch(Dynamic), DispatchOf)
import qualified Effectful              as E
import           Effectful.Dispatch.Dynamic (interpret_, send)

import           Pelotero.Domain.Id         (TeamId(..))
import qualified Pelotero.Domain.Game       as DGame
import qualified Pelotero.Domain.Team       as DTeam
import qualified Pelotero.MLB.Convert       as Convert
import qualified Pelotero.MLB.Fetch         as Fetch
import           Pelotero.MLB.Fetch         (FetchedRosters(..), FetchedSchedule(..))
import qualified Pelotero.MLB.Wire.Team     as WT

-- ============================================================================
-- Effect
-- ============================================================================

data MLBClient :: Effect where
  FetchRosters     :: Int              -> MLBClient m (Either String FetchedRosters)
  FetchSchedule    :: String -> String -> MLBClient m (Either String FetchedSchedule)
  FetchBoxscoreRaw :: Int              -> MLBClient m (Either String BS.ByteString)

type instance DispatchOf MLBClient = 'Dynamic

fetchRosters
  :: MLBClient E.:> es
  => Int -> E.Eff es (Either String FetchedRosters)
fetchRosters = send . FetchRosters

fetchSchedule
  :: MLBClient E.:> es
  => String -> String -> E.Eff es (Either String FetchedSchedule)
fetchSchedule start end = send (FetchSchedule start end)

fetchBoxscoreRaw
  :: MLBClient E.:> es
  => Int -> E.Eff es (Either String BS.ByteString)
fetchBoxscoreRaw = send . FetchBoxscoreRaw

-- ============================================================================
-- HTTP interpreter (production)
-- ============================================================================

runMLBClientHTTP
  :: IOE E.:> es
  => E.Eff (MLBClient : es) a
  -> E.Eff es a
runMLBClientHTTP = interpret_ $ \case
  FetchRosters season             -> E.liftIO (Fetch.fetchRosters season)
  FetchSchedule startDate endDate -> E.liftIO (Fetch.fetchSchedule startDate endDate)
  FetchBoxscoreRaw gamePk         -> E.liftIO (Fetch.fetchBoxscoreRaw gamePk)

-- ============================================================================
-- Fixture interpreter (offline tests)
--
-- The fixture maps requests to files on disk containing the raw response
-- bodies that the real MLB API would return. Decoding goes through the same
-- Pelotero.MLB.Convert pipeline as production, so the wire-to-domain
-- translation is exercised in tests.
-- ============================================================================

data MLBFixture = MLBFixture
  { fixturesDir      :: !FilePath
    -- ^ Default lookup directory. Expected layout:
    --   teams-<season>.json
    --   players-<season>.json
    --   schedule-<startDate>-<endDate>.json
    --   boxscore-<gamePk>.json
  , fixtureBoxscores :: !(Map Int FilePath)
    -- ^ Optional per-game overrides for boxscore lookups. When a gamePk
    --   is not in this map, FetchBoxscoreRaw falls back to
    --   <fixturesDir>/boxscore-<gamePk>.json.
  }

defaultFixture :: FilePath -> MLBFixture
defaultFixture dir = MLBFixture { fixturesDir = dir, fixtureBoxscores = Map.empty }

runMLBClientFixture
  :: IOE E.:> es
  => MLBFixture
  -> E.Eff (MLBClient : es) a
  -> E.Eff es a
runMLBClientFixture fix = interpret_ $ \case
  FetchRosters season -> E.liftIO $ do
    let teamsPath   = fixturesDir fix </> ("teams-"   <> show season <> ".json")
        playersPath = fixturesDir fix </> ("players-" <> show season <> ".json")
    teamsBytes   <- safeRead teamsPath
    playersBytes <- safeRead playersPath
    case (teamsBytes, playersBytes) of
      (Left e, _)         -> pure (Left e)
      (_, Left e)         -> pure (Left e)
      (Right tb, Right pb) ->
        case Aeson.eitherDecodeStrict tb of
          Left err       -> pure (Left ("Teams parse failure: " <> err))
          Right teamsEnv ->
            case Aeson.eitherDecodeStrict pb of
              Left err         -> pure (Left ("Players parse failure: " <> err))
              Right playersEnv -> do
                let wireTeams              = WT.wireTeams teamsEnv
                    (playerWarns, players) = Convert.convertPlayers playersEnv
                    domainTeams            = map wireToDomainTeam wireTeams
                    sha = sha256Hex (tb <> pb)
                pure $ Right FetchedRosters
                  { frTeams      = domainTeams
                  , frPlayers    = players
                  , frWarnings   = playerWarns
                  , frPayloadSha = sha
                  }

  FetchSchedule startDate endDate -> E.liftIO $ do
    let path = fixturesDir fix
             </> ("schedule-" <> startDate <> "-" <> endDate <> ".json")
    body <- safeRead path
    case body of
      Left e   -> pure (Left e)
      Right bs ->
        case Aeson.eitherDecodeStrict bs of
          Left err  -> pure (Left ("Schedule parse failure: " <> err))
          Right env -> do
            let (warnings, schedule) = Convert.convertSchedule env
            pure $ Right FetchedSchedule
              { fsGames      = DGame.unGameSchedule schedule
              , fsWarnings   = warnings
              , fsPayloadSha = sha256Hex bs
              }

  FetchBoxscoreRaw gamePk -> E.liftIO $ do
    let defaultPath = fixturesDir fix </> ("boxscore-" <> show gamePk <> ".json")
        path        = Map.findWithDefault defaultPath gamePk (fixtureBoxscores fix)
    safeRead path
  where
    safeRead :: FilePath -> IO (Either String BS.ByteString)
    safeRead p = do
      r <- try (BS.readFile p)
      pure $ case r of
        Right bs              -> Right bs
        Left (e :: IOError)   -> Left ("fixture not found at " <> p <> ": " <> show e)

    sha256Hex :: BS.ByteString -> T.Text
    sha256Hex = TE.decodeUtf8 . B16.encode . SHA256.hash

    -- Same conversion as Pelotero.MLB.Fetch.wireToDomainTeam (which is
    -- private to that module). One-line translation; not worth a new export.
    wireToDomainTeam wt = DTeam.Team
      { DTeam.teamId           = TeamId (WT.wtId wt)
      , DTeam.teamName         = WT.wtName wt
      , DTeam.teamAbbreviation = WT.wtAbbreviation wt
      , DTeam.teamLocationName = maybe T.empty id (WT.wtLocationName wt)
      }