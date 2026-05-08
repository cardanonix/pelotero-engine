{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Pelotero.Sync.ScheduleSpec (spec) where

import qualified Data.IORef as IORef
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))

import Effectful (Eff, (:>), runEff)
import Katip (Severity (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Pelotero.DB.Provider (ProviderName (..))
import Pelotero.DB.Team (TeamRow (..))
import Pelotero.Domain.Game (Game (..))
import Pelotero.Domain.Id (GameId (..), TeamId (..))
import Pelotero.Effects.Clock (runClockFixed)
import Pelotero.Effects.FetchLog (runFetchLogInMemory)
import Pelotero.Effects.Games (runGamesInMemory)
import Pelotero.Effects.Logging
  ( LogLine (..)
  , runLoggingCapture
  , runLoggingDiscard
  )
import Pelotero.Effects.Teams (Teams, runTeamsInMemory)
import qualified Pelotero.Effects.Teams as Teams
import Pelotero.Provider.ExternalId (externalIdFromTeamId)
import Pelotero.Sync.Schedule (ScheduleSyncResult (..), syncSchedule)

spec :: Spec
spec = describe "Pelotero.Sync.Schedule.syncSchedule" $ do

  it "upserts every game on the first call (with teams seeded)" $ do
    res <- runSchedule firstSha sampleGames
    schedGamesUpserted res `shouldBe` length sampleGames
    schedGamesSkipped  res `shouldBe` 0
    schedFetchSha256   res `shouldBe` firstSha

  it "skips games whose teams are missing (and counts them)" $ do
    -- No teams seeded; resolveTeam returns Nothing for both sides of every game.
    res <- runEff
      $ runLoggingDiscard
      $ runClockFixed fixedTime
      $ runFetchLogInMemory
      $ runGamesInMemory
      $ runTeamsInMemory
      $ syncSchedule ProviderMLB scope firstSha sampleGames
    schedGamesUpserted res `shouldBe` 0
    schedGamesSkipped  res `shouldBe` length sampleGames

  it "short-circuits when the SHA matches the prior fetch" $ do
    (firstRes, secondRes) <- runEff
      $ runLoggingDiscard
      $ runClockFixed fixedTime
      $ runFetchLogInMemory
      $ runGamesInMemory
      $ runTeamsInMemory
      $ do
          seedTeams
          a <- syncSchedule ProviderMLB scope firstSha sampleGames
          b <- syncSchedule ProviderMLB scope firstSha sampleGames
          pure (a, b)
    schedGamesUpserted firstRes  `shouldBe` length sampleGames
    schedGamesUpserted secondRes `shouldBe` 0
    schedGamesSkipped  secondRes `shouldBe` 0
    schedFetchSha256   secondRes `shouldBe` firstSha

  it "does NOT short-circuit when the SHA differs" $ do
    (_firstRes, secondRes) <- runEff
      $ runLoggingDiscard
      $ runClockFixed fixedTime
      $ runFetchLogInMemory
      $ runGamesInMemory
      $ runTeamsInMemory
      $ do
          seedTeams
          a <- syncSchedule ProviderMLB scope firstSha sampleGames
          b <- syncSchedule ProviderMLB scope secondSha sampleGames
          pure (a, b)
    schedGamesUpserted secondRes `shouldBe` length sampleGames
    schedFetchSha256   secondRes `shouldBe` secondSha

  it "does NOT short-circuit across different scopes (same SHA, different scope)" $ do
    (_firstRes, secondRes) <- runEff
      $ runLoggingDiscard
      $ runClockFixed fixedTime
      $ runFetchLogInMemory
      $ runGamesInMemory
      $ runTeamsInMemory
      $ do
          seedTeams
          a <- syncSchedule ProviderMLB scope      firstSha sampleGames
          b <- syncSchedule ProviderMLB otherScope firstSha sampleGames
          pure (a, b)
    schedGamesUpserted secondRes `shouldBe` length sampleGames

  it "emits exactly one InfoS skip line on the short-circuit path" $ do
    ref <- IORef.newIORef []
    _ <- runEff
      $ runLoggingCapture ref
      $ runClockFixed fixedTime
      $ runFetchLogInMemory
      $ runGamesInMemory
      $ runTeamsInMemory
      $ do
          seedTeams
          _ <- syncSchedule ProviderMLB scope firstSha sampleGames
          _ <- syncSchedule ProviderMLB scope firstSha sampleGames
          pure ()
    logs <- IORef.readIORef ref
    let skipLines = filter isScheduleSkipLine logs
    length skipLines              `shouldBe` 1
    map logLineSeverity skipLines `shouldBe` [InfoS]
    map logLineMessage  skipLines
      `shouldSatisfy` all (T.isInfixOf firstSha)


-- Helpers --------------------------------------------------------------

runSchedule :: T.Text -> [Game] -> IO ScheduleSyncResult
runSchedule sha games = runEff
  $ runLoggingDiscard
  $ runClockFixed fixedTime
  $ runFetchLogInMemory
  $ runGamesInMemory
  $ runTeamsInMemory
  $ do
      seedTeams
      syncSchedule ProviderMLB scope sha games

seedTeams :: Teams :> es => Eff es ()
seedTeams = mapM_ seed sampleTeamRows
  where
    seed (tid, name, abbr, loc) = do
      let row = TeamRow
            { teamRowId                 = Nothing
            , teamRowName               = name
            , teamRowAbbreviation       = abbr
            , teamRowLocationName       = loc
            , teamRowLastSyncedProvider = Just ProviderMLB
            , teamRowLastSyncedAt       = Nothing
            }
      _ <- Teams.upsertTeamByExternalId
             ProviderMLB
             (externalIdFromTeamId tid)
             row
      pure ()

isScheduleSkipLine :: LogLine -> Bool
isScheduleSkipLine line =
  "schedule: payload unchanged, skipping" `T.isInfixOf` logLineMessage line


-- Fixtures -------------------------------------------------------------

scope, otherScope, firstSha, secondSha :: T.Text
scope      = "2025-04-01..2025-04-07"
otherScope = "2025-04-08..2025-04-14"
firstSha   = "sha-aaa"
secondSha  = "sha-bbb"

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2025 4 1) 0

sampleTeamRows :: [(TeamId, T.Text, T.Text, T.Text)]
sampleTeamRows =
  [ (TeamId 117, "Houston Astros",     "HOU", "Houston")
  , (TeamId 121, "New York Mets",      "NYM", "New York")
  , (TeamId 110, "Baltimore Orioles",  "BAL", "Baltimore")
  , (TeamId 145, "Chicago White Sox",  "CWS", "Chicago")
  ]

sampleGames :: [Game]
sampleGames =
  [ Game
      { gameId       = GameId 778001
      , gameDate     = fromGregorian 2025 4 1
      , gameAwayTeam = TeamId 117
      , gameHomeTeam = TeamId 121
      }
  , Game
      { gameId       = GameId 778002
      , gameDate     = fromGregorian 2025 4 2
      , gameAwayTeam = TeamId 110
      , gameHomeTeam = TeamId 145
      }
  ]