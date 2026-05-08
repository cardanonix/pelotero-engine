{-# LANGUAGE OverloadedStrings #-}

module Pelotero.Sync.PlayersSpec (spec) where

import qualified Data.IORef as IORef
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))

import Effectful (runEff)
import Katip (Severity (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Pelotero.DB.Provider (ProviderName (..))
import Pelotero.Domain.Id (PlayerId (..), TeamId (..))
import qualified Pelotero.Domain.Player as DPlayer
import qualified Pelotero.Domain.Team as DTeam
import Pelotero.Effects.Clock (runClockFixed)
import Pelotero.Effects.FetchLog (runFetchLogInMemory)
import Pelotero.Effects.Logging
  ( LogLine (..)
  , runLoggingCapture
  , runLoggingDiscard
  )
import Pelotero.Effects.Players (runPlayersInMemory)
import Pelotero.Effects.Teams (runTeamsInMemory)
import Pelotero.Sync.Players (SyncResult (..), syncRosters)

spec :: Spec
spec = describe "Pelotero.Sync.Players.syncRosters" $ do

  it "upserts all rows on the first call" $ do
    res <- runRosters firstSha sampleTeams samplePlayers
    syncTeamsUpserted   res `shouldBe` length sampleTeams
    syncPlayersUpserted res `shouldBe` length samplePlayers
    syncFetchSha256     res `shouldBe` firstSha

  it "short-circuits when the SHA matches the prior fetch" $ do
    (firstRes, secondRes) <- runEff
      $ runLoggingDiscard
      $ runClockFixed fixedTime
      $ runFetchLogInMemory
      $ runTeamsInMemory
      $ runPlayersInMemory
      $ do
          a <- syncRosters ProviderMLB scope firstSha sampleTeams samplePlayers
          b <- syncRosters ProviderMLB scope firstSha sampleTeams samplePlayers
          pure (a, b)
    syncTeamsUpserted   firstRes  `shouldBe` length sampleTeams
    syncPlayersUpserted firstRes  `shouldBe` length samplePlayers
    syncTeamsUpserted   secondRes `shouldBe` 0
    syncPlayersUpserted secondRes `shouldBe` 0
    syncFetchSha256     secondRes `shouldBe` firstSha

  it "does NOT short-circuit when the SHA differs" $ do
    (_firstRes, secondRes) <- runEff
      $ runLoggingDiscard
      $ runClockFixed fixedTime
      $ runFetchLogInMemory
      $ runTeamsInMemory
      $ runPlayersInMemory
      $ do
          a <- syncRosters ProviderMLB scope firstSha sampleTeams samplePlayers
          b <- syncRosters ProviderMLB scope secondSha sampleTeams samplePlayers
          pure (a, b)
    syncTeamsUpserted   secondRes `shouldBe` length sampleTeams
    syncPlayersUpserted secondRes `shouldBe` length samplePlayers
    syncFetchSha256     secondRes `shouldBe` secondSha

  it "does NOT short-circuit across different scopes (same SHA, different scope)" $ do
    (_firstRes, secondRes) <- runEff
      $ runLoggingDiscard
      $ runClockFixed fixedTime
      $ runFetchLogInMemory
      $ runTeamsInMemory
      $ runPlayersInMemory
      $ do
          a <- syncRosters ProviderMLB scope      firstSha sampleTeams samplePlayers
          b <- syncRosters ProviderMLB otherScope firstSha sampleTeams samplePlayers
          pure (a, b)
    syncTeamsUpserted   secondRes `shouldBe` length sampleTeams
    syncPlayersUpserted secondRes `shouldBe` length samplePlayers

  it "emits exactly one InfoS skip line on the short-circuit path" $ do
    ref <- IORef.newIORef []
    _ <- runEff
      $ runLoggingCapture ref
      $ runClockFixed fixedTime
      $ runFetchLogInMemory
      $ runTeamsInMemory
      $ runPlayersInMemory
      $ do
          _ <- syncRosters ProviderMLB scope firstSha sampleTeams samplePlayers
          _ <- syncRosters ProviderMLB scope firstSha sampleTeams samplePlayers
          pure ()
    logs <- IORef.readIORef ref
    let skipLines = filter isRosterSkipLine logs
    length skipLines                  `shouldBe` 1
    map logLineSeverity skipLines     `shouldBe` [InfoS]
    map logLineMessage  skipLines
      `shouldSatisfy` all (T.isInfixOf firstSha)

  it "first call writes no skip line (the SHA was unknown)" $ do
    ref <- IORef.newIORef []
    _ <- runEff
      $ runLoggingCapture ref
      $ runClockFixed fixedTime
      $ runFetchLogInMemory
      $ runTeamsInMemory
      $ runPlayersInMemory
      $ syncRosters ProviderMLB scope firstSha sampleTeams samplePlayers
    logs <- IORef.readIORef ref
    filter isRosterSkipLine logs `shouldBe` []


-- Helpers --------------------------------------------------------------

runRosters :: T.Text -> [DTeam.Team] -> [DPlayer.Player] -> IO SyncResult
runRosters sha teams players = runEff
  $ runLoggingDiscard
  $ runClockFixed fixedTime
  $ runFetchLogInMemory
  $ runTeamsInMemory
  $ runPlayersInMemory
  $ syncRosters ProviderMLB scope sha teams players

isRosterSkipLine :: LogLine -> Bool
isRosterSkipLine line =
  "rosters: payload unchanged, skipping" `T.isInfixOf` logLineMessage line


-- Fixtures -------------------------------------------------------------

scope, otherScope, firstSha, secondSha :: T.Text
scope      = "2025"
otherScope = "2024"
firstSha   = "sha-aaa"
secondSha  = "sha-bbb"

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2025 4 1) 0

sampleTeams :: [DTeam.Team]
sampleTeams =
  [ DTeam.Team
      { DTeam.teamId           = TeamId 117
      , DTeam.teamName         = "Houston Astros"
      , DTeam.teamAbbreviation = "HOU"
      , DTeam.teamLocationName = "Houston"
      }
  , DTeam.Team
      { DTeam.teamId           = TeamId 121
      , DTeam.teamName         = "New York Mets"
      , DTeam.teamAbbreviation = "NYM"
      , DTeam.teamLocationName = "New York"
      }
  ]

samplePlayers :: [DPlayer.Player]
samplePlayers =
  [ DPlayer.Player
      { DPlayer.playerId        = PlayerId 545361
      , DPlayer.playerFirstName = "Mike"
      , DPlayer.playerLastName  = "Trout"
      , DPlayer.playerNameSlug  = "mike-trout"
      , DPlayer.playerTeamId    = Just (TeamId 117)
      , DPlayer.playerPosition  = Nothing
      , DPlayer.playerBatSide   = Nothing
      , DPlayer.playerPitchHand = Nothing
      , DPlayer.playerActive    = True
      }
  , DPlayer.Player
      { DPlayer.playerId        = PlayerId 660271
      , DPlayer.playerFirstName = "Shohei"
      , DPlayer.playerLastName  = "Ohtani"
      , DPlayer.playerNameSlug  = "shohei-ohtani"
      , DPlayer.playerTeamId    = Just (TeamId 121)
      , DPlayer.playerPosition  = Nothing
      , DPlayer.playerBatSide   = Nothing
      , DPlayer.playerPitchHand = Nothing
      , DPlayer.playerActive    = True
      }
  ]