{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pelotero.Sync.BoxscoresSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.IORef as IORef
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)

import Effectful (Eff, IOE, (:>), liftIO, runEff)
import Effectful.Dispatch.Dynamic (interpret)
import Katip (Severity (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Pelotero.DB.Game (GameRow (..))
import Pelotero.DB.Provider (ProviderName (..))
import Pelotero.DB.Team (TeamRow (..))
import Pelotero.Domain.Id (GameId (..), TeamId (..))
import Pelotero.Effects.FetchLog (runFetchLogInMemory)
import qualified Pelotero.Effects.FetchLog as FetchLog
import Pelotero.Effects.Games (Games, runGamesInMemory)
import qualified Pelotero.Effects.Games as Games
import Pelotero.Effects.Logging
  ( LogLine (..)
  , runLoggingCapture
  , runLoggingDiscard
  )
import Pelotero.Effects.MLBClient
  ( MLBClient (..)
  )
import Pelotero.Effects.Teams (Teams, runTeamsInMemory)
import qualified Pelotero.Effects.Teams as Teams
import Pelotero.DB.FetchLog (FetchLogRow (..))
import Pelotero.Provider.ExternalId (externalIdFromGameId, externalIdFromTeamId)
import Pelotero.Sync.Boxscores
  ( BoxscoreSyncError (..)
  , BoxscoreSyncResult (..)
  , syncBoxscores
  )
import Pelotero.Effects.BoxscoreEntry (BoxscoreEntry, runBoxscoreEntryNever)
import Pelotero.Effects.FetchLog (FetchLog)
import Pelotero.Effects.Logging
  ( Logging
  )
import Pelotero.Effects.Players (Players, runPlayersInMemory)

spec :: Spec
spec = describe "Pelotero.Sync.Boxscores.syncBoxscores" $ do

  it "processes a known game on the first call" $ do
    bytesRef <- IORef.newIORef (constResponse cleanBoxscoreBytes)
    res <- runStack bytesRef $ do
      seedGame
      syncBoxscores ProviderMLB [knownGameId]
    boxGamesSeen      res `shouldBe` 1
    boxGamesProcessed res `shouldBe` 1
    boxGamesUnchanged res `shouldBe` 0
    boxErrors         res `shouldBe` []

  it "short-circuits with BoxUnchanged when bytes are byte-identical" $ do
    bytesRef <- IORef.newIORef (constResponse cleanBoxscoreBytes)
    (firstRes, secondRes) <- runStack bytesRef $ do
      seedGame
      a <- syncBoxscores ProviderMLB [knownGameId]
      b <- syncBoxscores ProviderMLB [knownGameId]
      pure (a, b)
    boxGamesProcessed firstRes  `shouldBe` 1
    boxGamesUnchanged firstRes  `shouldBe` 0
    boxGamesProcessed secondRes `shouldBe` 0
    boxGamesUnchanged secondRes `shouldBe` 1
    boxErrors         secondRes `shouldBe` []

  it "does NOT short-circuit when the fetched bytes change" $ do
    bytesRef <- IORef.newIORef (constResponse cleanBoxscoreBytes)
    (firstRes, secondRes) <- runStack bytesRef $ do
      seedGame
      a <- syncBoxscores ProviderMLB [knownGameId]
      liftIO $ IORef.writeIORef bytesRef (constResponse mutatedBoxscoreBytes)
      b <- syncBoxscores ProviderMLB [knownGameId]
      pure (a, b)
    boxGamesProcessed firstRes  `shouldBe` 1
    boxGamesProcessed secondRes `shouldBe` 1
    boxGamesUnchanged secondRes `shouldBe` 0

  it "leaves the fetch-log SHA unchanged across a short-circuit" $ do
    bytesRef <- IORef.newIORef (constResponse cleanBoxscoreBytes)
    (priorAfterFirst, priorAfterSecond) <- runStack bytesRef $ do
      seedGame
      _ <- syncBoxscores ProviderMLB [knownGameId]
      a <- FetchLog.getLastFetch ProviderMLB "boxscore" knownGameExtId
      _ <- syncBoxscores ProviderMLB [knownGameId]
      b <- FetchLog.getLastFetch ProviderMLB "boxscore" knownGameExtId
      pure (a, b)
    let shaOf = fmap fetchLogPayloadSha256
    shaOf priorAfterFirst `shouldBe` shaOf priorAfterSecond
    shaOf priorAfterFirst `shouldSatisfy` (/= Nothing)

  it "emits exactly one InfoS skip line on the short-circuit path" $ do
    bytesRef  <- IORef.newIORef (constResponse cleanBoxscoreBytes)
    logsRef   <- IORef.newIORef []
    _ <- runEff
      $ runLoggingCapture logsRef
      $ runMLBClientStub bytesRef
      $ runFetchLogInMemory
      $ runBoxscoreEntryNever
      $ runGamesInMemory
      $ runPlayersInMemory
      $ runTeamsInMemory
      $ do
          seedGame
          _ <- syncBoxscores ProviderMLB [knownGameId]
          _ <- syncBoxscores ProviderMLB [knownGameId]
          pure ()
    logs <- IORef.readIORef logsRef
    let skipLines = filter isBoxscoreSkipLine logs
    length skipLines              `shouldBe` 1
    map logLineSeverity skipLines `shouldBe` [InfoS]
    map logLineMessage  skipLines
      `shouldSatisfy` all (T.isInfixOf knownGameExtId)

  it "GameNotKnown when the gameId isn't in the local games table" $ do
    bytesRef <- IORef.newIORef (constResponse cleanBoxscoreBytes)
    res <- runStack bytesRef $
      -- no seedGame here
      syncBoxscores ProviderMLB [knownGameId]
    boxGamesProcessed res `shouldBe` 0
    boxGamesUnchanged res `shouldBe` 0
    boxErrors         res `shouldBe` [GameNotKnown knownGameId]

  it "FetchFailed propagates to boxErrors" $ do
    bytesRef <- IORef.newIORef (\_ -> Left "stub: HTTP go boom")
    res <- runStack bytesRef $ do
      seedGame
      syncBoxscores ProviderMLB [knownGameId]
    boxGamesProcessed res `shouldBe` 0
    case boxErrors res of
      [FetchFailed gid msg] -> do
        gid `shouldBe` knownGameId
        msg `shouldSatisfy` ("HTTP go boom" `T.isInfixOf`) . T.pack
      other -> error ("expected one FetchFailed, got " <> show other)


-- Stack ----------------------------------------------------------------

-- | Peels (top of stack to bottom):
--   Teams, Players, Games, BoxscoreEntry, FetchLog, MLBClient, Logging, IOE
runStack
  :: IORef.IORef (Int -> Either String BS.ByteString)
  -> Eff
       '[ Teams
        , Players
        , Games
        , BoxscoreEntry
        , FetchLog
        , MLBClient
        , Logging
        , IOE
        ]
       a
  -> IO a
runStack bytesRef =
  runEff
    . runLoggingDiscard
    . runMLBClientStub bytesRef
    . runFetchLogInMemory
    . runBoxscoreEntryNever
    . runGamesInMemory
    . runPlayersInMemory
    . runTeamsInMemory

-- Helpers --------------------------------------------------------------

-- | A stub 'MLBClient' interpreter that returns whatever the IORef
-- function says for 'FetchBoxscoreRaw'. The other operations error if
-- invoked, since this test exercises only the boxscore path.
runMLBClientStub
  :: IOE :> es
  => IORef.IORef (Int -> Either String BS.ByteString)
  -> Eff (MLBClient : es) a
  -> Eff es a
runMLBClientStub ref = interpret $ \_ -> \case
  FetchBoxscoreRaw gpk -> liftIO $ do
    f <- IORef.readIORef ref
    pure (f gpk)
  FetchRosters _       -> error "runMLBClientStub: FetchRosters not stubbed"
  FetchSchedule _ _    -> error "runMLBClientStub: FetchSchedule not stubbed"

constResponse :: BS.ByteString -> Int -> Either String BS.ByteString
constResponse bs _ = Right bs

isBoxscoreSkipLine :: LogLine -> Bool
isBoxscoreSkipLine line =
  "boxscore: payload unchanged, skipping" `T.isInfixOf` logLineMessage line

seedGame :: (Teams :> es, Games :> es) => Eff es ()
seedGame = do
  awayDb <- Teams.upsertTeamByExternalId
              ProviderMLB
              (externalIdFromTeamId (TeamId 117))
              (mkTeamRow "Houston Astros" "HOU" "Houston")
  homeDb <- Teams.upsertTeamByExternalId
              ProviderMLB
              (externalIdFromTeamId (TeamId 121))
              (mkTeamRow "New York Mets"  "NYM" "New York")
  _ <- Games.upsertGameByExternalId
         ProviderMLB
         knownGameExtId
         GameRow
           { gameRowId                 = Nothing
           , gameRowGameDate           = fromGregorian 2025 4 1
           , gameRowAwayTeamId         = awayDb
           , gameRowHomeTeamId         = homeDb
           , gameRowLastSyncedProvider = Nothing
           , gameRowLastSyncedAt       = Nothing
           }
  pure ()

mkTeamRow :: T.Text -> T.Text -> T.Text -> TeamRow
mkTeamRow name abbr loc = TeamRow
  { teamRowId                 = Nothing
  , teamRowName               = name
  , teamRowAbbreviation       = abbr
  , teamRowLocationName       = loc
  , teamRowLastSyncedProvider = Just ProviderMLB
  , teamRowLastSyncedAt       = Nothing
  }


-- Fixtures -------------------------------------------------------------

knownGameId :: GameId
knownGameId = GameId 778001

knownGameExtId :: T.Text
knownGameExtId = externalIdFromGameId knownGameId

-- | Minimal valid 'WireBoxscore' JSON: empty player maps for both
-- teams. No entries means upsertEntries returns (0, 0) and the
-- 'runBoxscoreEntryNever' interpreter is never invoked.
cleanBoxscoreBytes :: BS.ByteString
cleanBoxscoreBytes =
  "{\"teams\":{\"away\":{\"players\":{}},\"home\":{\"players\":{}}}}"

-- | Same JSON shape, different bytes (whitespace), so SHA differs.
mutatedBoxscoreBytes :: BS.ByteString
mutatedBoxscoreBytes =
  "{\"teams\":{\"away\":{\"players\":{}},\"home\":{\"players\":{}}}} "