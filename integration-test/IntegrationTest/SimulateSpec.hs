{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : IntegrationTest.SimulateSpec
--
-- Runs 'Pelotero.Simulate.runSimulation' against a real database with
-- the MLB client reading the on-disk 2025 fixtures. This covers the
-- whole workflow in one chain: roster sync, schedule sync, league
-- creation, random rankings, auto-draft, lineups, activation, lineup
-- snapshots, boxscore sync, scoring, and matchups.
--
-- The rosters come from a full-season teams and players snapshot
-- (teams-2025-full.json, players-2025-full.json) rather than the small
-- players-2025.json other specs use, because a four-team draft of 25
-- players each needs a pool deep enough at every position. Both files
-- are fetched with the same URLs as Pelotero.MLB.Urls.
--
-- The run is deterministic: fixture data, a fixed clock, and a fixed
-- random seed.
module IntegrationTest.SimulateSpec (spec) where

import           Data.List                       (nub, sort)
import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import           Data.Time                       (Day, UTCTime (..), fromGregorian)
import           Test.Hspec

import           Effectful                       (Eff, IOE, runEff)
import           Effectful.Error.Static          (Error, runErrorNoCallStack)

import           Pelotero.DB.LeagueConfig        (LoadedLeagueConfig (..))
import           Pelotero.DB.LeagueTeam          (LoadedLeagueTeam (..))
import           Pelotero.DB.LineupSlot          (LineupSlotRow (..))
import           Pelotero.DB.Pool                (DBError, Pool)
import           Pelotero.DB.RosterSlot          (RosterSlotRow (..))
import           Pelotero.Domain.Roster
                     ( RosterSlot
                     , allRosterSlots
                     , lineupLimitFor
                     , renderRosterSlot
                     , rosterLimitFor
                     )
import           Pelotero.Draft                  (DraftPickEntry (..), DraftSummary (..))
import           Pelotero.Effects.BoxscoreEntry  (BoxscoreEntry, runBoxscoreEntryDB)
import           Pelotero.Effects.Clock          (Clock, runClockFixed)
import           Pelotero.Effects.Database       (Database, runDatabasePool)
import           Pelotero.Effects.DraftPick      (DraftPick, runDraftPickDB)
import           Pelotero.Effects.FetchLog       (FetchLog, runFetchLogDB)
import           Pelotero.Effects.Games          (Games, runGamesDB)
import qualified Pelotero.Effects.LeagueConfig   as LC
import           Pelotero.Effects.LeagueTeam     (LeagueTeam, runLeagueTeamDB)
import qualified Pelotero.Effects.LineupSlot     as LS
import           Pelotero.Effects.LineupSnapshot (LineupSnapshot, runLineupSnapshotDB)
import           Pelotero.Effects.Logging        (Logging, runLoggingDiscard)
import           Pelotero.Effects.MLBClient
                     ( MLBClient
                     , MLBFixture (fixturePlayers, fixtureTeams)
                     , defaultFixture
                     , runMLBClientFixture
                     )
import           Pelotero.Effects.PlayerRanking  (PlayerRanking, runPlayerRankingDB)
import           Pelotero.Effects.Players        (Players, runPlayersDB)
import           Pelotero.Effects.Random         (Random, runRandomSeeded)
import qualified Pelotero.Effects.RosterSlot     as RS
import           Pelotero.Effects.Teams          (Teams, runTeamsDB)
import           Pelotero.League.Setup
                     ( LeagueSetupError (..)
                     , standardLineupLimits
                     , standardRosterLimits
                     )
import qualified Pelotero.Lineup.Snapshot        as Snapshot
import           Pelotero.Matchup                (matchupAway, matchupHome)
import           Pelotero.Score                  (LeagueScore (..), TeamScore (..))
import           Pelotero.Simulate

import           IntegrationTest.Setup           (cleanDatabase, runEffectsOrFail)

spec :: SpecWith Pool
spec = describe "Pelotero.Simulate.runSimulation (fixture-driven)" $ do

  it "drafts, activates, snapshots and scores a random four-team league"
     $ \pool -> do
    cleanDatabase pool

    (result, rosters, lineups, status) <- runStack pool 2025 $ do
      result <- runSimulation (config "sim-a")
      case result of
        Left _ -> pure (result, [], [], Nothing)
        Right report -> do
          let teamIds = map lltId (srTeams report)
          rosters <- traverse RS.getSlotsForTeam teamIds
          lineups <- traverse LS.getSlotsForTeam teamIds
          status  <- fmap llcStatus <$> LC.getById (srLeague report)
          pure (result, rosters, lineups, status)

    report <- case result of
      Right r  -> pure r
      Left err -> expectationFailure ("simulation failed: " <> show err)
                    >> error "unreachable"

    -- League shape.
    length (srTeams report) `shouldBe` 4
    status `shouldBe` Just "active"

    -- The draft made 25 picks per team and no player went twice.
    let picks = dsPicks (srDraft report)
    length picks `shouldBe` 100
    length (nub (map dpePlayer picks)) `shouldBe` 100

    -- Every roster fills every slot exactly to the standard limits.
    length rosters `shouldBe` 4
    mapM_ (\roster ->
             slotCounts (map rsSlot roster) `shouldBe` expectedCounts rosterLimit)
          rosters

    -- Every lineup fills every slot exactly to the standard lineup
    -- limits, and only with players from the same team's roster.
    mapM_ (\(roster, lineup) -> do
             slotCounts (map lsSlot lineup) `shouldBe` expectedCounts lineupLimit
             all (`elem` map rsPlayerId roster) (map lsPlayerId lineup)
               `shouldBe` True)
          (zip rosters lineups)
    srLineupRows report `shouldBe` 60

    -- The fixture schedule has games, and every team was snapshotted
    -- for every one of them with its full lineup.
    srGames report `shouldSatisfy` (> 0)
    Snapshot.snapTeamsSnapshotted (srSnapshot report) `shouldBe` 4 * srGames report
    Snapshot.snapRowsInserted (srSnapshot report) `shouldBe` 60 * srGames report

    -- Scoring covers all four teams, paired into two matchups.
    let scoredTeams = map tsTeam (lscTeams (srScore report))
    sort scoredTeams `shouldBe` sort (map lltId (srTeams report))
    length (srMatchups report) `shouldBe` 2
    fmap tsTeam (srBye report) `shouldBe` Nothing
    sort (concatMap (\m -> [tsTeam (matchupHome m), tsTeam (matchupAway m)])
                    (srMatchups report))
      `shouldBe` sort scoredTeams

  it "is reproducible: the same seed drafts the same players in the same order"
     $ \pool -> do
    cleanDatabase pool
    first  <- runStack pool 7 (runSimulation (config "sim-b1"))
    cleanDatabase pool
    second <- runStack pool 7 (runSimulation (config "sim-b2"))
    case (first, second) of
      (Right a, Right b) -> do
        -- Player ids are assigned by the roster sync in fixture order
        -- after each cleanDatabase, so they line up across the runs.
        map dpePlayer (dsPicks (srDraft a)) `shouldBe` map dpePlayer (dsPicks (srDraft b))
        map dpeSlot   (dsPicks (srDraft a)) `shouldBe` map dpeSlot   (dsPicks (srDraft b))
        map tsTotalPoints (lscTeams (srScore a))
          `shouldBe` map tsTotalPoints (lscTeams (srScore b))
      other -> expectationFailure ("simulation failed: " <> show other)

  it "refuses a league id that already exists" $ \pool -> do
    cleanDatabase pool
    first  <- runStack pool 1 (runSimulation (config "sim-c"))
    second <- runStack pool 2 (runSimulation (config "sim-c"))
    either (const False) (const True) first `shouldBe` True
    second `shouldBe` Left (SimLeagueSetupFailed (LeagueIdTaken "sim-c"))

  it "rejects a team count outside the supported range" $ \pool -> do
    cleanDatabase pool
    result <- runStack pool 1 (runSimulation (config "sim-d") { scTeamCount = 1 })
    result `shouldBe` Left (SimTeamCountOutOfRange 1 2 (length teamNamePool))

  it "reports a window with no games instead of scoring nothing" $ \pool -> do
    cleanDatabase pool
    result <- runStack pool 1 $ runSimulation (config "sim-e")
      { scFrom = fromGregorian 2025 1 1
      , scTo   = fromGregorian 2025 1 2
      }
    -- There is no schedule fixture for January, so the fetch itself fails.
    case result of
      Left (SimScheduleFetchFailed _) -> pure ()
      Left (SimNoGames _ _)           -> pure ()
      other -> expectationFailure ("expected a schedule failure; got " <> show other)

-- ---------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------

rangeStart, rangeEnd :: Day
rangeStart = fromGregorian 2025 4 1
rangeEnd   = fromGregorian 2025 4 7

config :: Text -> SimConfig
config tag = SimConfig
  { scLeagueId  = tag
  , scSeason    = 2025
  , scFrom      = rangeStart
  , scTo        = rangeEnd
  , scTeamCount = 4
  }

-- | Rows per slot name.
slotCounts :: [Text] -> Map.Map Text Int
slotCounts slots = Map.fromListWith (+) [(slot, 1) | slot <- slots]

-- | The per-slot counts a full roster or lineup must show: one entry
-- for every slot whose limit is above zero.
expectedCounts :: (RosterSlot -> Int) -> Map.Map Text Int
expectedCounts limitFor = Map.fromList
  [ (renderRosterSlot slot, limitFor slot)
  | slot <- allRosterSlots
  , limitFor slot > 0
  ]

rosterLimit, lineupLimit :: RosterSlot -> Int
rosterLimit slot = rosterLimitFor slot standardRosterLimits
lineupLimit slot = lineupLimitFor slot standardLineupLimits

-- ---------------------------------------------------------------------
-- Effect stack
-- ---------------------------------------------------------------------

type SimStack =
  '[ Random
   , Players
   , Teams
   , Games
   , BoxscoreEntry
   , FetchLog
   , LS.LineupSlot
   , LineupSnapshot
   , RS.RosterSlot
   , LC.LeagueConfig
   , LeagueTeam
   , PlayerRanking
   , DraftPick
   , MLBClient
   , Database
   , Clock
   , Logging
   , Error DBError
   , IOE
   ]

runStack :: Pool -> Int -> Eff SimStack a -> IO a
runStack pool seed =
    runEffectsOrFail
  . runEff
  . runErrorNoCallStack @DBError
  . runLoggingDiscard
  . runClockFixed fixedTime
  . runDatabasePool pool
  . runMLBClientFixture simFixture
  . runDraftPickDB
  . runPlayerRankingDB
  . runLeagueTeamDB
  . LC.runLeagueConfigDB
  . RS.runRosterSlotDB
  . runLineupSnapshotDB
  . LS.runLineupSlotDB
  . runFetchLogDB
  . runBoxscoreEntryDB
  . runGamesDB
  . runTeamsDB
  . runPlayersDB
  . runRandomSeeded seed

-- | The shared fixture directory with the 2025 rosters replaced by a
-- full-season snapshot. Schedule and boxscore files come from the
-- shared directory unchanged.
simFixture :: MLBFixture
simFixture = (defaultFixture fixturesDir)
  { fixtureTeams   = Map.singleton 2025 (fixturesDir <> "/teams-2025-full.json")
  , fixturePlayers = Map.singleton 2025 (fixturesDir <> "/players-2025-full.json")
  }

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2025 4 15) 0

fixturesDir :: FilePath
fixturesDir = "integration-test/fixtures/mlb"