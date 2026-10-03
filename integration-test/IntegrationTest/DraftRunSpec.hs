{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : IntegrationTest.DraftRunSpec
--
-- End-to-end integration test for 'Pelotero.Draft.Run.runAutoDraftForLeague'.
--
-- Seeds a 2-team league with a 2-slot roster (so the draft is exactly
-- 4 picks), 4 active players, and per-team preference rankings whose
-- intersection forces every step of the auto-pick loop into a known
-- branch. With serpentine order over 4 picks the team order is
-- [T1, T2, T2, T1]; with the rankings below the predicted pick
-- sequence is A, B, C, D, and the test asserts both the in-memory
-- 'DraftSummary' and the persisted 'draft_pick' rows match.
--
-- This is the only end-to-end coverage for the draft path. The unit
-- tests in 'Pelotero.Draft.MachineSpec' cover the state machine in
-- isolation; this test covers config lookup, strategy parsing, team
-- enumeration, player enumeration, plan construction, the auto-pick
-- loop, pick persistence, roster persistence, and the refusal of a
-- second run as a single integrated chain.
module IntegrationTest.DraftRunSpec (spec) where

import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import           Data.Time
                     ( UTCTime (..)
                     , fromGregorian
                     , secondsToDiffTime
                     )
import           Test.Hspec

import           Effectful                       (Eff, IOE, runEff)
import           Effectful.Error.Static          (Error, runErrorNoCallStack)

import qualified Pelotero.DB.DraftPick           as DPRepo
import qualified Pelotero.DB.LeagueConfig        as LC
import qualified Pelotero.DB.LeagueTeam          as LT
import qualified Pelotero.DB.Player              as P
import           Pelotero.DB.DraftPick           (DraftPickRow (..))
import           Pelotero.DB.LeagueConfig        (LeagueConfigRow (..))
import           Pelotero.DB.Player              (PlayerRow (..))
import           Pelotero.DB.PlayerRanking       (PlayerRankingRow (..))
import           Pelotero.DB.RosterSlot          (RosterSlotRow (..))
import           Pelotero.DB.Pool                (DBError, Pool)
import           Pelotero.Domain.Id              (DraftPickNumber (..))
import           Pelotero.Domain.Roster          (RosterLimits (..), RosterSlot (..))
import           Pelotero.Draft
                     ( DraftPickEntry (..)
                     , DraftSummary (..)
                     )
import           Pelotero.Draft.Run
                     ( AutoDraftError (..)
                     , runAutoDraftForLeague
                     )

import           Pelotero.Effects.Clock          (Clock, runClockFixed)
import           Pelotero.Effects.Database       (Database, runDatabasePool, runTx)
import           Pelotero.Effects.DraftPick      (DraftPick, runDraftPickDB)
import           Pelotero.Effects.LeagueConfig   (LeagueConfig, runLeagueConfigDB)
import           Pelotero.Effects.LeagueTeam     (LeagueTeam, runLeagueTeamDB)
import           Pelotero.Effects.Logging        (Logging, runLoggingDiscard)
import qualified Pelotero.Effects.PlayerRanking  as PR
import           Pelotero.Effects.PlayerRanking  (PlayerRanking, runPlayerRankingDB)
import           Pelotero.Effects.Players        (Players, runPlayersDB)
import qualified Pelotero.Effects.RosterSlot     as RS

import           IntegrationTest.Fixtures
                     ( mkLeagueConfigRow
                     , mkLeagueTeamRow
                     , mkPlayerRow
                     )
import           IntegrationTest.Setup           (cleanDatabase, runEffectsOrFail)

-- ---------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------

spec :: SpecWith Pool
spec = describe "Pelotero.Draft.Run.runAutoDraftForLeague" $

  it "drives a serpentine auto-draft to completion and persists every pick"
     $ \pool -> do
    cleanDatabase pool

    seeded <- runStack pool $ do
      -- Seed a 2-team league with 2 slots per team (4 total picks).
      lcid  <- runTx (LC.insertLeagueConfigT testConfig)
      ltid1 <- runTx $ LT.insertLeagueTeamT (mkLeagueTeamRow lcid "draft-t1")
      ltid2 <- runTx $ LT.insertLeagueTeamT (mkLeagueTeamRow lcid "draft-t2")

      -- Seed 4 active designated hitters. A player needs a position to be
      -- draftable, and a DH fits the utility slots this league uses.
      pidA <- runTx (P.insertPlayerT (hitter "draft-a"))
      pidB <- runTx (P.insertPlayerT (hitter "draft-b"))
      pidC <- runTx (P.insertPlayerT (hitter "draft-c"))
      pidD <- runTx (P.insertPlayerT (hitter "draft-d"))

      -- Seed per-team rankings. rank_slot is 1-indexed (DB check
      -- constraint requires rank_slot > 0) and ASC = preferred-first.
      --   T1: A > B > C > D
      --   T2: B > A > C > D
      PR.replaceRankings ltid1
        [ PlayerRankingRow ltid1 pidA 1
        , PlayerRankingRow ltid1 pidB 2
        , PlayerRankingRow ltid1 pidC 3
        , PlayerRankingRow ltid1 pidD 4
        ]
      PR.replaceRankings ltid2
        [ PlayerRankingRow ltid2 pidB 1
        , PlayerRankingRow ltid2 pidA 2
        , PlayerRankingRow ltid2 pidC 3
        , PlayerRankingRow ltid2 pidD 4
        ]

      result         <- runAutoDraftForLeague lcid
      persistedPicks <- runTx (DPRepo.getPicksForLeagueT lcid)
      roster1        <- RS.getSlotsForTeam ltid1
      roster2        <- RS.getSlotsForTeam ltid2
      rerun          <- runAutoDraftForLeague lcid

      pure ( lcid, ltid1, ltid2, pidA, pidB, pidC, pidD
           , result, persistedPicks, roster1, roster2, rerun )

    let ( lcid, ltid1, ltid2, pidA, pidB, pidC, pidD
          , result, persistedPicks, roster1, roster2, rerun ) = seeded

    summary <- case result of
      Right s -> pure s
      Left err ->
        expectationFailure ("auto-draft failed: " <> show err)
          >> error "unreachable"

    -- The summary references the seeded league and has the predicted
    -- pick count (2 slots/team * 2 teams = 4).
    dsLeague summary           `shouldBe` lcid
    length (dsPicks summary)   `shouldBe` 4

    -- Predicted serpentine sequence. With 4 picks over [T1, T2]:
    --   order = [T1, T2, T2, T1]
    --   pick 1: T1, avail {A,B,C,D}, T1 top is A           -> A
    --   pick 2: T2, avail {B,C,D},   T2 top (B) available  -> B
    --   pick 3: T2, avail {C,D},     T2 ranks B,A,C,D, B/A gone, C next  -> C
    --   pick 4: T1, avail {D},       T1 ranks A,B,C,D, only D left       -> D
    map dpePickNumber (dsPicks summary)
      `shouldBe` map DraftPickNumber [1, 2, 3, 4]
    map dpeTeam       (dsPicks summary)
      `shouldBe` [ltid1, ltid2, ltid2, ltid1]
    map dpePlayer     (dsPicks summary)
      `shouldBe` [pidA, pidB, pidC, pidD]

    -- Every pick was persisted, ordered ASC by pick_number.
    length persistedPicks                `shouldBe` 4
    map dpPickNumber   persistedPicks    `shouldBe` [1, 2, 3, 4]
    map dpLeagueTeamId persistedPicks    `shouldBe` [ltid1, ltid2, ltid2, ltid1]
    map dpPlayerId     persistedPicks    `shouldBe` [pidA, pidB, pidC, pidD]
    map dpLeagueConfigId persistedPicks  `shouldBe` replicate 4 lcid

    -- Every pick was placed in the utility slot the limits provide.
    map dpeSlot (dsPicks summary) `shouldBe` replicate 4 SlotUtility

    -- Each pick also produced a roster_slot row for the picking team.
    -- getSlotsForTeam orders by slot then player id.
    roster1 `shouldBe`
      [ RosterSlotRow ltid1 "utility" pidA
      , RosterSlotRow ltid1 "utility" pidD
      ]
    roster2 `shouldBe`
      [ RosterSlotRow ltid2 "utility" pidB
      , RosterSlotRow ltid2 "utility" pidC
      ]

    -- A second run on the same league is refused before any write.
    rerun `shouldBe` Left (AutoDraftAlreadyHasPicks lcid 4)

-- ---------------------------------------------------------------------
-- Effect stack
-- ---------------------------------------------------------------------

type DraftStack =
  '[ DraftPick
   , RS.RosterSlot
   , PlayerRanking
   , Players
   , LeagueConfig
   , LeagueTeam
   , Database
   , Clock
   , Logging
   , Error DBError
   , IOE
   ]

runStack :: Pool -> Eff DraftStack a -> IO a
runStack pool =
    runEffectsOrFail
  . runEff
  . runErrorNoCallStack @DBError
  . runLoggingDiscard
  . runClockFixed fixedTime
  . runDatabasePool pool
  . runLeagueTeamDB
  . runLeagueConfigDB
  . runPlayersDB
  . runPlayerRankingDB
  . RS.runRosterSlotDB
  . runDraftPickDB

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2025 4 15) (secondsToDiffTime 0)

-- ---------------------------------------------------------------------
-- League config
-- ---------------------------------------------------------------------

testConfig :: LeagueConfigRow
testConfig = (mkLeagueConfigRow "draft-run-test")
  { lcStatus        = "draft"
  , lcDraftStrategy = "serpentine"
  , lcRosterLimits  = testRosterLimits
  , lcScoringStart  = utc (fromGregorian 2025 4 1)
  , lcScoringEnd    = utc (fromGregorian 2025 4 30)
  }
  where
    utc d = UTCTime d (secondsToDiffTime 0)

-- | Two utility slots per team: totalRosterSize = 2, so with 2 teams
-- the auto-draft runs exactly 4 picks.
testRosterLimits :: RosterLimits
testRosterLimits = RosterLimits $ Map.fromList
  [ (SlotUtility, 2)
  ]

-- | An active player whose position (DH) makes him draftable into a
-- utility slot.
hitter :: Text -> PlayerRow
hitter tag = (mkPlayerRow tag) { playerRowPosition = Just "DH" }
