{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- | Unit tests for the auto-draft loop with every effect interpreted in
-- memory. No database: draft picks and roster rows accumulate in a
-- 'World' value that the properties inspect afterwards.
module Pelotero.Draft.RunSpec (spec) where

import           Data.List                      (nub, sort)
import qualified Data.Map.Strict                as Map
import           Data.Time                      (UTCTime (..), fromGregorian)
import           Effectful
import           Effectful.Dispatch.Dynamic     (interpret_)
import           Effectful.State.Static.Local   (State, gets, modify, runState)
import           Hedgehog                       (Gen, annotateShow, failure, forAll, (===))
import qualified Hedgehog.Gen                   as Gen
import qualified Hedgehog.Range                 as Range
import           Test.Hspec                     (Spec, describe, it, shouldBe)
import           Test.Hspec.Hedgehog            (hedgehog)

import           Pelotero.DB.DraftPick          (DraftPickRow (..))
import           Pelotero.DB.PlayerRanking      (PlayerRankingRow (..))
import           Pelotero.DB.RosterSlot         (RosterSlotRow (..))
import           Pelotero.Domain.Id
                     ( DbDraftPickId (..)
                     , DbLeagueConfigId (..)
                     , DbLeagueTeamId (..)
                     , DbPlayerId (..)
                     , DraftPickNumber (..)
                     )
import           Pelotero.Domain.Position       (Position (..))
import           Pelotero.Domain.Roster
                     ( RosterLimits (..)
                     , RosterSlot (..)
                     , allRosterSlots
                     , renderRosterSlot
                     , rosterLimitFor
                     , totalRosterSize
                     )
import           Pelotero.Draft
                     ( DraftError (..)
                     , DraftPickEntry (..)
                     , DraftPlan (..)
                     , DraftSummary (..)
                     )
import           Pelotero.Draft.Run             (AutoDraftError (..), runAutoDraft)
import           Pelotero.Effects.Clock         (Clock, runClockFixed)
import           Pelotero.Effects.DraftPick     (DraftPick (..))
import           Pelotero.Effects.Logging       (Logging, runLoggingDiscard)
import           Pelotero.Effects.PlayerRanking (PlayerRanking (..))

spec :: Spec
spec = describe "Pelotero.Draft.Run.runAutoDraft (in-memory)" $ do

  it "completes and persists a roster that matches the limits exactly" $
    hedgehog $ do
      scenario <- forAll genScenario
      let plan            = scPlan scenario
          (result, world) = runWorld (scRankings scenario) (runAutoDraft plan)
      case result of
        Left err -> annotateShow err >> failure
        Right summary -> do
          let picks  = dsPicks summary
              teams  = nub (map fst (dpOrder plan))
              limits = dpLimits plan
          length picks === length (dpOrder plan)
          -- One draft_pick row per pick, in pick order.
          map dpPlayerId (wPicks world) === map dpePlayer picks
          map dpPickNumber (wPicks world)
            === map (fromIntegral . unDraftPickNumber . dpePickNumber) picks
          -- One roster_slot row per pick, with the slot the machine chose.
          wRoster world ===
            [ RosterSlotRow (dpeTeam p) (renderRosterSlot (dpeSlot p)) (dpePlayer p)
            | p <- picks
            ]
          -- Every team's roster fills every slot exactly.
          sequence_
            [ length [ () | r <- wRoster world
                          , rsLeagueTeamId r == team
                          , rsSlot r == renderRosterSlot slot ]
                === rosterLimitFor slot limits
            | team <- teams
            , slot <- allRosterSlots
            ]
          -- Nobody is on two rosters.
          let players = map rsPlayerId (wRoster world)
          sort (nub players) === sort players

  it "follows a team's ranking when the ranked player fits" $ do
    let plan = DraftPlan
          { dpLeague = league
          , dpOrder  = [(team1, DraftPickNumber 1), (team2, DraftPickNumber 2)]
          , dpPool   = Map.fromList [(DbPlayerId p, Catcher) | p <- [1 .. 5]]
          , dpLimits = RosterLimits (Map.fromList [(SlotCatcher, 1)])
          }
        rankings = Map.fromList
          [ (team1, map DbPlayerId [4, 2])
          , (team2, map DbPlayerId [4, 5])
          ]
        (result, _) = runWorld rankings (runAutoDraft plan)
    fmap (map dpePlayer . dsPicks) result
      `shouldBe` Right [DbPlayerId 4, DbPlayerId 5]

  it "skips a ranked player who does not fit and takes the next one who does" $ do
    let plan = DraftPlan
          { dpLeague = league
          , dpOrder  = [(team1, DraftPickNumber 1), (team1, DraftPickNumber 2)]
          , dpPool   = Map.fromList
              [ (DbPlayerId 1, Catcher), (DbPlayerId 2, Catcher), (DbPlayerId 3, Pitcher) ]
          , dpLimits = RosterLimits
              (Map.fromList [(SlotCatcher, 1), (SlotStartingPitcher, 1)])
          }
        rankings    = Map.fromList [(team1, map DbPlayerId [1, 2, 3])]
        (result, _) = runWorld rankings (runAutoDraft plan)
    fmap (map (\p -> (dpePlayer p, dpeSlot p)) . dsPicks) result
      `shouldBe` Right
        [ (DbPlayerId 1, SlotCatcher)
        , (DbPlayerId 3, SlotStartingPitcher)
        ]

  it "falls back to the lowest-id fitting player when the ranking is empty" $ do
    let plan = DraftPlan
          { dpLeague = league
          , dpOrder  = [(team1, DraftPickNumber 1)]
          , dpPool   = Map.fromList [(DbPlayerId 9, Catcher), (DbPlayerId 3, Catcher)]
          , dpLimits = RosterLimits (Map.fromList [(SlotCatcher, 1)])
          }
        (result, _) = runWorld Map.empty (runAutoDraft plan)
    fmap (map dpePlayer . dsPicks) result `shouldBe` Right [DbPlayerId 3]

  it "refuses a plan the pool cannot complete before making any pick" $ do
    -- Two catcher slots and one catcher: only one of the two picks can
    -- be placed, so the plan is refused at StartDraft and nothing is
    -- written.
    let plan = DraftPlan
          { dpLeague = league
          , dpOrder  = [(team1, DraftPickNumber 1), (team2, DraftPickNumber 2)]
          , dpPool   = Map.fromList [(DbPlayerId 1, Catcher), (DbPlayerId 2, Pitcher)]
          , dpLimits = RosterLimits (Map.fromList [(SlotCatcher, 1)])
          }
        (result, world) = runWorld Map.empty (runAutoDraft plan)
    result `shouldBe` Left (AutoDraftRejected (PlanInfeasible 1 2))
    (wPicks world, wRoster world) `shouldBe` ([], [])

  it "skips a ranked player who fits but would leave another team short" $ do
    -- Each team needs a catcher and a utility player, and the pool has
    -- exactly two catchers. team1 ranks both catchers first. Taking the
    -- second one into its utility slot would leave team2 no catcher, so
    -- team1 takes a designated hitter there instead.
    let plan = DraftPlan
          { dpLeague = league
          , dpOrder  = [ (team1, DraftPickNumber 1), (team1, DraftPickNumber 2)
                       , (team2, DraftPickNumber 3), (team2, DraftPickNumber 4)
                       ]
          , dpPool   = Map.fromList
              [ (DbPlayerId 1, Catcher), (DbPlayerId 2, Catcher)
              , (DbPlayerId 3, DesignatedHitter), (DbPlayerId 4, DesignatedHitter)
              ]
          , dpLimits = RosterLimits (Map.fromList [(SlotCatcher, 1), (SlotUtility, 1)])
          }
        rankings    = Map.fromList [(team1, map DbPlayerId [1, 2, 3, 4])]
        (result, _) = runWorld rankings (runAutoDraft plan)
    fmap (map (\p -> (dpePlayer p, dpeSlot p)) . dsPicks) result
      `shouldBe` Right
        [ (DbPlayerId 1, SlotCatcher)
        , (DbPlayerId 3, SlotUtility)
        , (DbPlayerId 2, SlotCatcher)
        , (DbPlayerId 4, SlotUtility)
        ]

  it "writes nothing when the plan is refused" $ do
    let plan = DraftPlan
          { dpLeague = league
          , dpOrder  = []
          , dpPool   = Map.empty
          , dpLimits = RosterLimits Map.empty
          }
        (result, world) = runWorld Map.empty (runAutoDraft plan)
    either (const True) (const False) result `shouldBe` True
    (wPicks world, wRoster world) `shouldBe` ([], [])

-- ---------------------------------------------------------------------
-- In-memory world
-- ---------------------------------------------------------------------

data World = World
  { wPicks  :: [DraftPickRow]
    -- ^ In insertion order.
  , wRoster :: [RosterSlotRow]
    -- ^ In insertion order.
  }

type WorldEffects = '[DraftPick, PlayerRanking, Clock, Logging, State World]

-- | Run a draft action against in-memory tables. Rankings are fixed
-- up front; picks and roster rows are collected in the returned 'World'.
runWorld :: Map.Map DbLeagueTeamId [DbPlayerId] -> Eff WorldEffects a -> (a, World)
runWorld rankings =
    runPureEff
  . runState (World [] [])
  . runLoggingDiscard
  . runClockFixed (UTCTime (fromGregorian 2025 4 1) 0)
  . runRankingsFixed rankings
  . runPicksInMemory

runPicksInMemory :: State World :> es => Eff (DraftPick : es) a -> Eff es a
runPicksInMemory = interpret_ $ \case
  RecordPick row -> do
    n <- gets (length . wPicks)
    modify (\w -> w { wPicks = wPicks w ++ [row] })
    pure (DbDraftPickId (fromIntegral n + 1))
  RecordPickWithSlot row slot -> do
    n <- gets (length . wPicks)
    modify (\w -> w { wPicks = wPicks w ++ [row], wRoster = wRoster w ++ [slot] })
    pure (DbDraftPickId (fromIntegral n + 1))
  GetPicksForLeague lcid ->
    gets (filter ((== lcid) . dpLeagueConfigId) . wPicks)
  GetPickCount lcid ->
    gets (fromIntegral . length . filter ((== lcid) . dpLeagueConfigId) . wPicks)

runRankingsFixed
  :: Map.Map DbLeagueTeamId [DbPlayerId]
  -> Eff (PlayerRanking : es) a
  -> Eff es a
runRankingsFixed rankings = interpret_ $ \case
  GetRankingsForTeam team ->
    pure (zipWith (\rank pid -> PlayerRankingRow team pid rank) [1 ..] (rankingOf team))
  ReplaceRankings _ _ -> pure ()
  ClearRankings _     -> pure ()
  GetRankingCount team ->
    pure (fromIntegral (length (rankingOf team)))
  where
    rankingOf team = Map.findWithDefault [] team rankings

-- ---------------------------------------------------------------------
-- Fixtures and generators
-- ---------------------------------------------------------------------

league :: DbLeagueConfigId
league = DbLeagueConfigId 1

team1, team2 :: DbLeagueTeamId
team1 = DbLeagueTeamId 1
team2 = DbLeagueTeamId 2

data Scenario = Scenario
  { scPlan     :: DraftPlan
  , scRankings :: Map.Map DbLeagueTeamId [DbPlayerId]
  }
  deriving stock (Show)

-- | A plan that can always be completed (see the pool-size argument in
-- "Pelotero.Draft.MachineSpec") with a random, possibly partial,
-- ranking for every team. Partial rankings exercise the fallback path.
genScenario :: Gen Scenario
genScenario = do
  teamCount <- Gen.int (Range.linear 2 5)
  counts    <- traverse (const (Gen.int (Range.linear 0 maxSlotLimit))) allRosterSlots
  let generated = RosterLimits (Map.fromList (zip allRosterSlots counts))
      limits
        | totalRosterSize generated == 0 = RosterLimits (Map.fromList [(SlotUtility, 1)])
        | otherwise                      = generated
      teams       = [DbLeagueTeamId (fromIntegral t) | t <- [1 .. teamCount]]
      order       = zipWith (\n t -> (t, DraftPickNumber n))
                            [1 ..]
                            (concat (replicate (totalRosterSize limits) teams))
      perPosition = teamCount * (maxSlotLimit + 2)
      positions   = concatMap (replicate perPosition) [minBound .. maxBound :: Position]
      pool        = Map.fromList (zip (map DbPlayerId [1 ..]) positions)
  rankings <- traverse (const (genRanking (Map.keys pool))) teams
  pure Scenario
    { scPlan     = DraftPlan league order pool limits
    , scRankings = Map.fromList (zip teams rankings)
    }
  where
    maxSlotLimit = 2
    genRanking players = do
      shuffled <- Gen.shuffle players
      keep     <- Gen.int (Range.linear 0 (length players))
      pure (take keep shuffled)