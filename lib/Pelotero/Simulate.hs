{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Pelotero.Simulate
-- Description : End-to-end run of the whole engine on a randomised league.
--
-- 'runSimulation' exercises every stage in the order a real league goes
-- through them:
--
-- 1. sync teams and players for a season from the stats provider;
-- 2. sync the schedule for the scoring window;
-- 3. create a league with randomly named teams;
-- 4. give every team a random ranking of the whole draftable pool;
-- 5. run the auto-draft, which fills each team's roster;
-- 6. set each lineup from its roster and activate the league;
-- 7. snapshot lineups for every game in the window;
-- 8. sync boxscores for the window;
-- 9. score the league and pair the teams into matchups.
--
-- All randomness comes from the 'Random' effect, so a run is fully
-- determined by the seed given to its interpreter and the data the
-- provider returns.
--
-- The first failure stops the run and is returned as a 'SimError'.
-- Stages that completed before the failure have already written their
-- rows; the run is not one database transaction.
module Pelotero.Simulate
  ( SimConfig (..)
  , SimError (..)
  , SimReport (..)
  , runSimulation
  , teamNamePool
  ) where

import           Control.Monad                   (when)
import           Data.Foldable                   (for_, traverse_)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe, listToMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Time                       (Day, showGregorian)
import           Effectful
import           Effectful.Error.Static          (Error, runErrorNoCallStack, throwError)

import           Pelotero.DB.LeagueTeam          (LoadedLeagueTeam)
import           Pelotero.DB.Provider            (ProviderName (..))
import           Pelotero.Domain.Draft           (DraftOrderStrategy (..))
import           Pelotero.Domain.Id              (DbLeagueConfigId, DbLeagueTeamId)
import           Pelotero.Draft                  (DraftSummary)
import           Pelotero.Draft.Run
                     ( AutoDraftError
                     , draftablePool
                     , runAutoDraftForLeague
                     )
import           Pelotero.Effects.BoxscoreEntry  (BoxscoreEntry)
import           Pelotero.Effects.Clock          (Clock)
import           Pelotero.Effects.DraftPick      (DraftPick)
import           Pelotero.Effects.FetchLog       (FetchLog)
import           Pelotero.Effects.Games          (Games)
import qualified Pelotero.Effects.Games          as Games
import           Pelotero.Effects.LeagueConfig   (LeagueConfig)
import           Pelotero.Effects.LeagueTeam     (LeagueTeam)
import qualified Pelotero.Effects.LeagueTeam     as LT
import           Pelotero.Effects.LineupSlot     (LineupSlot)
import           Pelotero.Effects.LineupSnapshot (LineupSnapshot)
import           Pelotero.Effects.Logging
                     ( Logging
                     , Namespace (..)
                     , Severity (..)
                     , addNamespace
                     , logFM
                     )
import           Pelotero.Effects.MLBClient      (MLBClient)
import qualified Pelotero.Effects.MLBClient      as MLB
import           Pelotero.Effects.PlayerRanking  (PlayerRanking)
import           Pelotero.Effects.Players        (Players)
import qualified Pelotero.Effects.Players        as Players
import           Pelotero.Effects.Random         (Random)
import qualified Pelotero.Effects.Random         as Random
import           Pelotero.Effects.RosterSlot     (RosterSlot)
import           Pelotero.Effects.Teams          (Teams)
import           Pelotero.League.Setup
                     ( CreatedLeague (..)
                     , LeagueSetupError
                     , NewLeague (..)
                     , NewTeam (..)
                     , activateLeague
                     , createLeague
                     , replaceRanking
                     , setLineupFromRoster
                     , standardLineupLimits
                     , standardRosterLimits
                     , standardScoring
                     )
import qualified Pelotero.Lineup.Snapshot        as Snapshot
import           Pelotero.Matchup                (Matchup, matchupsFor)
import qualified Pelotero.MLB.Convert            as Convert
import           Pelotero.MLB.Fetch
                     ( FetchedRosters (..)
                     , FetchedSchedule (..)
                     )
import           Pelotero.Score                  (LeagueScore (..), TeamScore, scoreLeague)
import qualified Pelotero.Sync.Boxscores         as SyncBox
import qualified Pelotero.Sync.Players           as SyncPlayers
import qualified Pelotero.Sync.Schedule          as SyncSchedule

data SimConfig = SimConfig
  { scLeagueId  :: !Text
    -- ^ Unique @league_config.league_id@ for the league this run creates.
  , scSeason    :: !Int
    -- ^ Season whose rosters form the player pool.
  , scFrom      :: !Day
    -- ^ First scoring day, inclusive.
  , scTo        :: !Day
    -- ^ Last scoring day, inclusive.
  , scTeamCount :: !Int
  }
  deriving stock (Show, Eq)

data SimError
  = SimTeamCountOutOfRange !Int !Int !Int
    -- ^ Requested count, minimum, maximum.
  | SimRosterFetchFailed   !String
  | SimScheduleFetchFailed !String
  | SimNoGames             !Day !Day
    -- ^ The provider has no games in the scoring window, so there would
    --   be nothing to score.
  | SimLeagueSetupFailed   !LeagueSetupError
  | SimDraftFailed         !AutoDraftError
  | SimLeagueVanished      !DbLeagueConfigId
    -- ^ The league created earlier in this run could not be loaded for
    --   scoring. Only possible if another process deleted it.
  deriving stock (Show, Eq)

data SimReport = SimReport
  { srLeague            :: !DbLeagueConfigId
  , srTeams             :: ![LoadedLeagueTeam]
  , srPoolSize          :: !Int
  , srDraft             :: !DraftSummary
  , srLineupRows        :: !Int
  , srGames             :: !Int
  , srSnapshot          :: !Snapshot.SnapshotResult
  , srBoxscores         :: !SyncBox.BoxscoreSyncResult
  , srScore             :: !LeagueScore
  , srMatchups          :: ![Matchup]
  , srBye               :: !(Maybe TeamScore)
  }
  deriving stock (Show, Eq)

-- | Names handed out to simulated teams. Its length is the largest
-- league a simulation can create.
teamNamePool :: [Text]
teamNamePool =
  [ "Aces", "Barnstormers", "Clippers", "Dukes", "Express", "Foxes"
  , "Grays", "Hilltoppers", "Iron Pigs", "Jackals", "Knights", "Larks"
  , "Mudcats", "Naturals", "Owls", "Pilots"
  ]

runSimulation
  :: ( Players        :> es
     , Teams          :> es
     , Games          :> es
     , BoxscoreEntry  :> es
     , FetchLog       :> es
     , LineupSlot     :> es
     , LineupSnapshot :> es
     , RosterSlot     :> es
     , LeagueConfig   :> es
     , LeagueTeam     :> es
     , PlayerRanking  :> es
     , DraftPick      :> es
     , Clock          :> es
     , MLBClient      :> es
     , Logging        :> es
     , Random         :> es
     )
  => SimConfig
  -> Eff es (Either SimError SimReport)
runSimulation cfg =
  addNamespace (Namespace ["simulate"]) $ runErrorNoCallStack @SimError $ do
    let maxTeams = length teamNamePool
    when (scTeamCount cfg < 2 || scTeamCount cfg > maxTeams) $
      throwError (SimTeamCountOutOfRange (scTeamCount cfg) 2 maxTeams)

    syncRostersStage cfg
    gameCount <- syncScheduleStage cfg

    created <- createLeagueStage cfg
    let lcid = clLeague created
    teams <- LT.getForLeague lcid

    poolSize <- rankingStage (clTeams created)

    summary <- orThrow SimDraftFailed =<< runAutoDraftForLeague lcid

    lineupRows <- sum <$> traverse (setLineupFromRoster standardLineupLimits) (clTeams created)
    orThrow SimLeagueSetupFailed =<< activateLeague lcid
    logFM InfoS $ "league " <> tshow lcid <> " active with "
      <> tshow lineupRows <> " lineup rows"

    snapshot <- mconcat <$> traverse Snapshot.snapshotLineupsForDate [scFrom cfg .. scTo cfg]

    boxscores <- SyncBox.syncBoxscoresForDateRange ProviderMLB (scFrom cfg) (scTo cfg)
    logFM InfoS $ "boxscores: " <> tshow (SyncBox.boxGamesProcessed boxscores)
      <> " games processed, " <> tshow (length (SyncBox.boxErrors boxscores)) <> " errors"

    score <- maybe (throwError (SimLeagueVanished lcid)) pure =<< scoreLeague lcid
    let (matchups, bye) = matchupsFor (lscTeams score)

    pure SimReport
      { srLeague     = lcid
      , srTeams      = teams
      , srPoolSize   = poolSize
      , srDraft      = summary
      , srLineupRows = lineupRows
      , srGames      = gameCount
      , srSnapshot   = snapshot
      , srBoxscores  = boxscores
      , srScore      = score
      , srMatchups   = matchups
      , srBye        = bye
      }

-- ---------------------------------------------------------------------
-- Stages
-- ---------------------------------------------------------------------

syncRostersStage
  :: ( Players :> es, Teams :> es, FetchLog :> es, Clock :> es
     , MLBClient :> es, Logging :> es, Error SimError :> es
     )
  => SimConfig
  -> Eff es ()
syncRostersStage cfg = do
  fetched <- orThrow SimRosterFetchFailed =<< MLB.fetchRosters (scSeason cfg)
  traverse_ (logFM WarningS . Convert.renderWarning) (frWarnings fetched)
  result <- SyncPlayers.syncRosters
              ProviderMLB
              (tshow (scSeason cfg))
              (frPayloadSha fetched)
              (frTeams fetched)
              (frPlayers fetched)
  logFM InfoS $ "rosters: " <> tshow (SyncPlayers.syncTeamsUpserted result)
    <> " teams, " <> tshow (SyncPlayers.syncPlayersUpserted result) <> " players upserted"

-- | Sync the schedule for the window and return how many games the
-- database now holds for it.
syncScheduleStage
  :: ( Games :> es, Teams :> es, FetchLog :> es, Clock :> es
     , MLBClient :> es, Logging :> es, Error SimError :> es
     )
  => SimConfig
  -> Eff es Int
syncScheduleStage cfg = do
  let from  = showGregorian (scFrom cfg)
      to    = showGregorian (scTo cfg)
      scope = T.pack from <> ".." <> T.pack to
  fetched <- orThrow SimScheduleFetchFailed =<< MLB.fetchSchedule from to
  traverse_ (logFM WarningS . Convert.renderWarning) (fsWarnings fetched)
  _ <- SyncSchedule.syncSchedule ProviderMLB scope (fsPayloadSha fetched) (fsGames fetched)
  games <- Games.getGamesByDateRange (scFrom cfg) (scTo cfg)
  when (null games) $
    throwError (SimNoGames (scFrom cfg) (scTo cfg))
  logFM InfoS $ "schedule: " <> tshow (length games) <> " games in " <> scope
  pure (length games)

createLeagueStage
  :: ( LeagueConfig :> es, LeagueTeam :> es, Random :> es
     , Logging :> es, Error SimError :> es
     )
  => SimConfig
  -> Eff es CreatedLeague
createLeagueStage cfg = do
  names <- take (scTeamCount cfg) <$> Random.shuffle teamNamePool
  strategy <- pickFrom (SerpentineOrder :| [ExperimentalSnakeOrder])
  let newLeague = NewLeague
        { nlLeagueId     = scLeagueId cfg
        , nlCommissioner = "simulation"
        , nlScoring      = standardScoring
        , nlRosterLimits = standardRosterLimits
        , nlLineupLimits = standardLineupLimits
        , nlStrategy     = strategy
        , nlScoringStart = scFrom cfg
        , nlScoringEnd   = scTo cfg
        , nlTeams        = zipWith newTeam [1 :: Int ..] names
        }
  created <- orThrow SimLeagueSetupFailed =<< createLeague newLeague
  logFM InfoS $ "created league " <> scLeagueId cfg <> " (" <> tshow (clLeague created)
    <> ") with teams " <> T.intercalate ", " names
    <> " and draft strategy " <> tshow strategy
  pure created
  where
    newTeam i name = NewTeam
      { ntKey   = "team-" <> tshow i
      , ntName  = name
      , ntOwner = "owner-" <> tshow i
      }

-- | Give every team an independent random ranking of the whole
-- draftable pool. Returns the pool size.
rankingStage
  :: ( Players :> es, PlayerRanking :> es, Random :> es, Logging :> es )
  => [DbLeagueTeamId]
  -> Eff es Int
rankingStage teams = do
  -- Ascending id order, so the shuffles depend only on the seed and not
  -- on the order the database happens to return rows in.
  pool <- Map.keys . draftablePool <$> Players.getActivePlayers
  for_ teams $ \team ->
    replaceRanking team =<< Random.shuffle pool
  logFM InfoS $ "ranked " <> tshow (length pool) <> " draftable players for each of "
    <> tshow (length teams) <> " teams"
  pure (length pool)

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

orThrow :: Error SimError :> es => (e -> SimError) -> Either e a -> Eff es a
orThrow wrap = either (throwError . wrap) pure

-- | A uniformly chosen element of a non-empty list.
pickFrom :: Random :> es => NonEmpty a -> Eff es a
pickFrom (x :| xs) = do
  i <- Random.uniformInt (0, length xs)
  pure (fromMaybe x (listToMaybe (drop i (x : xs))))

tshow :: Show a => a -> Text
tshow = T.pack . show
