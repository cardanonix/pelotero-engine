{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Pelotero.League.Setup
-- Description : League lifecycle around the draft: create, rank, set lineups, activate.
--
-- The steps a league goes through outside the draft loop itself:
--
-- 1. 'createLeague' validates a 'NewLeague' and inserts the config and
--    its teams with status @"draft"@.
-- 2. 'replaceRanking' stores a team's pre-draft preference list.
-- 3. "Pelotero.Draft.Run" runs the draft and fills @roster_slot@.
-- 4. 'setLineupFromRoster' fills a team's lineup from its roster.
-- 5. 'activateLeague' moves the league from @"draft"@ to @"active"@,
--    after which lineup snapshots and scoring apply to it.
--
-- Validation happens before the first write in each step, so a rejected
-- request leaves the database untouched.
module Pelotero.League.Setup
  ( -- * Creating a league
    NewLeague (..)
  , NewTeam (..)
  , CreatedLeague (..)
  , LeagueSetupError (..)
  , validateNewLeague
  , createLeague
    -- * Standard presets
  , standardScoring
  , standardRosterLimits
  , standardLineupLimits
    -- * Rankings
  , replaceRanking
    -- * Lineups
  , chooseLineup
  , setLineupFromRoster
    -- * Activation
  , activateLeague
  ) where

import           Data.Int                       (Int32)
import           Data.List                      (mapAccumL)
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (catMaybes, mapMaybe)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import           Data.Time                      (Day, UTCTime (..))
import           Effectful

import           Pelotero.DB.LeagueConfig
                     ( LeagueConfigRow (..)
                     , LoadedLeagueConfig (..)
                     )
import           Pelotero.DB.LeagueTeam         (LeagueTeamRow (..))
import           Pelotero.DB.LineupSlot         (LineupSlotRow (..))
import           Pelotero.DB.PlayerRanking      (PlayerRankingRow (..))
import           Pelotero.DB.RosterSlot         (RosterSlotRow (..))
import           Pelotero.Domain.Draft
                     ( DraftOrderStrategy
                     , renderDraftOrderStrategy
                     )
import           Pelotero.Domain.Id
                     ( DbLeagueConfigId
                     , DbLeagueTeamId
                     , DbPlayerId
                     )
import           Pelotero.Domain.League
                     ( LeagueStatus (..)
                     , parseLeagueStatus
                     , renderLeagueStatus
                     )
import           Pelotero.Domain.Roster
                     ( LineupLimits (..)
                     , RosterLimits (..)
                     , RosterSlot (..)
                     , allRosterSlots
                     , lineupLimitFor
                     , parseRosterSlot
                     , renderRosterSlot
                     , rosterLimitFor
                     )
import           Pelotero.Domain.Scoring
                     ( BattingMultipliers (..)
                     , LeagueScoring (..)
                     , PitchingMultipliers (..)
                     )
import           Pelotero.Effects.LeagueConfig  (LeagueConfig)
import qualified Pelotero.Effects.LeagueConfig  as LC
import           Pelotero.Effects.LeagueTeam    (LeagueTeam)
import qualified Pelotero.Effects.LeagueTeam    as LT
import           Pelotero.Effects.LineupSlot    (LineupSlot)
import qualified Pelotero.Effects.LineupSlot    as LS
import           Pelotero.Effects.PlayerRanking (PlayerRanking)
import qualified Pelotero.Effects.PlayerRanking as PR
import qualified Pelotero.Effects.RosterSlot    as RS

-- ---------------------------------------------------------------------
-- Creating a league
-- ---------------------------------------------------------------------

data NewTeam = NewTeam
  { ntKey   :: !Text
  , ntName  :: !Text
  , ntOwner :: !Text
  }
  deriving stock (Show, Eq)

data NewLeague = NewLeague
  { nlLeagueId     :: !Text
  , nlCommissioner :: !Text
  , nlScoring      :: !LeagueScoring
  , nlRosterLimits :: !RosterLimits
  , nlLineupLimits :: !LineupLimits
  , nlStrategy     :: !DraftOrderStrategy
  , nlScoringStart :: !Day
    -- ^ First day whose games count, inclusive.
  , nlScoringEnd   :: !Day
    -- ^ Last day whose games count, inclusive.
  , nlTeams        :: ![NewTeam]
  }
  deriving stock (Show, Eq)

data CreatedLeague = CreatedLeague
  { clLeague :: !DbLeagueConfigId
  , clTeams  :: ![DbLeagueTeamId]
    -- ^ In the order the teams were given in 'nlTeams'.
  }
  deriving stock (Show, Eq)

data LeagueSetupError
  = LeagueIdTaken          !Text
  | LeagueNeedsTwoTeams    !Int
  | DuplicateTeamKey       !Text
  | ScoringPeriodInverted  !Day !Day
  | NegativeRosterLimit    !RosterSlot !Int
  | LineupExceedsRoster    !RosterSlot !Int !Int
    -- ^ Slot, lineup limit, roster limit.
  | LeagueNotFound         !DbLeagueConfigId
  | LeagueNotInDraft       !DbLeagueConfigId !Text
  deriving stock (Show, Eq)

-- | Every problem with a 'NewLeague' that can be found without the
-- database. An empty list means the league is well formed.
validateNewLeague :: NewLeague -> [LeagueSetupError]
validateNewLeague nl = concat
  [ [ LeagueNeedsTwoTeams teamCount | teamCount < 2 ]
  , map DuplicateTeamKey duplicateKeys
  , [ ScoringPeriodInverted (nlScoringStart nl) (nlScoringEnd nl)
    | nlScoringEnd nl < nlScoringStart nl
    ]
  , [ NegativeRosterLimit slot limit
    | slot <- allRosterSlots
    , let limit = rosterLimitFor slot (nlRosterLimits nl)
    , limit < 0
    ]
  , [ LineupExceedsRoster slot lineupLimit rosterLimit
    | slot <- allRosterSlots
    , let lineupLimit = lineupLimitFor slot (nlLineupLimits nl)
          rosterLimit = rosterLimitFor slot (nlRosterLimits nl)
    , rosterLimit >= 0
    , lineupLimit > rosterLimit
    ]
  ]
  where
    teamCount     = length (nlTeams nl)
    keys          = map ntKey (nlTeams nl)
    duplicateKeys = Map.keys (Map.filter (> 1) (Map.fromListWith (+) [(k, 1 :: Int) | k <- keys]))

-- | Insert a league and its teams with status @"draft"@. Returns the
-- first validation problem, or 'LeagueIdTaken', without writing
-- anything when the request is not acceptable.
createLeague
  :: ( LeagueConfig :> es
     , LeagueTeam   :> es
     )
  => NewLeague
  -> Eff es (Either LeagueSetupError CreatedLeague)
createLeague nl = case validateNewLeague nl of
  err : _ -> pure (Left err)
  []      -> do
    existing <- LC.getByLeagueId (nlLeagueId nl)
    case existing of
      Just _  -> pure (Left (LeagueIdTaken (nlLeagueId nl)))
      Nothing -> do
        lcid  <- LC.insertLeagueConfig configRow
        teams <- traverse (LT.insertLeagueTeam . teamRow lcid) (nlTeams nl)
        pure (Right (CreatedLeague lcid teams))
  where
    configRow = LeagueConfigRow
      { lcId            = Nothing
      , lcLeagueId      = nlLeagueId nl
      , lcCommissioner  = nlCommissioner nl
      , lcStatus        = renderLeagueStatus LeagueDraft
      , lcScoring       = nlScoring nl
      , lcRosterLimits  = nlRosterLimits nl
      , lcLineupLimits  = nlLineupLimits nl
      , lcDraftAuto     = True
      , lcDraftStrategy = renderDraftOrderStrategy (nlStrategy nl)
      , lcDraftAutoAt   = Nothing
      , lcScoringStart  = UTCTime (nlScoringStart nl) 0
      , lcScoringEnd    = UTCTime (nlScoringEnd nl) 0
      }
    teamRow lcid t = LeagueTeamRow
      { ltId             = Nothing
      , ltLeagueConfigId = lcid
      , ltTeamKey        = ntKey t
      , ltName           = ntName t
      , ltOwner          = ntOwner t
      }

-- ---------------------------------------------------------------------
-- Standard presets
-- ---------------------------------------------------------------------

standardScoring :: LeagueScoring
standardScoring = LeagueScoring
  { lsBatting = BattingMultipliers
      { bmSingle = 1, bmDouble = 2, bmTriple = 3, bmHomeRun = 4
      , bmRbi = 1, bmRun = 1, bmBaseOnBalls = 1, bmStolenBase = 2
      , bmHitByPitch = 1, bmStrikeOut = -1, bmCaughtStealing = -1
      }
  , lsPitching = PitchingMultipliers
      { pmWin = 5, pmSave = 5, pmQualityStart = 4, pmInningPitched = 3
      , pmStrikeOut = 1, pmCompleteGame = 5, pmShutout = 5
      , pmBaseOnBalls = -1, pmHitsAllowed = 0, pmEarnedRun = -1
      , pmHitBatsman = -1, pmLoss = -3
      }
  }

-- | 25 players per team.
standardRosterLimits :: RosterLimits
standardRosterLimits = RosterLimits $ Map.fromList
  [ (SlotCatcher,         2)
  , (SlotFirstBase,       2)
  , (SlotSecondBase,      2)
  , (SlotThirdBase,       2)
  , (SlotShortstop,       2)
  , (SlotOutfield,        5)
  , (SlotUtility,         2)
  , (SlotStartingPitcher, 5)
  , (SlotReliefPitcher,   3)
  ]

-- | 15 active players per team.
standardLineupLimits :: LineupLimits
standardLineupLimits = LineupLimits $ Map.fromList
  [ (SlotCatcher,         1)
  , (SlotFirstBase,       1)
  , (SlotSecondBase,      1)
  , (SlotThirdBase,       1)
  , (SlotShortstop,       1)
  , (SlotOutfield,        3)
  , (SlotUtility,         1)
  , (SlotStartingPitcher, 4)
  , (SlotReliefPitcher,   2)
  ]

-- ---------------------------------------------------------------------
-- Rankings
-- ---------------------------------------------------------------------

-- | Store a team's preference list. The list order is the preference
-- order; rank slots are assigned 1, 2, 3, ... A player listed more than
-- once keeps only the first, highest, position.
replaceRanking
  :: PlayerRanking :> es
  => DbLeagueTeamId
  -> [DbPlayerId]
  -> Eff es ()
replaceRanking team players =
  PR.replaceRankings team (zipWith row [1 ..] (dedupe players))
  where
    row :: Int32 -> DbPlayerId -> PlayerRankingRow
    row rank pid = PlayerRankingRow
      { prLeagueTeamId = team
      , prPlayerId     = pid
      , prRankSlot     = rank
      }

-- | Remove later duplicates, keeping first occurrences in order.
dedupe :: Ord a => [a] -> [a]
dedupe = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member x seen = go seen xs
      | otherwise         = x : go (Set.insert x seen) xs

-- ---------------------------------------------------------------------
-- Lineups
-- ---------------------------------------------------------------------

-- | Choose a lineup from a roster: for each slot keep the first
-- @lineupLimitFor slot@ entries, preserving the input order. The result
-- is a sub-list of the input and never exceeds any lineup limit.
chooseLineup :: LineupLimits -> [(RosterSlot, a)] -> [(RosterSlot, a)]
chooseLineup limits = catMaybes . snd . mapAccumL step Map.empty
  where
    step taken entry@(slot, _)
      | used < lineupLimitFor slot limits = (Map.insert slot (used + 1) taken, Just entry)
      | otherwise                         = (taken, Nothing)
      where
        used = Map.findWithDefault (0 :: Int) slot taken

-- | Replace a team's lineup with the one 'chooseLineup' picks from its
-- current roster. Returns the number of lineup rows written.
setLineupFromRoster
  :: ( RS.RosterSlot :> es
     , LineupSlot    :> es
     )
  => LineupLimits
  -> DbLeagueTeamId
  -> Eff es Int
setLineupFromRoster limits team = do
  roster <- RS.getSlotsForTeam team
  let entries = mapMaybe (\r -> (,) <$> parseRosterSlot (rsSlot r) <*> Just (rsPlayerId r)) roster
      lineup  = [ LineupSlotRow
                    { lsLeagueTeamId = team
                    , lsSlot         = renderRosterSlot slot
                    , lsPlayerId     = pid
                    }
                | (slot, pid) <- chooseLineup limits entries
                ]
  LS.replaceTeamLineup team lineup
  pure (length lineup)

-- ---------------------------------------------------------------------
-- Activation
-- ---------------------------------------------------------------------

-- | Move a league from @"draft"@ to @"active"@. Refused when the league
-- does not exist or is in any other status.
activateLeague
  :: LeagueConfig :> es
  => DbLeagueConfigId
  -> Eff es (Either LeagueSetupError ())
activateLeague lcid = do
  mConfig <- LC.getById lcid
  case mConfig of
    Nothing -> pure (Left (LeagueNotFound lcid))
    Just config
      | parseLeagueStatus (llcStatus config) /= Just LeagueDraft ->
          pure (Left (LeagueNotInDraft lcid (llcStatus config)))
      | otherwise -> do
          LC.updateLeagueConfig lcid (toRow config)
            { lcStatus = renderLeagueStatus LeagueActive }
          pure (Right ())
  where
    toRow c = LeagueConfigRow
      { lcId            = Just (llcId c)
      , lcLeagueId      = llcLeagueId c
      , lcCommissioner  = llcCommissioner c
      , lcStatus        = llcStatus c
      , lcScoring       = llcScoring c
      , lcRosterLimits  = llcRosterLimits c
      , lcLineupLimits  = llcLineupLimits c
      , lcDraftAuto     = llcDraftAuto c
      , lcDraftStrategy = llcDraftStrategy c
      , lcDraftAutoAt   = llcDraftAutoAt c
      , lcScoringStart  = llcScoringStart c
      , lcScoringEnd    = llcScoringEnd c
      }
