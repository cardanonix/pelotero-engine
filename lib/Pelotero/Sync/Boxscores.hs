{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Pelotero.Sync.Boxscores
  ( BoxscoreSyncResult (..)
  , BoxscoreSyncError (..)
  , BoxscoreOutcome (..)
  , BoxscoreUpsertCounts (..)
  , syncBoxscores
  , syncBoxscoresForDateRange
  , syncOne
  , upsertEntries
  , battingRowFor
  , pitchingRowFor
  , sha256Hex
  ) where

import Control.Monad (foldM, when)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Int (Int32)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (Day)
import Effectful
import Katip (Severity (..))

import Pelotero.DB.BoxscoreEntry (BattingRow (..), PitchingRow (..))
import Pelotero.DB.FetchLog (FetchLogRow (..))
import Pelotero.DB.Game (LoadedGameRow (..))
import Pelotero.DB.Provider (ProviderName, renderProviderName)
import Pelotero.Domain.Id
  ( DbGameId
  , DbPlayerId
  , DbTeamId
  , GameId
  , unGameId
  )
import qualified Pelotero.Domain.Stats as DStats
import Pelotero.Effects.BoxscoreEntry (BoxscoreEntry)
import qualified Pelotero.Effects.BoxscoreEntry as Box
import Pelotero.Effects.FetchLog (FetchLog)
import qualified Pelotero.Effects.FetchLog as FetchLog
import Pelotero.Effects.Games (Games)
import qualified Pelotero.Effects.Games as Games
import Pelotero.Effects.Logging (Logging, logFM)
import Pelotero.Effects.MLBClient (MLBClient)
import qualified Pelotero.Effects.MLBClient as MLB
import Pelotero.Effects.Players (Players)
import qualified Pelotero.Effects.Players as Players
import Pelotero.Effects.Teams (Teams)
import qualified Pelotero.Effects.Teams as Teams
import qualified Pelotero.MLB.Convert as Convert
import Pelotero.Provider.ExternalId
  ( externalIdFromGameId
  , externalIdFromPlayerId
  , externalIdFromTeamId
  , externalIdToGameId
  )

-- | Per-game upsert counts. 'bucPlayersSkipped' is the number of
-- boxscore entries whose 'Convert.boxPlayerId' could not be resolved
-- against the local 'player_external_id' table; those entries were
-- logged at 'WarningS' and dropped. Operators monitor this to detect
-- when roster sync is falling behind boxscore sync at scale.
data BoxscoreUpsertCounts = BoxscoreUpsertCounts
  { bucBatting        :: !Int
  , bucPitching       :: !Int
  , bucPlayersSkipped :: !Int
  }
  deriving stock (Show, Eq)

emptyUpsertCounts :: BoxscoreUpsertCounts
emptyUpsertCounts = BoxscoreUpsertCounts 0 0 0

data BoxscoreOutcome
  = BoxUpserted !BoxscoreUpsertCounts ![Convert.ConvertWarning]
  | BoxUnchanged
  deriving stock (Show, Eq)

data BoxscoreSyncResult = BoxscoreSyncResult
  { boxGamesSeen        :: !Int
  , boxGamesProcessed   :: !Int
  , boxGamesUnchanged   :: !Int
  , boxBattingUpserted  :: !Int
  , boxPitchingUpserted :: !Int
  , boxPlayersSkipped   :: !Int
    -- ^ Total boxscore entries dropped because the player was not in
    --   the local DB. Symmetric to 'boxConvertWarnings' but for a
    --   different category (sync ordering, not wire data quality).
  , boxErrors           :: ![BoxscoreSyncError]
  , boxConvertWarnings  :: ![Convert.ConvertWarning]
  }
  deriving stock (Show, Eq)

data BoxscoreSyncError
  = GameNotKnown !GameId
  | FetchFailed  !GameId !String
  | ParseFailed  !GameId !String
  deriving stock (Show, Eq)

emptyResult :: BoxscoreSyncResult
emptyResult = BoxscoreSyncResult
  { boxGamesSeen        = 0
  , boxGamesProcessed   = 0
  , boxGamesUnchanged   = 0
  , boxBattingUpserted  = 0
  , boxPitchingUpserted = 0
  , boxPlayersSkipped   = 0
  , boxErrors           = []
  , boxConvertWarnings  = []
  }

boxscoreResource :: T.Text
boxscoreResource = "boxscore"

syncBoxscores
  :: ( BoxscoreEntry :> es
     , Games         :> es
     , Players       :> es
     , Teams         :> es
     , MLBClient     :> es
     , FetchLog      :> es
     , Logging       :> es
     )
  => ProviderName
  -> [GameId]
  -> Eff es BoxscoreSyncResult
syncBoxscores provider = foldM step emptyResult
  where
    step acc gid = folded acc <$> syncOne provider gid

    folded acc (Left err) = acc
      { boxGamesSeen = boxGamesSeen acc + 1
      , boxErrors    = boxErrors acc ++ [err]
      }
    folded acc (Right BoxUnchanged) = acc
      { boxGamesSeen      = boxGamesSeen acc + 1
      , boxGamesUnchanged = boxGamesUnchanged acc + 1
      }
    folded acc (Right (BoxUpserted counts warns)) = acc
      { boxGamesSeen        = boxGamesSeen acc + 1
      , boxGamesProcessed   = boxGamesProcessed acc + 1
      , boxBattingUpserted  = boxBattingUpserted acc + bucBatting counts
      , boxPitchingUpserted = boxPitchingUpserted acc + bucPitching counts
      , boxPlayersSkipped   = boxPlayersSkipped acc + bucPlayersSkipped counts
      , boxConvertWarnings  = boxConvertWarnings acc ++ warns
      }

-- | Sync boxscores for every game scheduled in the given inclusive
-- date range. Looks the games up via 'Games.getGamesByDateRange',
-- resolves each to its provider-side 'GameId' via
-- 'Games.getGameExternalId' and 'externalIdToGameId', and delegates
-- to 'syncBoxscores'. Games in the range that have no recorded
-- external id under the given provider are dropped with a single
-- WarningS log line naming the count; this is normal during initial
-- bootstrap of a new provider, abnormal during steady-state operation.
syncBoxscoresForDateRange
  :: ( BoxscoreEntry :> es
     , Games         :> es
     , Players       :> es
     , Teams         :> es
     , MLBClient     :> es
     , FetchLog      :> es
     , Logging       :> es
     )
  => ProviderName
  -> Day
  -> Day
  -> Eff es BoxscoreSyncResult
syncBoxscoresForDateRange provider from to_ = do
  games    <- Games.getGamesByDateRange from to_
  resolved <- traverse resolve games
  let gameIds = catMaybes resolved
      missing = length games - length gameIds
  when (missing > 0) $
    logFM WarningS $
      T.pack (show missing)
        <> " games in range have no "
        <> renderProviderName provider
        <> " external id; skipping"
  syncBoxscores provider gameIds
  where
    resolve g = do
      mExt <- Games.getGameExternalId (lgrId g) provider
      pure (mExt >>= externalIdToGameId)

syncOne
  :: ( BoxscoreEntry :> es
     , Games         :> es
     , Players       :> es
     , Teams         :> es
     , MLBClient     :> es
     , FetchLog      :> es
     , Logging       :> es
     )
  => ProviderName
  -> GameId
  -> Eff es (Either BoxscoreSyncError BoxscoreOutcome)
syncOne provider gid = do
  let extId = externalIdFromGameId gid
  mDb <- Games.lookupGameByExternalId provider extId
  case mDb of
    Nothing -> pure (Left (GameNotKnown gid))
    Just dbId -> do
      eBytes <- MLB.fetchBoxscoreRaw (unGameId gid)
      case eBytes of
        Left err -> pure (Left (FetchFailed gid err))
        Right rawBytes -> do
          let newSha = sha256Hex rawBytes
          prior <- FetchLog.getLastFetch provider boxscoreResource extId
          case prior of
            Just FetchLogRow { fetchLogPayloadSha256 = oldSha }
              | oldSha == newSha -> do
                  logFM InfoS $
                    "boxscore: payload unchanged, skipping; gameId="
                      <> extId <> " sha=" <> newSha
                  pure (Right BoxUnchanged)
            _ ->
              case Aeson.eitherDecodeStrict rawBytes of
                Left perr -> pure (Left (ParseFailed gid perr))
                Right wireBox -> do
                  let (warns, entries) = Convert.convertBoxscore gid wireBox
                  counts <- upsertEntries provider dbId entries
                  FetchLog.recordFetch FetchLogRow
                    { fetchLogId            = Nothing
                    , fetchLogProvider      = provider
                    , fetchLogResource      = boxscoreResource
                    , fetchLogScope         = extId
                    , fetchLogFetchedAt     = Nothing
                    , fetchLogPayloadSha256 = newSha
                    , fetchLogRecordCount   = fromIntegral (length entries)
                    }
                  pure (Right (BoxUpserted counts warns))

upsertEntries
  :: ( BoxscoreEntry :> es
     , Players       :> es
     , Teams         :> es
     , Logging       :> es
     )
  => ProviderName
  -> DbGameId
  -> [Convert.BoxscoreEntry]
  -> Eff es BoxscoreUpsertCounts
upsertEntries provider dbGameId = foldM step emptyUpsertCounts
  where
    step counts entry = do
      let pidExt = externalIdFromPlayerId (Convert.boxPlayerId entry)
      mPlayerDb <- Players.lookupPlayerByExternalId provider pidExt
      case mPlayerDb of
        Nothing -> do
          logFM WarningS $
            "boxscore: skipping unknown player; provider="
              <> renderProviderName provider
              <> " externalId=" <> pidExt
              <> " (player not yet synced)"
          pure counts { bucPlayersSkipped = bucPlayersSkipped counts + 1 }
        Just playerDb -> do
          mTeamDb <- case Convert.boxTeamId entry of
            Nothing -> pure Nothing
            Just t  ->
              Teams.lookupTeamByExternalId provider (externalIdFromTeamId t)
          case Convert.boxBatting entry of
            Just bs ->
              Box.upsertBatting (battingRowFor dbGameId playerDb mTeamDb bs)
            Nothing -> pure ()
          case Convert.boxPitching entry of
            Just ps ->
              Box.upsertPitching (pitchingRowFor dbGameId playerDb mTeamDb ps)
            Nothing -> pure ()
          let bumpBat = maybe 0 (const 1) (Convert.boxBatting entry)
              bumpPit = maybe 0 (const 1) (Convert.boxPitching entry)
          pure counts
            { bucBatting  = bucBatting counts  + bumpBat
            , bucPitching = bucPitching counts + bumpPit
            }

battingRowFor
  :: DbGameId
  -> DbPlayerId
  -> Maybe DbTeamId
  -> DStats.BattingStats
  -> BattingRow
battingRowFor gid pid tid bs = BattingRow
  { battingGameId               = gid
  , battingPlayerId             = pid
  , battingTeamId               = tid
  , battingGamesPlayed          = i32 (DStats.batGamesPlayed bs)
  , battingPlateAppearances     = i32 (DStats.batPlateAppearances bs)
  , battingAtBats               = i32 (DStats.batAtBats bs)
  , battingRuns                 = i32 (DStats.batRuns bs)
  , battingHits                 = i32 (DStats.batHits bs)
  , battingDoubles              = i32 (DStats.batDoubles bs)
  , battingTriples              = i32 (DStats.batTriples bs)
  , battingHomeRuns             = i32 (DStats.batHomeRuns bs)
  , battingRbi                  = i32 (DStats.batRbi bs)
  , battingBaseOnBalls          = i32 (DStats.batBaseOnBalls bs)
  , battingIntentionalWalks     = i32 (DStats.batIntentionalWalks bs)
  , battingStrikeOuts           = i32 (DStats.batStrikeOuts bs)
  , battingStolenBases          = i32 (DStats.batStolenBases bs)
  , battingCaughtStealing       = i32 (DStats.batCaughtStealing bs)
  , battingHitByPitch           = i32 (DStats.batHitByPitch bs)
  , battingSacBunts             = i32 (DStats.batSacBunts bs)
  , battingSacFlies             = i32 (DStats.batSacFlies bs)
  , battingGroundIntoDoublePlay = i32 (DStats.batGroundIntoDoublePlay bs)
  , battingGroundIntoTriplePlay = i32 (DStats.batGroundIntoTriplePlay bs)
  , battingLeftOnBase           = i32 (DStats.batLeftOnBase bs)
  , battingTotalBases           = i32 (DStats.batTotalBases bs)
  , battingFlyOuts              = i32 (DStats.batFlyOuts bs)
  , battingGroundOuts           = i32 (DStats.batGroundOuts bs)
  , battingCatchersInterference = i32 (DStats.batCatchersInterference bs)
  , battingPickoffs             = i32 (DStats.batPickoffs bs)
  }

pitchingRowFor
  :: DbGameId
  -> DbPlayerId
  -> Maybe DbTeamId
  -> DStats.PitchingStats
  -> PitchingRow
pitchingRowFor gid pid tid ps = PitchingRow
  { pitchingGameId                 = gid
  , pitchingPlayerId               = pid
  , pitchingTeamId                 = tid
  , pitchingGamesPlayed            = i32 (DStats.pitGamesPlayed ps)
  , pitchingGamesStarted           = i32 (DStats.pitGamesStarted ps)
  , pitchingGamesFinished          = i32 (DStats.pitGamesFinished ps)
  , pitchingCompleteGames          = i32 (DStats.pitCompleteGames ps)
  , pitchingShutouts               = i32 (DStats.pitShutouts ps)
  , pitchingWins                   = i32 (DStats.pitWins ps)
  , pitchingLosses                 = i32 (DStats.pitLosses ps)
  , pitchingSaves                  = i32 (DStats.pitSaves ps)
  , pitchingSaveOpportunities      = i32 (DStats.pitSaveOpportunities ps)
  , pitchingHolds                  = i32 (DStats.pitHolds ps)
  , pitchingBlownSaves             = i32 (DStats.pitBlownSaves ps)
  , pitchingInningsPitchedOuts     = i32 (DStats.pitOuts ps)
  , pitchingBattersFaced           = i32 (DStats.pitBattersFaced ps)
  , pitchingNumberOfPitches        = i32 (DStats.pitNumberOfPitches ps)
  , pitchingStrikes                = i32 (DStats.pitStrikes ps)
  , pitchingBalls                  = i32 (DStats.pitBalls ps)
  , pitchingHits                   = i32 (DStats.pitHits ps)
  , pitchingDoubles                = i32 (DStats.pitDoubles ps)
  , pitchingTriples                = i32 (DStats.pitTriples ps)
  , pitchingHomeRuns               = i32 (DStats.pitHomeRuns ps)
  , pitchingRuns                   = i32 (DStats.pitRuns ps)
  , pitchingEarnedRuns             = i32 (DStats.pitEarnedRuns ps)
  , pitchingStrikeOuts             = i32 (DStats.pitStrikeOuts ps)
  , pitchingBaseOnBalls            = i32 (DStats.pitBaseOnBalls ps)
  , pitchingIntentionalWalks       = i32 (DStats.pitIntentionalWalks ps)
  , pitchingHitBatsmen             = i32 (DStats.pitHitBatsmen ps)
  , pitchingWildPitches            = i32 (DStats.pitWildPitches ps)
  , pitchingBalks                  = i32 (DStats.pitBalks ps)
  , pitchingPickoffs               = i32 (DStats.pitPickoffs ps)
  , pitchingFlyOuts                = i32 (DStats.pitFlyOuts ps)
  , pitchingGroundOuts             = i32 (DStats.pitGroundOuts ps)
  , pitchingAirOuts                = i32 (DStats.pitAirOuts ps)
  , pitchingInheritedRunners       = i32 (DStats.pitInheritedRunners ps)
  , pitchingInheritedRunnersScored = i32 (DStats.pitInheritedRunnersScored ps)
  , pitchingStolenBases            = i32 (DStats.pitStolenBases ps)
  , pitchingCaughtStealing         = i32 (DStats.pitCaughtStealing ps)
  , pitchingAtBats                 = i32 (DStats.pitAtBats ps)
  , pitchingRbi                    = i32 (DStats.pitRbi ps)
  , pitchingSacBunts               = i32 (DStats.pitSacBunts ps)
  , pitchingSacFlies               = i32 (DStats.pitSacFlies ps)
  , pitchingCatchersInterference   = i32 (DStats.pitCatchersInterference ps)
  , pitchingPassedBall             = i32 (DStats.pitPassedBall ps)
  }

i32 :: Maybe Int -> Maybe Int32
i32 = fmap fromIntegral

sha256Hex :: BS.ByteString -> T.Text
sha256Hex = TE.decodeUtf8 . B16.encode . SHA256.hash