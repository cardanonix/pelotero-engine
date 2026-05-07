{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}

module Pelotero.Sync.Boxscores
  ( BoxscoreSyncResult (..)
  , BoxscoreSyncError  (..)
  , syncBoxscores
  ) where

import           Control.Monad              (foldM)
import qualified Crypto.Hash.SHA256         as SHA256
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base16     as B16
import           Data.Int                   (Int32)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE

import           Effectful                  (Eff, (:>))

import           Pelotero.DB.BoxscoreEntry  (BattingRow (..), PitchingRow (..))
import           Pelotero.DB.FetchLog       (FetchLogRow (..))
import           Pelotero.DB.Provider       (ProviderName)
import           Pelotero.Domain.Id
                     ( DbGameId
                     , DbPlayerId
                     , DbTeamId
                     , GameId
                     , unGameId
                     )
import qualified Pelotero.Domain.Stats      as DStats
import qualified Pelotero.MLB.Convert       as Convert

import           Pelotero.Effects.BoxscoreEntry
                     (BoxscoreEntry, upsertBatting, upsertPitching)
import           Pelotero.Effects.FetchLog  (FetchLog, recordFetch)
import           Pelotero.Effects.Games     (Games, lookupGameByExternalId)
import           Pelotero.Effects.MLBClient (MLBClient, fetchBoxscoreRaw)
import           Pelotero.Effects.Players   (Players, lookupPlayerByExternalId)
import           Pelotero.Effects.Teams     (Teams, lookupTeamByExternalId)

import           Pelotero.Provider.ExternalId
                     ( externalIdFromGameId
                     , externalIdFromPlayerId
                     , externalIdFromTeamId
                     )

data BoxscoreSyncResult = BoxscoreSyncResult
  { boxGamesSeen        :: !Int
  , boxGamesProcessed   :: !Int
  , boxGamesSkipped     :: !Int
  , boxBattingUpserted  :: !Int
  , boxPitchingUpserted :: !Int
  , boxErrors           :: ![BoxscoreSyncError]
  , boxConvertWarnings  :: ![Convert.ConvertWarning]
  }
  deriving stock (Show, Eq)

data BoxscoreSyncError
  = -- | The 'GameId' wasn't in the local games table. Run schedule sync
    --   first, then retry.
    GameNotKnown !GameId
    -- | HTTP or transport failure from the 'MLBClient' effect.
  | FetchFailed  !GameId !String
    -- | Aeson decode failure on the raw bytes.
  | ParseFailed  !GameId !String
  deriving stock (Show, Eq)

syncBoxscores
  :: ( BoxscoreEntry :> es
     , Games         :> es
     , Players       :> es
     , Teams         :> es
     , MLBClient     :> es
     , FetchLog      :> es
     )
  => ProviderName
  -> [GameId]
  -> Eff es BoxscoreSyncResult
syncBoxscores provider gids = do
  outcomes <- traverse (syncOne provider) gids
  let processed = length [() | Right _ <- outcomes]
      skipped   = length [() | Left  _ <- outcomes]
      errors    = [e | Left e <- outcomes]
      bat       = sum    [b | Right (b, _, _) <- outcomes]
      pit       = sum    [p | Right (_, p, _) <- outcomes]
      warns     = concat [w | Right (_, _, w) <- outcomes]
  pure BoxscoreSyncResult
    { boxGamesSeen        = length gids
    , boxGamesProcessed   = processed
    , boxGamesSkipped     = skipped
    , boxBattingUpserted  = bat
    , boxPitchingUpserted = pit
    , boxErrors           = errors
    , boxConvertWarnings  = warns
    }

syncOne
  :: ( BoxscoreEntry :> es
     , Games         :> es
     , Players       :> es
     , Teams         :> es
     , MLBClient     :> es
     , FetchLog      :> es
     )
  => ProviderName
  -> GameId
  -> Eff es (Either BoxscoreSyncError (Int, Int, [Convert.ConvertWarning]))
syncOne provider gid = do
  let extId = externalIdFromGameId gid
  mDbGid <- lookupGameByExternalId provider extId
  case mDbGid of
    Nothing -> pure (Left (GameNotKnown gid))
    Just dbGid -> do
      eraw <- fetchBoxscoreRaw (unGameId gid)
      case eraw of
        Left err  -> pure (Left (FetchFailed gid err))
        Right raw ->
          case Aeson.eitherDecodeStrict raw of
            Left err -> pure (Left (ParseFailed gid err))
            Right wireBox -> do
              let (warns, entries) = Convert.convertBoxscore gid wireBox
              (b, p) <- upsertEntries provider dbGid entries
              recordFetch FetchLogRow
                { fetchLogId            = Nothing
                , fetchLogProvider      = provider
                , fetchLogResource      = "boxscore"
                , fetchLogScope         = extId
                , fetchLogFetchedAt     = Nothing
                , fetchLogPayloadSha256 = sha256Hex raw
                , fetchLogRecordCount   = fromIntegral (length entries)
                }
              pure (Right (b, p, warns))

upsertEntries
  :: ( BoxscoreEntry :> es
     , Players       :> es
     , Teams         :> es
     )
  => ProviderName
  -> DbGameId
  -> [Convert.BoxscoreEntry]
  -> Eff es (Int, Int)
upsertEntries provider dbGid = foldM step (0, 0)
  where
    step (b, p) entry = do
      let pidExt = externalIdFromPlayerId (Convert.boxPlayerId entry)
      mPid <- lookupPlayerByExternalId provider pidExt
      case mPid of
        Nothing    -> pure (b, p)
        Just dbPid -> do
          mTid <- case Convert.boxTeamId entry of
            Nothing  -> pure Nothing
            Just tid ->
              lookupTeamByExternalId provider (externalIdFromTeamId tid)
          dB <- case Convert.boxBatting entry of
            Nothing -> pure 0
            Just bs -> do
              upsertBatting (battingRowFor dbGid dbPid mTid bs)
              pure 1
          dP <- case Convert.boxPitching entry of
            Nothing -> pure 0
            Just ps -> do
              upsertPitching (pitchingRowFor dbGid dbPid mTid ps)
              pure 1
          pure (b + dB, p + dP)

battingRowFor
  :: DbGameId
  -> DbPlayerId
  -> Maybe DbTeamId
  -> DStats.BattingStats
  -> BattingRow
battingRowFor gid pid mTid s = BattingRow
  { battingGameId               = gid
  , battingPlayerId             = pid
  , battingTeamId               = mTid
  , battingGamesPlayed          = i32 (DStats.batGamesPlayed s)
  , battingPlateAppearances     = i32 (DStats.batPlateAppearances s)
  , battingAtBats               = i32 (DStats.batAtBats s)
  , battingRuns                 = i32 (DStats.batRuns s)
  , battingHits                 = i32 (DStats.batHits s)
  , battingDoubles              = i32 (DStats.batDoubles s)
  , battingTriples              = i32 (DStats.batTriples s)
  , battingHomeRuns             = i32 (DStats.batHomeRuns s)
  , battingRbi                  = i32 (DStats.batRbi s)
  , battingBaseOnBalls          = i32 (DStats.batBaseOnBalls s)
  , battingIntentionalWalks     = i32 (DStats.batIntentionalWalks s)
  , battingStrikeOuts           = i32 (DStats.batStrikeOuts s)
  , battingStolenBases          = i32 (DStats.batStolenBases s)
  , battingCaughtStealing       = i32 (DStats.batCaughtStealing s)
  , battingHitByPitch           = i32 (DStats.batHitByPitch s)
  , battingSacBunts             = i32 (DStats.batSacBunts s)
  , battingSacFlies             = i32 (DStats.batSacFlies s)
  , battingGroundIntoDoublePlay = i32 (DStats.batGroundIntoDoublePlay s)
  , battingGroundIntoTriplePlay = i32 (DStats.batGroundIntoTriplePlay s)
  , battingLeftOnBase           = i32 (DStats.batLeftOnBase s)
  , battingTotalBases           = i32 (DStats.batTotalBases s)
  , battingFlyOuts              = i32 (DStats.batFlyOuts s)
  , battingGroundOuts           = i32 (DStats.batGroundOuts s)
  , battingCatchersInterference = i32 (DStats.batCatchersInterference s)
  , battingPickoffs             = i32 (DStats.batPickoffs s)
  }

pitchingRowFor
  :: DbGameId
  -> DbPlayerId
  -> Maybe DbTeamId
  -> DStats.PitchingStats
  -> PitchingRow
pitchingRowFor gid pid mTid s = PitchingRow
  { pitchingGameId                 = gid
  , pitchingPlayerId               = pid
  , pitchingTeamId                 = mTid
  , pitchingGamesPlayed            = i32 (DStats.pitGamesPlayed s)
  , pitchingGamesStarted           = i32 (DStats.pitGamesStarted s)
  , pitchingGamesFinished          = i32 (DStats.pitGamesFinished s)
  , pitchingCompleteGames          = i32 (DStats.pitCompleteGames s)
  , pitchingShutouts               = i32 (DStats.pitShutouts s)
  , pitchingWins                   = i32 (DStats.pitWins s)
  , pitchingLosses                 = i32 (DStats.pitLosses s)
  , pitchingSaves                  = i32 (DStats.pitSaves s)
  , pitchingSaveOpportunities      = i32 (DStats.pitSaveOpportunities s)
  , pitchingHolds                  = i32 (DStats.pitHolds s)
  , pitchingBlownSaves             = i32 (DStats.pitBlownSaves s)
  , pitchingInningsPitchedOuts     = inningsPitchedOuts s
  , pitchingBattersFaced           = i32 (DStats.pitBattersFaced s)
  , pitchingNumberOfPitches        = i32 (DStats.pitNumberOfPitches s)
  , pitchingStrikes                = i32 (DStats.pitStrikes s)
  , pitchingBalls                  = i32 (DStats.pitBalls s)
  , pitchingHits                   = i32 (DStats.pitHits s)
  , pitchingDoubles                = i32 (DStats.pitDoubles s)
  , pitchingTriples                = i32 (DStats.pitTriples s)
  , pitchingHomeRuns               = i32 (DStats.pitHomeRuns s)
  , pitchingRuns                   = i32 (DStats.pitRuns s)
  , pitchingEarnedRuns             = i32 (DStats.pitEarnedRuns s)
  , pitchingStrikeOuts             = i32 (DStats.pitStrikeOuts s)
  , pitchingBaseOnBalls            = i32 (DStats.pitBaseOnBalls s)
  , pitchingIntentionalWalks       = i32 (DStats.pitIntentionalWalks s)
  , pitchingHitBatsmen             = i32 (DStats.pitHitBatsmen s)
  , pitchingWildPitches            = i32 (DStats.pitWildPitches s)
  , pitchingBalks                  = i32 (DStats.pitBalks s)
  , pitchingPickoffs               = i32 (DStats.pitPickoffs s)
  , pitchingFlyOuts                = i32 (DStats.pitFlyOuts s)
  , pitchingGroundOuts             = i32 (DStats.pitGroundOuts s)
  , pitchingAirOuts                = i32 (DStats.pitAirOuts s)
  , pitchingInheritedRunners       = i32 (DStats.pitInheritedRunners s)
  , pitchingInheritedRunnersScored = i32 (DStats.pitInheritedRunnersScored s)
  , pitchingStolenBases            = i32 (DStats.pitStolenBases s)
  , pitchingCaughtStealing         = i32 (DStats.pitCaughtStealing s)
  , pitchingAtBats                 = i32 (DStats.pitAtBats s)
  , pitchingRbi                    = i32 (DStats.pitRbi s)
  , pitchingSacBunts               = i32 (DStats.pitSacBunts s)
  , pitchingSacFlies               = i32 (DStats.pitSacFlies s)
  , pitchingCatchersInterference   = i32 (DStats.pitCatchersInterference s)
  , pitchingPassedBall             = i32 (DStats.pitPassedBall s)
  }

-- | Pre-Phase B.1 reconciliation: prefer the wire IP-string parse, fall
-- back to wire @outs@. After Phase B.1 this whole helper goes away —
-- @PitchingStats@ will carry only @pitOuts@ and the reconciliation
-- (with a 'WireFieldDiscrepancy' warning on disagreement) will live in
-- 'Pelotero.MLB.Convert.convertPitching'.
inningsPitchedOuts :: DStats.PitchingStats -> Maybe Int32
inningsPitchedOuts s =
  case DStats.pitInningsPitched s >>= DStats.parseInningsPitched of
    Just o  -> Just (fromIntegral o)
    Nothing -> fromIntegral <$> DStats.pitOuts s

i32 :: Maybe Int -> Maybe Int32
i32 = fmap fromIntegral

sha256Hex :: BS.ByteString -> T.Text
sha256Hex = TE.decodeUtf8 . B16.encode . SHA256.hash