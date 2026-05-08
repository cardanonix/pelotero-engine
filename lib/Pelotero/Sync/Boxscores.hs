-- lib/Pelotero/Sync/Boxscores.hs
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Pelotero.Sync.Boxscores
  ( BoxscoreSyncResult (..)
  , BoxscoreSyncError (..)
  , BoxscoreOutcome (..)
  , syncBoxscores
  , syncOne
  , upsertEntries
  , battingRowFor
  , pitchingRowFor
  , sha256Hex
  ) where

import Control.Monad (foldM)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Int (Int32)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful
import Katip (Severity (..))

import Pelotero.DB.BoxscoreEntry (BattingRow (..), PitchingRow (..))
import Pelotero.DB.FetchLog (FetchLogRow (..))
import Pelotero.DB.Provider (ProviderName)
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
  )

-- | Outcome of processing one game in 'syncOne'.
--
-- The 'BoxUnchanged' constructor is the Phase C.1 short-circuit: the
-- fetched payload SHA matched the prior fetch and no parse / convert /
-- upsert / fetch-log work was done. 'BoxUpserted' means the full path
-- ran; the two Ints are batting and pitching row counts and the list
-- is the per-game ConvertWarning batch.
data BoxscoreOutcome
  = BoxUpserted !Int !Int ![Convert.ConvertWarning]
  | BoxUnchanged
  deriving stock (Show, Eq)

-- | Aggregate result across a batch of games. 'boxGamesUnchanged' is
-- the Phase C.1 SHA-skip counter; 'boxGamesProcessed' counts only games
-- whose full path ran. 'boxGamesSkipped' is preserved as a field for
-- backwards compatibility but is no longer incremented by this module
-- (the SHA-skip path increments 'boxGamesUnchanged' instead).
data BoxscoreSyncResult = BoxscoreSyncResult
  { boxGamesSeen        :: !Int
  , boxGamesProcessed   :: !Int
  , boxGamesUnchanged   :: !Int
  , boxGamesSkipped     :: !Int
  , boxBattingUpserted  :: !Int
  , boxPitchingUpserted :: !Int
  , boxErrors           :: ![BoxscoreSyncError]
  , boxConvertWarnings  :: ![Convert.ConvertWarning]
  }
  deriving stock (Show, Eq)

data BoxscoreSyncError
  = -- | The 'GameId' wasn't in the local games table. Run schedule
    --   sync first, then retry.
    GameNotKnown !GameId
  | -- | HTTP or transport failure from the 'MLBClient' effect.
    FetchFailed  !GameId !String
  | -- | Aeson decode failure on the raw bytes.
    ParseFailed  !GameId !String
  deriving stock (Show, Eq)

emptyResult :: BoxscoreSyncResult
emptyResult = BoxscoreSyncResult
  { boxGamesSeen        = 0
  , boxGamesProcessed   = 0
  , boxGamesUnchanged   = 0
  , boxGamesSkipped     = 0
  , boxBattingUpserted  = 0
  , boxPitchingUpserted = 0
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
    folded acc (Right (BoxUpserted bat pit warns)) = acc
      { boxGamesSeen        = boxGamesSeen acc + 1
      , boxGamesProcessed   = boxGamesProcessed acc + 1
      , boxBattingUpserted  = boxBattingUpserted acc + bat
      , boxPitchingUpserted = boxPitchingUpserted acc + pit
      , boxConvertWarnings  = boxConvertWarnings acc ++ warns
      }

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
              -- IMPORTANT: the FetchLog.recordFetch call below MUST
              -- remain the last step of this branch. Inverting it with
              -- the upserts would let a crash mid-upsert leave a
              -- fetch-log row that masks a partial state on re-run.
              case Aeson.eitherDecodeStrict rawBytes of
                Left perr -> pure (Left (ParseFailed gid perr))
                Right wireBox -> do
                  let (warns, entries) = Convert.convertBoxscore gid wireBox
                  (batCount, pitCount) <- upsertEntries provider dbId entries
                  FetchLog.recordFetch FetchLogRow
                    { fetchLogId            = Nothing
                    , fetchLogProvider      = provider
                    , fetchLogResource      = boxscoreResource
                    , fetchLogScope         = extId
                    , fetchLogFetchedAt     = Nothing
                    , fetchLogPayloadSha256 = newSha
                    , fetchLogRecordCount   = fromIntegral (length entries)
                    }
                  pure (Right (BoxUpserted batCount pitCount warns))

upsertEntries
  :: ( BoxscoreEntry :> es
     , Players       :> es
     , Teams         :> es
     )
  => ProviderName
  -> DbGameId
  -> [Convert.BoxscoreEntry]
  -> Eff es (Int, Int)
upsertEntries provider dbGameId = foldM step (0, 0)
  where
    step (bat, pit) entry = do
      let pidExt = externalIdFromPlayerId (Convert.boxPlayerId entry)
      mPlayerDb <- Players.lookupPlayerByExternalId provider pidExt
      case mPlayerDb of
        Nothing -> pure (bat, pit)
        Just playerDb -> do
          mTeamDb <- case Convert.boxTeamId entry of
            Nothing -> pure Nothing
            Just t  ->
              Teams.lookupTeamByExternalId provider (externalIdFromTeamId t)
          let bat' = bat + maybeOne (Convert.boxBatting entry)
              pit' = pit + maybeOne (Convert.boxPitching entry)
          case Convert.boxBatting entry of
            Just bs ->
              Box.upsertBatting (battingRowFor dbGameId playerDb mTeamDb bs)
            Nothing -> pure ()
          case Convert.boxPitching entry of
            Just ps ->
              Box.upsertPitching (pitchingRowFor dbGameId playerDb mTeamDb ps)
            Nothing -> pure ()
          pure (bat', pit')

    maybeOne :: Maybe a -> Int
    maybeOne Nothing  = 0
    maybeOne (Just _) = 1

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