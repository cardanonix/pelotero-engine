{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Pelotero.Effects.Logging
-- Description : Structured-logging effect over Katip.
--
-- Two send operations ('logFM', 'logItem') and two scoping
-- operations ('addNamespace', 'addContext'). The scoping operations
-- push namespace and context around a sub-computation, mirroring
-- 'Katip.katipAddNamespace' / 'Katip.katipAddContext'; they do not
-- mutate ambient state.
--
-- Three interpreters:
--
--  * 'runLoggingKatip' wires the effect to a real 'LogEnv'. The
--    standard production scribe is JSON-to-stdout via
--    'withStdoutLogEnv'.
--
--  * 'runLoggingCapture' captures every emitted 'LogLine' into an
--    'IORef' for assertion in tests.
--
--  * 'runLoggingDiscard' throws every log line away. Use in tests
--    that don't assert on logs and don't want Katip output noise.
module Pelotero.Effects.Logging
  ( -- * Effect
    Logging
  , LogLine (..)
    -- * Send-only API
  , logFM
  , logItem
  , addNamespace
  , addContext
    -- * Re-exports for caller convenience
  , Severity (..)
  , Namespace (..)
  , LogEnv
  , Environment
  , SimpleLogPayload
  , sl
    -- * Production interpreter
  , runLoggingKatip
  , withStdoutLogEnv
    -- * Test interpreters
  , runLoggingCapture
  , runLoggingDiscard
  ) where

import           Control.Exception          (bracket)
import qualified Data.Aeson                 as Aeson
import           Data.IORef                 (IORef, atomicModifyIORef')
import           Data.Text                  (Text)
import           Effectful
import           Effectful.Dispatch.Dynamic
import           Effectful.Reader.Static    (ask, local, runReader)
import           Katip
  ( Severity (..)
  , Namespace (..)
  , LogEnv
  , LogContexts
  , Environment
  , SimpleLogPayload
  , Verbosity (..)
  , sl
  , logStr
  , liftPayload
  , toObject
  , runKatipContextT
  , initLogEnv
  , permitItem
  , registerScribe
  , defaultScribeSettings
  , closeScribes
  )
import qualified Katip                      as K
import           Katip.Scribes.Handle
  ( ColorStrategy (..)
  , jsonFormat
  , mkHandleScribeWithFormatter
  )
import           System.IO                  (stdout)

-- | The Logging effect. Higher-order in 'AddNamespace' and
-- 'AddLogContext': they scope a sub-computation rather than mutating
-- ambient state.
data Logging :: Effect where
  LogFM         :: Severity -> Text -> Logging m ()
  LogItemFM     :: SimpleLogPayload -> Severity -> Text -> Logging m ()
  AddNamespace  :: Namespace -> m a -> Logging m a
  AddLogContext :: SimpleLogPayload -> m a -> Logging m a

type instance DispatchOf Logging = Dynamic

-- | A captured log message. Equality is structural so tests can
-- match line-for-line.
data LogLine = LogLine
  { logLineSeverity  :: !Severity
  , logLineNamespace :: !Namespace
  , logLineMessage   :: !Text
  , logLineContext   :: !Aeson.Value
  }
  deriving stock (Show, Eq)

-- ===== Send-only API =====

logFM :: Logging :> es => Severity -> Text -> Eff es ()
logFM sev msg = send (LogFM sev msg)

logItem :: Logging :> es => SimpleLogPayload -> Severity -> Text -> Eff es ()
logItem payload sev msg = send (LogItemFM payload sev msg)

addNamespace :: Logging :> es => Namespace -> Eff es a -> Eff es a
addNamespace ns action = send (AddNamespace ns action)

addContext :: Logging :> es => SimpleLogPayload -> Eff es a -> Eff es a
addContext payload action = send (AddLogContext payload action)

-- ===== Production interpreter =====

data KatipReader = KatipReader
  { krLogEnv    :: !LogEnv
  , krContext   :: !LogContexts
  , krNamespace :: !Namespace
  }

-- | Wire the Logging effect to a Katip 'LogEnv'. Requires only
-- 'IOE'.
runLoggingKatip
  :: IOE :> es
  => LogEnv
  -> Eff (Logging : es) a
  -> Eff es a
runLoggingKatip logEnv =
  reinterpret (runReader (KatipReader logEnv mempty mempty)) $ \localEnv -> \case
    LogFM sev msg -> do
      KatipReader le ctx ns <- ask
      liftIO $ runKatipContextT le ctx ns $ K.logFM sev (logStr msg)
    LogItemFM payload sev msg -> do
      KatipReader le ctx ns <- ask
      let ctx' = ctx <> liftPayload payload
      liftIO $ runKatipContextT le ctx' ns $ K.logFM sev (logStr msg)
    AddNamespace newNs action ->
      localSeqUnlift localEnv $ \unlift ->
        local (\r -> r { krNamespace = krNamespace r <> newNs }) (unlift action)
    AddLogContext payload action ->
      localSeqUnlift localEnv $ \unlift ->
        local (\r -> r { krContext = krContext r <> liftPayload payload }) (unlift action)

-- | Bracket a JSON-to-stdout 'LogEnv'. The runtime is responsible
-- for rotation and persistence.
withStdoutLogEnv
  :: Namespace
  -> Environment
  -> Severity
  -> (LogEnv -> IO a)
  -> IO a
withStdoutLogEnv ns env minSev action = do
  initLE <- initLogEnv ns env
  scribe <- mkHandleScribeWithFormatter
              jsonFormat
              (ColorLog False)
              stdout
              (permitItem minSev)
              V2
  bracket
    (registerScribe "stdout" scribe defaultScribeSettings initLE)
    closeScribes
    action

-- ===== Test interpreters =====

data CaptureReader = CaptureReader
  { crContext   :: !LogContexts
  , crNamespace :: !Namespace
  }

-- | Capture every emitted log line into the given 'IORef'
-- (cons-prepended; 'reverse' on read for chronological order).
runLoggingCapture
  :: IOE :> es
  => IORef [LogLine]
  -> Eff (Logging : es) a
  -> Eff es a
runLoggingCapture ref =
  reinterpret (runReader (CaptureReader mempty mempty)) $ \localEnv -> \case
    LogFM sev msg -> do
      CaptureReader ctx ns <- ask
      liftIO $ atomicModifyIORef' ref $ \xs ->
        (LogLine sev ns msg (Aeson.Object (toObject ctx)) : xs, ())
    LogItemFM payload sev msg -> do
      CaptureReader ctx ns <- ask
      let ctx' = ctx <> liftPayload payload
      liftIO $ atomicModifyIORef' ref $ \xs ->
        (LogLine sev ns msg (Aeson.Object (toObject ctx')) : xs, ())
    AddNamespace newNs action ->
      localSeqUnlift localEnv $ \unlift ->
        local (\r -> r { crNamespace = crNamespace r <> newNs }) (unlift action)
    AddLogContext payload action ->
      localSeqUnlift localEnv $ \unlift ->
        local (\r -> r { crContext = crContext r <> liftPayload payload }) (unlift action)

-- | Discard every log line. No IO, no state, no allocation per
-- message. Use in tests that do not assert on logs.
runLoggingDiscard
  :: Eff (Logging : es) a
  -> Eff es a
runLoggingDiscard = interpret $ \localEnv -> \case
  LogFM _ _              -> pure ()
  LogItemFM _ _ _        -> pure ()
  AddNamespace _ action  ->
    localSeqUnlift localEnv $ \unlift -> unlift action
  AddLogContext _ action ->
    localSeqUnlift localEnv $ \unlift -> unlift action