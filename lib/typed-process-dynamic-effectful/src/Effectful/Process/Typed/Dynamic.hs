{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Dynamic effect for "System.Process.Typed". For static effects, see
-- https://hackage.haskell.org/package/typed-process-effectful-dynamic.
--
-- @since 0.1
module Effectful.Process.Typed.Dynamic
  ( -- * Effect
    TypedProcessDynamic (..),

    -- ** Handlers
    runTypedProcessDynamicIO,

    -- * Types
    ProcessConfig,
    StreamSpec,
    StreamType,
    Process,

    -- * ProcessConfig
    P.proc,
    P.shell,

    -- * Setters
    P.setStdin,
    P.setStdout,
    P.setStderr,
    P.setWorkingDir,
    P.setWorkingDirInherit,
    P.setEnv,
    P.setEnvInherit,
    P.setCloseFds,
    P.setCreateGroup,
    P.setDelegateCtlc,
    P.setDetachConsole,
    P.setCreateNewConsole,
    P.setNewSession,
#if !WINDOWS
    P.setChildGroup,
    P.setChildGroupInherit,
    P.setChildUser,
    P.setChildUserInherit,
#endif

    -- * Stream specs

    -- ** Built-in stream specs
    P.inherit,
    P.nullStream,
    P.closed,
    P.byteStringInput,
    P.byteStringOutput,
    P.createPipe,
    P.useHandleOpen,
    P.useHandleClose,

    -- ** Create your own stream spec
    P.mkStreamSpec,
    P.mkPipeStreamSpec,

    -- * Launch a process
    runProcess,
    readProcess,
    readProcessStdout,
    readProcessStderr,
    readProcessInterleaved,
    withProcessWait,
    withProcessTerm,
    startProcess,
    stopProcess,

    -- * Exception-throwing functions
    runProcess_,
    readProcess_,
    readProcessStdout_,
    readProcessStderr_,
    readProcessInterleaved_,
    withProcessWait_,
    withProcessTerm_,

    -- * Interact with a process

    -- ** Process exit code
    waitExitCode,
    P.waitExitCodeSTM,
    getExitCode,
    P.getExitCodeSTM,
    checkExitCode,
    P.checkExitCodeSTM,

    -- ** Process streams
    P.getStdin,
    P.getStdout,
    P.getStderr,

    -- * Exceptions
    P.ExitCodeException (..),
    P.ByteStringOutputException (..),

    -- * Re-exports
    P.ExitCode (..),
    P.StdStream (..),

    -- * Unsafe functions
    P.unsafeProcessHandle,
  )
where

{- ORMOLU_ENABLE -}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy qualified as BSL
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import System.Exit (ExitCode)
import System.Process.Typed
  ( Process,
    ProcessConfig,
    StreamSpec,
    StreamType,
  )
import System.Process.Typed qualified as P

-- NOTE: [Reimplementation vs. lifting]
--
-- In general, there is tension between reimplementing typed-process
-- (henceforth TP) functions vs. lifting them directly. On the one hand,
-- reimplementing means we can provide a shorter, simpler interface and
-- potentially make writing custom handlers easier. On the other hand,
-- reimplementing many (possibly complex) functions increases the possibility
-- of novel bugs, the maintenance burden, and can (conversely) make mocking
-- certain behavior harder.
--
-- Fortunately, typed-process's effectful API is not massive, and
-- most are sufficiently complicated that we do not attempt reimplementation.
-- The exceptions that we do reimplement are withProcessWait, withProcessWait_,
-- withProcessTerm, withProcessTerm_.
--
-- We implement these for two reasons:
--
-- 1. The implementation is trivial; they merely wrap other functions in
--    bracket.
-- 2. Reimplementation makes mocking easier, Continuation functions are
--    generally hard to mock, as we need to provide a natural transformation
--    (f a -> g a). By implementing these here, mocking uses e.g. the
--    startProcess/stopProcess functions, which are significantly easier to
--    mock.
--
-- Note that the usage of bracket here __is__ consistent as both typed-process
-- and Effectful.Exception use base's version, despite the former also
-- depending on UnliftIO (hence safe-exceptions).

-- | Dynamic effect for "System.Process.Typed".
--
-- @since 0.1
data TypedProcessDynamic :: Effect where
  RunProcess ::
    ProcessConfig stdin stdout stderr ->
    TypedProcessDynamic m ExitCode
  ReadProcess ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    TypedProcessDynamic m (ExitCode, BSL.ByteString, BSL.ByteString)
  ReadProcessStdout ::
    ProcessConfig stdin stdoutIgnored stderr ->
    TypedProcessDynamic m (ExitCode, BSL.ByteString)
  ReadProcessStderr ::
    ProcessConfig stdin stdout stderrIgnored ->
    TypedProcessDynamic m (ExitCode, BSL.ByteString)
  ReadProcessInterleaved ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    TypedProcessDynamic m (ExitCode, BSL.ByteString)
  WithProcessWait ::
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    TypedProcessDynamic m a
  WithProcessTerm ::
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    TypedProcessDynamic m a
  StartProcess ::
    ProcessConfig stdin stdout stderr ->
    TypedProcessDynamic m (Process stdin stdout stderr)
  StopProcess ::
    Process stdin stdout stderr ->
    TypedProcessDynamic m ()
  RunProcess_ ::
    ProcessConfig stdin stdout stderr ->
    TypedProcessDynamic m ()
  ReadProcess_ ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    TypedProcessDynamic m (BSL.ByteString, BSL.ByteString)
  ReadProcessStdout_ ::
    ProcessConfig stdin stdoutIgnored stderr ->
    TypedProcessDynamic m BSL.ByteString
  ReadProcessStderr_ ::
    ProcessConfig stdin stdout stderrIgnored ->
    TypedProcessDynamic m BSL.ByteString
  ReadProcessInterleaved_ ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    TypedProcessDynamic m BSL.ByteString
  WithProcessWait_ ::
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    TypedProcessDynamic m a
  WithProcessTerm_ ::
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    TypedProcessDynamic m a
  WaitExitCode ::
    Process stdin stdout stderr ->
    TypedProcessDynamic m ExitCode
  GetExitCode ::
    Process stdin stdout stderr ->
    TypedProcessDynamic m (Maybe ExitCode)
  CheckExitCode ::
    Process stdin stdout stderr ->
    TypedProcessDynamic m ()

-- | @since 0.1
type instance DispatchOf TypedProcessDynamic = Dynamic

-- | Runs 'TypedProcessDynamic' in 'IO'.
--
-- @since 0.1
runTypedProcessDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (TypedProcessDynamic : es) a ->
  Eff es a
runTypedProcessDynamicIO = interpret $ \env -> \case
  RunProcess pc -> liftIO $ P.runProcess pc
  ReadProcess pc -> liftIO $ P.readProcess pc
  ReadProcessStdout pc -> liftIO $ P.readProcessStdout pc
  ReadProcessStderr pc -> liftIO $ P.readProcessStderr pc
  ReadProcessInterleaved p -> liftIO $ P.readProcessInterleaved p
  WithProcessTerm pc onProcess -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ P.withProcessTerm pc (runInIO . onProcess)
  WithProcessWait pc onProcess -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ P.withProcessWait pc (runInIO . onProcess)
  StartProcess pc -> liftIO $ P.startProcess pc
  StopProcess p -> liftIO $ P.stopProcess p
  RunProcess_ pc -> liftIO $ P.runProcess_ pc
  ReadProcess_ pc -> liftIO $ P.readProcess_ pc
  ReadProcessStdout_ pc -> liftIO $ P.readProcessStdout_ pc
  ReadProcessStderr_ pc -> liftIO $ P.readProcessStderr_ pc
  ReadProcessInterleaved_ pc -> liftIO $ P.readProcessInterleaved_ pc
  WithProcessTerm_ pc onProcess -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ P.withProcessTerm_ pc (runInIO . onProcess)
  WithProcessWait_ pc onProcess -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ P.withProcessWait_ pc (runInIO . onProcess)
  WaitExitCode p -> liftIO $ P.waitExitCode p
  GetExitCode p -> liftIO $ P.getExitCode p
  CheckExitCode p -> liftIO $ P.checkExitCode p

-- | Lifted 'P.runProcess'.
--
-- @since 0.1
runProcess ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdout stderr ->
  Eff es ExitCode
runProcess = send . RunProcess

-- | Lifted 'P.readProcess'.
--
-- @since 0.1
readProcess ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es (ExitCode, BSL.ByteString, BSL.ByteString)
readProcess = send . ReadProcess

-- | Lifted 'P.readProcessStdout'.
--
-- @since 0.1
readProcessStdout ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdoutIgnored stderr ->
  Eff es (ExitCode, BSL.ByteString)
readProcessStdout = send . ReadProcessStdout

-- | Lifted 'P.runProcess'.
--
-- @since 0.1
readProcessStderr ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdout stderrIgnored ->
  Eff es (ExitCode, BSL.ByteString)
readProcessStderr = send . ReadProcessStderr

-- | Lifted 'P.readProcessInterleaved'.
--
-- @since 0.1
readProcessInterleaved ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es (ExitCode, BSL.ByteString)
readProcessInterleaved = send . ReadProcessInterleaved

-- | Lifted 'P.withProcessWait'.
--
-- @since 0.1
withProcessWait ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessWait pc = send . WithProcessWait pc

-- | Lifted 'P.withProcessTerm'.
--
-- @since 0.1
withProcessTerm ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessTerm pc = send . WithProcessTerm pc

-- | Lifted 'P.startProcess'.
--
-- @since 0.1
startProcess ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdout stderr ->
  Eff es (Process stdin stdout stderr)
startProcess = send . StartProcess

-- | Lifted 'P.stopProcess'.
--
-- @since 0.1
stopProcess ::
  (TypedProcessDynamic :> es) =>
  Process stdin stdout stderr ->
  Eff es ()
stopProcess = send . StopProcess

-- | Lifted 'P.runProcess_'.
--
-- @since 0.1
runProcess_ ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdout stderr ->
  Eff es ()
runProcess_ = send . RunProcess_

-- | Lifted 'P.readProcess_'.
--
-- @since 0.1
readProcess_ ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es (BSL.ByteString, BSL.ByteString)
readProcess_ = send . ReadProcess_

-- | Lifted 'P.readProcessStdout'.
--
-- @since 0.1
readProcessStdout_ ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdoutIgnored stderr ->
  Eff es BSL.ByteString
readProcessStdout_ = send . ReadProcessStdout_

-- | Lifted 'P.readProcess_'.
--
-- @since 0.1
readProcessStderr_ ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdout stderrIgnored ->
  Eff es BSL.ByteString
readProcessStderr_ = send . ReadProcessStderr_

-- | Lifted 'P.readProcessInterleaved'.
--
-- @since 0.1
readProcessInterleaved_ ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es BSL.ByteString
readProcessInterleaved_ = send . ReadProcessInterleaved_

-- | Lifted 'P.withProcessWait_'.
--
-- @since 0.1
withProcessWait_ ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessWait_ pc = send . WithProcessWait_ pc

-- | Lifted 'P.withProcessTerm_'.
--
-- @since 0.1
withProcessTerm_ ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessTerm_ pc = send . WithProcessTerm_ pc

-- | Lifted 'P.waitExitCode'.
--
-- @since 0.1
waitExitCode ::
  (TypedProcessDynamic :> es) =>
  Process stdin stdout stderr ->
  Eff es ExitCode
waitExitCode = send . WaitExitCode

-- | Lifted 'P.getExitCode'.
--
-- @since 0.1
getExitCode ::
  (TypedProcessDynamic :> es) =>
  Process stdin stdout stderr ->
  Eff es ExitCode
getExitCode = send . WaitExitCode

-- | Lifted 'P.checkExitCode'.
--
-- @since 0.1
checkExitCode ::
  (TypedProcessDynamic :> es) =>
  Process stdin stdout stderr ->
  Eff es ()
checkExitCode = send . CheckExitCode
