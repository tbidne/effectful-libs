{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Provides a dynamic effect for typed process.
--
-- @since 0.1
module Effectful.Process.Typed.Dynamic
  ( -- * Effect
    TypedProcess (..),

    -- ** Handler
    runTypedProcess,

    -- * Types
    ProcessConfig,
    StreamSpec,
    StreamType (..),
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
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode)
import System.Process.Typed
  ( Process,
    ProcessConfig,
    StreamSpec,
    StreamType (STInput, STOutput),
  )
import System.Process.Typed qualified as P
import System.Process.Typed qualified as TP

-- NOTE: This could be implemented in terms of the static effect, defined in
-- typed-process-effectful. But not doing so saves us a dependency, so...

-- | Dynamic effect for typed process.
--
-- @since 0.1
data TypedProcess :: Effect where
  RunProcess :: ProcessConfig stdin stdout stderr -> TypedProcess m ExitCode
  ReadProcess ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    TypedProcess m (ExitCode, BSL.ByteString, BSL.ByteString)
  ReadProcessStdout ::
    ProcessConfig stdin stdoutIgnored stderr ->
    TypedProcess m (ExitCode, BSL.ByteString)
  ReadProcessStderr ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    TypedProcess m (ExitCode, BSL.ByteString)
  ReadProcessInterleaved ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    TypedProcess m (ExitCode, BSL.ByteString)
  WithProcessWait ::
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    TypedProcess m a
  WithProcessTerm ::
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    TypedProcess m a
  StartProcess ::
    ProcessConfig stdin stdout stderr ->
    TypedProcess m (Process stdin stdout stderr)
  StopProcess :: Process stdin stdout stderr -> TypedProcess m ()
  RunProcess_ :: ProcessConfig stdin stdout stderr -> TypedProcess m ()
  ReadProcess_ ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    TypedProcess m (BSL.ByteString, BSL.ByteString)
  ReadProcessStdout_ ::
    ProcessConfig stdin stdoutIgnored stderr ->
    TypedProcess m BSL.ByteString
  ReadProcessStderr_ ::
    ProcessConfig stdin stdout stderrIgnored ->
    TypedProcess m BSL.ByteString
  ReadProcessInterleaved_ ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    TypedProcess m BSL.ByteString
  WithProcessWait_ ::
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    TypedProcess m a
  WithProcessTerm_ ::
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    TypedProcess m a
  WaitExitCode :: Process stdin stdout stderr -> TypedProcess m ExitCode
  GetExitCode :: Process stdin stdout stderr -> TypedProcess m (Maybe ExitCode)
  CheckExitCode :: Process stdin stdout stderr -> TypedProcess m ()

-- | @since 0.1
type instance DispatchOf TypedProcess = Dynamic

-- | Runs 'TypedProcess' in 'IO'.
--
-- @since 0.1
runTypedProcess ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (TypedProcess : es) a ->
  Eff es a
runTypedProcess = interpret $ \env -> \case
  RunProcess pc -> liftIO $ TP.runProcess pc
  ReadProcess pc -> liftIO $ TP.readProcess pc
  ReadProcessStdout pc -> liftIO $ TP.readProcessStdout pc
  ReadProcessStderr pc -> liftIO $ TP.readProcessStderr pc
  ReadProcessInterleaved pc -> liftIO $ TP.readProcessInterleaved pc
  WithProcessWait pc onProcess -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ TP.withProcessWait pc (runInIO . onProcess)
  WithProcessTerm pc onProcess -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ TP.withProcessTerm pc (runInIO . onProcess)
  StartProcess pc -> liftIO $ TP.startProcess pc
  StopProcess pc -> liftIO $ TP.stopProcess pc
  RunProcess_ pc -> liftIO $ TP.runProcess_ pc
  ReadProcess_ pc -> liftIO $ TP.readProcess_ pc
  ReadProcessStdout_ pc -> liftIO $ TP.readProcessStdout_ pc
  ReadProcessStderr_ pc -> liftIO $ TP.readProcessStderr_ pc
  ReadProcessInterleaved_ pc -> liftIO $ TP.readProcessInterleaved_ pc
  WithProcessWait_ pc onProcess -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ TP.withProcessWait_ pc (runInIO . onProcess)
  WithProcessTerm_ pc onProcess -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ TP.withProcessTerm_ pc (runInIO . onProcess)
  WaitExitCode pc -> liftIO $ TP.waitExitCode pc
  GetExitCode pc -> liftIO $ TP.getExitCode pc
  CheckExitCode pc -> liftIO $ TP.checkExitCode pc

-- | Lifted 'TP.runProcess'.
--
-- @since 0.1
runProcess ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdout stderr ->
  Eff es ExitCode
runProcess = send . RunProcess

-- | Lifted 'TP.readProcess'.
--
-- @since 0.1
readProcess ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es (ExitCode, BSL.ByteString, BSL.ByteString)
readProcess = send . ReadProcess

-- | Lifted 'TP.readProcessStdout'.
--
-- @since 0.1
readProcessStdout ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdoutIgnored stderr ->
  Eff es (ExitCode, BSL.ByteString)
readProcessStdout = send . ReadProcessStdout

-- | Lifted 'TP.readProcessStderr'.
--
-- @since 0.1
readProcessStderr ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es (ExitCode, BSL.ByteString)
readProcessStderr = send . ReadProcessStderr

-- | Lifted 'TP.readProcessInterleaved'.
--
-- @since 0.1
readProcessInterleaved ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es (ExitCode, BSL.ByteString)
readProcessInterleaved = send . ReadProcessInterleaved

-- | Lifted 'TP.withProcessWait'.
--
-- @since 0.1
withProcessWait ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessWait pc = send . WithProcessWait pc

-- | Lifted 'TP.withProcessTerm'.
--
-- @since 0.1
withProcessTerm ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessTerm pc = send . WithProcessTerm pc

-- | Lifted 'TP.startProcess'.
--
-- @since 0.1
startProcess ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdout stderr ->
  Eff es (Process stdin stdout stderr)
startProcess = send . StartProcess

-- | Lifted 'TP.stopProcess'.
--
-- @since 0.1
stopProcess ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  Process stdin stdout stderr ->
  Eff es ()
stopProcess = send . StopProcess

-- | Lifted 'TP.runProcess_'.
--
-- @since 0.1
runProcess_ ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdout stderr ->
  Eff es ()
runProcess_ = send . RunProcess_

-- | Lifted 'TP.readProcess_'.
--
-- @since 0.1
readProcess_ ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es (BSL.ByteString, BSL.ByteString)
readProcess_ = send . ReadProcess_

-- | Lifted 'TP.readProcessStdout_'.
--
-- @since 0.1
readProcessStdout_ ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdoutIgnored stderr ->
  Eff es BSL.ByteString
readProcessStdout_ = send . ReadProcessStdout_

-- | Lifted 'TP.readProcessStderr_'.
--
-- @since 0.1
readProcessStderr_ ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdout stderrIgnored ->
  Eff es BSL.ByteString
readProcessStderr_ = send . ReadProcessStderr_

-- | Lifted 'TP.readProcessInterleaved_'.
--
-- @since 0.1
readProcessInterleaved_ ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es BSL.ByteString
readProcessInterleaved_ = send . ReadProcessInterleaved_

-- | Lifted 'TP.withProcessWait_'.
--
-- @since 0.1
withProcessWait_ ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessWait_ pc = send . WithProcessWait_ pc

-- | Lifted 'TP.withProcessTerm_'.
--
-- @since 0.1
withProcessTerm_ ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessTerm_ pc = send . WithProcessTerm_ pc

-- | Lifted 'TP.waitExitCode'.
--
-- @since 0.1
waitExitCode ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  Process stdin stdout stderr ->
  Eff es ExitCode
waitExitCode = send . WaitExitCode

-- | Lifted 'TP.getExitCode'.
--
-- @since 0.1
getExitCode ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  Process stdin stdout stderr ->
  Eff es (Maybe ExitCode)
getExitCode = send . GetExitCode

-- | Lifted 'TP.checkExitCode'.
--
-- @since 0.1
checkExitCode ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  Process stdin stdout stderr ->
  Eff es ()
checkExitCode = send . CheckExitCode
