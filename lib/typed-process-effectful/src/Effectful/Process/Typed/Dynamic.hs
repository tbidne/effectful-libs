{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Dynamic effect for "System.Process.Typed". For static effects, see
-- https://github.com/haskell-effectful/typed-process-effectful.
--
-- @since 0.1
module Effectful.Process.Typed.Dynamic
  ( -- * Effect
    TypedProcessDynamic (..),
    readProcessInterleaved,
    withProcessTerm,
    startProcess,
    stopProcess,
    readProcessInterleaved_,

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
    withProcessWait,

    -- * Exception-throwing functions
    runProcess_,
    readProcess_,
    readProcessStdout_,
    readProcessStderr_,
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
import Effectful.Concurrent.STM.Dynamic (STMDynamic (..), atomically)
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.Exception (bracket, finally)
import GHC.Conc (catchSTM, throwSTM)
import System.Exit (ExitCode)
import System.Process.Typed
  ( ExitCodeException (..),
    Process,
    ProcessConfig,
    StreamSpec,
    StreamType,
  )
import System.Process.Typed qualified as P

-- | Dynamic effect for "System.Process.Typed".
--
-- @since 0.1
data TypedProcessDynamic :: Effect where
  ReadProcessInterleaved ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    TypedProcessDynamic m (ExitCode, BSL.ByteString)
  WithProcessTerm ::
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    TypedProcessDynamic m a
  StartProcess ::
    ProcessConfig stdin stdout stderr ->
    TypedProcessDynamic m (Process stdin stdout stderr)
  StopProcess :: Process stdin stdout stderr -> TypedProcessDynamic m ()
  ReadProcessInterleaved_ ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    TypedProcessDynamic m BSL.ByteString

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
  ReadProcessInterleaved p -> liftIO $ P.readProcessInterleaved p
  WithProcessTerm pc onProcess -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ P.withProcessTerm pc (runInIO . onProcess)
  StartProcess pc -> liftIO $ P.startProcess pc
  StopProcess p -> liftIO $ P.stopProcess p
  ReadProcessInterleaved_ pc -> liftIO $ P.readProcessInterleaved_ pc

-- | Lifted 'P.readProcessInterleaved'.
--
-- @since 0.1
readProcessInterleaved ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es (ExitCode, BSL.ByteString)
readProcessInterleaved = send . ReadProcessInterleaved

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

-- | Lifted 'P.readProcessInterleaved'.
--
-- @since 0.1
readProcessInterleaved_ ::
  (TypedProcessDynamic :> es) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es BSL.ByteString
readProcessInterleaved_ = send . ReadProcessInterleaved_

-- | Run the given process, wait for it to exit, and returns its
-- 'ExitCode'.
--
-- @since 0.1
runProcess ::
  (TypedProcessDynamic :> es, STMDynamic :> es) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  Eff es ExitCode
runProcess pc = withProcessTerm pc waitExitCode
{-# INLINEABLE runProcess #-}

-- | Run a process, capture its standard output and error as a
-- 'BSL.ByteString', wait for it to complete, and then return its exit
-- code, output, and error.
--
-- Note that any previously used 'setStdout' or 'setStderr' will be
-- overridden.
--
-- @since 0.1
readProcess ::
  (TypedProcessDynamic :> es, STMDynamic :> es) =>
  -- | .
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es (ExitCode, BSL.ByteString, BSL.ByteString)
readProcess pc =
  withProcessTerm pc' $ \p ->
    atomically $
      (,,)
        <$> P.waitExitCodeSTM p
        <*> P.getStdout p
        <*> P.getStderr p
  where
    pc' =
      P.setStdout P.byteStringOutput $
        P.setStderr P.byteStringOutput pc
{-# INLINEABLE readProcess #-}

-- | Same as 'readProcess', but only read the stdout of the process.
-- Original settings for stderr remain.
--
-- @since 0.1
readProcessStdout ::
  (TypedProcessDynamic :> es, STMDynamic :> es) =>
  -- | .
  ProcessConfig stdin stdoutIgnored stderr ->
  Eff es (ExitCode, BSL.ByteString)
readProcessStdout pc =
  withProcessTerm pc' $ \p ->
    atomically $
      (,)
        <$> P.waitExitCodeSTM p
        <*> P.getStdout p
  where
    pc' = P.setStdout P.byteStringOutput pc
{-# INLINEABLE readProcessStdout #-}

-- | Same as 'readProcess', but only read the stderr of the process.
-- Original settings for stdout remain.
--
-- @since 0.1
readProcessStderr ::
  (TypedProcessDynamic :> es, STMDynamic :> es) =>
  -- | .
  ProcessConfig stdin stdout stderrIgnored ->
  Eff es (ExitCode, BSL.ByteString)
readProcessStderr pc =
  withProcessTerm pc' $ \p ->
    atomically $
      (,)
        <$> P.waitExitCodeSTM p
        <*> P.getStderr p
  where
    pc' = P.setStderr P.byteStringOutput pc
{-# INLINEABLE readProcessStderr #-}

-- | Uses the bracket pattern to call 'startProcess'. Unlike
-- 'withProcessTerm', this function will wait for the child process to
-- exit, and only kill it with 'stopProcess' in the event that the
-- inner function throws an exception.
--
-- To interact with a @Process@ use the functions from the section
-- [Interact with a process](#interactwithaprocess).
--
-- @since 0.1
withProcessWait ::
  (TypedProcessDynamic :> es, STMDynamic :> es) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessWait config f =
  bracket
    (startProcess config)
    stopProcess
    (\p -> f p <* waitExitCode p)
{-# INLINEABLE withProcessWait #-}

-- | Same as 'runProcess', but instead of returning the 'ExitCode', checks it
-- with 'checkExitCode'.
--
-- @since 0.1
runProcess_ ::
  (TypedProcessDynamic :> es, STMDynamic :> es) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  Eff es ()
runProcess_ pc = withProcessTerm pc checkExitCode
{-# INLINEABLE runProcess_ #-}

-- | Same as 'readProcess', but instead of returning the 'ExitCode',
-- checks it with 'checkExitCode'.
--
-- Exceptions thrown by this function will include stdout and stderr.
--
-- @since 0.1
readProcess_ ::
  (TypedProcessDynamic :> es, STMDynamic :> es) =>
  -- | .
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es (BSL.ByteString, BSL.ByteString)
readProcess_ pc =
  withProcessTerm pc' $ \p -> atomically $ do
    stdout <- P.getStdout p
    stderr <- P.getStderr p
    P.checkExitCodeSTM p `catchSTM` \ece ->
      throwSTM
        ece
          { eceStdout = stdout,
            eceStderr = stderr
          }
    return (stdout, stderr)
  where
    pc' =
      P.setStdout P.byteStringOutput $
        P.setStderr P.byteStringOutput pc
{-# INLINEABLE readProcess_ #-}

-- | Same as 'readProcessStdout', but instead of returning the
-- 'ExitCode', checks it with 'checkExitCode'.
--
-- Exceptions thrown by this function will include stdout.
--
-- @since 0.1
readProcessStdout_ ::
  (TypedProcessDynamic :> es, STMDynamic :> es) =>
  -- | .
  ProcessConfig stdin stdoutIgnored stderr ->
  Eff es BSL.ByteString
readProcessStdout_ pc =
  withProcessTerm pc' $ \p -> atomically $ do
    stdout <- P.getStdout p
    P.checkExitCodeSTM p `catchSTM` \ece ->
      throwSTM
        ece
          { eceStdout = stdout
          }
    return stdout
  where
    pc' = P.setStdout P.byteStringOutput pc
{-# INLINEABLE readProcessStdout_ #-}

-- | Same as 'readProcessStderr', but instead of returning the
-- 'ExitCode', checks it with 'checkExitCode'.
--
-- Exceptions thrown by this function will include stderr.
--
-- @since 0.1
readProcessStderr_ ::
  (TypedProcessDynamic :> es, STMDynamic :> es) =>
  -- | .
  ProcessConfig stdin stdout stderrIgnored ->
  Eff es BSL.ByteString
readProcessStderr_ pc =
  withProcessTerm pc' $ \p -> atomically $ do
    stderr <- P.getStderr p
    P.checkExitCodeSTM p `catchSTM` \ece ->
      throwSTM
        ece
          { eceStderr = stderr
          }
    return stderr
  where
    pc' = P.setStderr P.byteStringOutput pc
{-# INLINEABLE readProcessStderr_ #-}

-- | Same as 'withProcessWait', but also calls 'checkExitCode'
--
-- @since 0.1
withProcessWait_ ::
  (TypedProcessDynamic :> es, STMDynamic :> es) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessWait_ config f =
  bracket
    (startProcess config)
    stopProcess
    (\p -> f p <* checkExitCode p)
{-# INLINEABLE withProcessWait_ #-}

-- | Lifted Same as 'withProcessTerm', but also calls 'checkExitCode'.
--
-- To interact with a @Process@ use the functions from the section
-- [Interact with a process](#interactwithaprocess).
--
-- @since 0.1
withProcessTerm_ ::
  (TypedProcessDynamic :> es, STMDynamic :> es) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessTerm_ config =
  bracket
    (startProcess config)
    (\p -> stopProcess p `finally` checkExitCode p)
{-# INLINEABLE withProcessTerm_ #-}

-- | Wait for the process to exit and then return its 'ExitCode'.
--
-- @since 0.1
waitExitCode ::
  (STMDynamic :> es) =>
  Process stdin stdout stderr ->
  Eff es ExitCode
waitExitCode = atomically . P.waitExitCodeSTM
{-# INLINEABLE waitExitCode #-}

-- | Check if a process has exited and, if so, return its 'ExitCode'.
--
-- @since 0.1
getExitCode ::
  (STMDynamic :> es) =>
  Process stdin stdout stderr ->
  Eff es (Maybe ExitCode)
getExitCode = atomically . P.getExitCodeSTM
{-# INLINEABLE getExitCode #-}

-- | Wait for a process to exit, and ensure that it exited successfully.
-- If not, throws an 'ExitCodeException'.
--
-- Exceptions thrown by this function will not include stdout or stderr
-- (This prevents unbounded memory usage from reading them into memory).
-- However, some callers such as 'readProcess_' catch the exception, add the
-- stdout and stderr, and rethrow.
--
-- @since 0.1
checkExitCode ::
  (STMDynamic :> es) =>
  Process stdin stdout stderr ->
  Eff es ()
checkExitCode = atomically . P.checkExitCodeSTM
{-# INLINEABLE checkExitCode #-}
