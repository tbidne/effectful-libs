{-# LANGUAGE CPP #-}

-- | Provides a dynamic effect for typed process.
--
-- @since 0.1
module Effectful.Process.Dynamic
  ( -- * Effect
    -- $effect
    Process,

    -- * Handler
    runProcess,

    -- * API

    -- ** Running sub-processes
    createProcess,
    createProcess_,
    EP.shell,
    EP.proc,
    EP.CreateProcess (..),
    EP.CmdSpec (..),
    EP.StdStream (..),
    EP.ProcessHandle,

    -- ** Simpler functions for common tasks
    callProcess,
    callCommand,
    spawnProcess,
    spawnCommand,
    readCreateProcess,
    readProcess,
    readCreateProcessWithExitCode,
    readProcessWithExitCode,
    withCreateProcess,
    cleanupProcess,

    -- ** Related utilities
    EP.showCommandForUser,
    Pid,
    getPid,
    getCurrentPid,
    waitForProcess,
    getProcessExitCode,
    terminateProcess,
    interruptProcessGroupOf,
    createPipe,
    createPipeFd,

    -- ** Re-exports
    FD,
    Handle,
  )
where

import Effectful
  ( Eff,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (localSeqUnlift, reinterpret, send)
import Effectful.Process (Pid, ProcessHandle)
import Effectful.Process qualified as EP
import Effectful.Process.Dynamic.Effect (Process)
import Effectful.Process.Dynamic.Effect qualified as Effect
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Posix.Internals (FD)

-- $effect
--
-- The full definition for 'Process' is exported from
-- "Effectful.Process.Dynamic.Effect", due to a name clash between some of
-- its constructors and process re-exports. Hence if you need the data
-- constructors, import that module qualified.

-- | Runs 'Process' in 'IO'.
--
-- @since 0.1
runProcess :: (HasCallStack, IOE :> es) => Eff (Process : es) a -> Eff es a
runProcess = reinterpret EP.runProcess $ \env -> \case
  Effect.CreateProcess x1 -> EP.createProcess x1
  Effect.CreateProcess_ x1 x2 -> EP.createProcess_ x1 x2
  Effect.CallProcess x1 x2 -> EP.callProcess x1 x2
  Effect.CallCommand x1 -> EP.callCommand x1
  Effect.SpawnProcess x1 x2 -> EP.spawnProcess x1 x2
  Effect.SpawnCommand x1 -> EP.spawnCommand x1
  Effect.ReadCreateProcess x1 x2 -> EP.readCreateProcess x1 x2
  Effect.ReadProcess x1 x2 x3 -> EP.readProcess x1 x2 x3
  Effect.ReadCreateProcessWithExitCode x1 x2 -> EP.readCreateProcessWithExitCode x1 x2
  Effect.ReadProcessWithExitCode x1 x2 x3 -> EP.readProcessWithExitCode x1 x2 x3
  Effect.WithCreateProcess x1 onProcess -> localSeqUnlift env $ \unlift ->
    EP.withCreateProcess x1 (\y1 y2 y3 y4 -> unlift (onProcess y1 y2 y3 y4))
  Effect.CleanupProcess x1 -> EP.cleanupProcess x1
  Effect.GetPid x1 -> EP.getPid x1
  Effect.GetCurrentPid -> EP.getCurrentPid
  Effect.WaitForProcess x1 -> EP.waitForProcess x1
  Effect.GetProcessExitCode x1 -> EP.getProcessExitCode x1
  Effect.TerminateProcess x1 -> EP.terminateProcess x1
  Effect.InterruptProcessGroupOf x1 -> EP.interruptProcessGroupOf x1
  Effect.CreatePipe -> EP.createPipe
  Effect.CreatePipeFd -> EP.createPipeFd

-- | Lifted 'EP.createProcess'.
--
-- @since 0.1
createProcess ::
  ( HasCallStack,
    Process :> es
  ) =>
  EP.CreateProcess ->
  Eff es (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess = send . Effect.CreateProcess

-- | Lifted 'EP.createProcess_'.
--
-- @since 0.1
createProcess_ ::
  ( HasCallStack,
    Process :> es
  ) =>
  String ->
  EP.CreateProcess ->
  Eff es (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ x1 = send . Effect.CreateProcess_ x1

-- | Lifted 'EP.callProcess'.
--
-- @since 0.1
callProcess ::
  ( HasCallStack,
    Process :> es
  ) =>
  FilePath ->
  [String] ->
  Eff es ()
callProcess x1 = send . Effect.CallProcess x1

-- | Lifted 'EP.callCommand'.
--
-- @since 0.1
callCommand ::
  ( HasCallStack,
    Process :> es
  ) =>
  String ->
  Eff es ()
callCommand = send . Effect.CallCommand

-- | Lifted 'EP.spawnProcess'.
--
-- @since 0.1
spawnProcess ::
  ( HasCallStack,
    Process :> es
  ) =>
  FilePath ->
  [String] ->
  Eff es ProcessHandle
spawnProcess x1 = send . Effect.SpawnProcess x1

-- | Lifted 'EP.spawnCommand'.
--
-- @since 0.1
spawnCommand ::
  ( HasCallStack,
    Process :> es
  ) =>
  String ->
  Eff es ProcessHandle
spawnCommand = send . Effect.SpawnCommand

-- | Lifted 'EP.readCreateProcess'.
--
-- @since 0.1
readCreateProcess ::
  ( HasCallStack,
    Process :> es
  ) =>
  EP.CreateProcess ->
  String ->
  Eff es String
readCreateProcess x1 = send . Effect.ReadCreateProcess x1

-- | Lifted 'EP.readProcess'.
--
-- @since 0.1
readProcess ::
  ( HasCallStack,
    Process :> es
  ) =>
  FilePath ->
  [String] ->
  String ->
  Eff es String
readProcess x1 x2 = send . Effect.ReadProcess x1 x2

-- | Lifted 'EP.readCreateProcessWithExitCode'.
--
-- @since 0.1
readCreateProcessWithExitCode ::
  ( HasCallStack,
    Process :> es
  ) =>
  EP.CreateProcess ->
  String ->
  Eff es (ExitCode, String, String)
readCreateProcessWithExitCode x1 = send . Effect.ReadCreateProcessWithExitCode x1

-- | Lifted 'EP.readProcessWithExitCode'.
--
-- @since 0.1
readProcessWithExitCode ::
  ( HasCallStack,
    Process :> es
  ) =>
  FilePath ->
  [String] ->
  String ->
  Eff es (ExitCode, String, String)
readProcessWithExitCode x1 x2 = send . Effect.ReadProcessWithExitCode x1 x2

-- | Lifted 'EP.withCreateProcess'.
--
-- @since 0.1
withCreateProcess ::
  ( HasCallStack,
    Process :> es
  ) =>
  EP.CreateProcess ->
  (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> Eff es a) ->
  Eff es a
withCreateProcess x1 = send . Effect.WithCreateProcess x1

-- | Lifted 'EP.cleanupProcess'.
--
-- @since 0.1
cleanupProcess ::
  ( HasCallStack,
    Process :> es
  ) =>
  (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) ->
  Eff es ()
cleanupProcess = send . Effect.CleanupProcess

-- | Lifted 'EP.getPid'.
--
-- @since 0.1
getPid ::
  ( HasCallStack,
    Process :> es
  ) =>
  ProcessHandle ->
  Eff es (Maybe Pid)
getPid = send . Effect.GetPid

-- | Lifted 'EP.getCurrentPid'.
--
-- @since 0.1
getCurrentPid ::
  ( HasCallStack,
    Process :> es
  ) =>
  Eff es Pid
getCurrentPid = send Effect.GetCurrentPid

-- | Lifted 'EP.waitForProcess'.
--
-- @since 0.1
waitForProcess ::
  ( HasCallStack,
    Process :> es
  ) =>
  ProcessHandle ->
  Eff es ExitCode
waitForProcess = send . Effect.WaitForProcess

-- | Lifted 'EP.getProcessExitCode'.
--
-- @since 0.1
getProcessExitCode ::
  ( HasCallStack,
    Process :> es
  ) =>
  ProcessHandle ->
  Eff es (Maybe ExitCode)
getProcessExitCode = send . Effect.GetProcessExitCode

-- | Lifted 'EP.terminateProcess'.
--
-- @since 0.1
terminateProcess ::
  ( HasCallStack,
    Process :> es
  ) =>
  ProcessHandle ->
  Eff es ()
terminateProcess = send . Effect.TerminateProcess

-- | Lifted 'EP.interruptProcessGroupOf'.
--
-- @since 0.1
interruptProcessGroupOf ::
  ( HasCallStack,
    Process :> es
  ) =>
  ProcessHandle ->
  Eff es ()
interruptProcessGroupOf = send . Effect.InterruptProcessGroupOf

-- | Lifted 'EP.createPipe'.
--
-- @since 0.1
createPipe ::
  ( HasCallStack,
    Process :> es
  ) =>
  Eff es (Handle, Handle)
createPipe = send Effect.CreatePipe

-- | Lifted 'EP.createPipeFd'.
--
-- @since 0.1
createPipeFd ::
  ( HasCallStack,
    Process :> es
  ) =>
  Eff es (FD, FD)
createPipeFd = send Effect.CreatePipeFd
