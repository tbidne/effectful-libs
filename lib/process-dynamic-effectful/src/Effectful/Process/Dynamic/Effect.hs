{-# LANGUAGE CPP #-}

-- | Provides a dynamic effect for typed process.
--
-- @since 0.1
module Effectful.Process.Dynamic.Effect
  ( -- * Effect
    Process (..),
  )
where

import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Effect,
  )
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.Process (Pid, ProcessHandle)
import Effectful.Process qualified as EP
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Posix.Internals (FD)

-- | Dynamic effect for process.
--
-- @since 0.1
data Process :: Effect where
  -- | @since 0.1
  CreateProcess ::
    EP.CreateProcess ->
    Process m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  -- | @since 0.1
  CreateProcess_ ::
    String ->
    EP.CreateProcess ->
    Process m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  -- | @since 0.1
  CallProcess :: FilePath -> [String] -> Process m ()
  -- | @since 0.1
  CallCommand :: String -> Process m ()
  -- | @since 0.1
  SpawnProcess :: FilePath -> [String] -> Process m ProcessHandle
  -- | @since 0.1
  SpawnCommand :: String -> Process m ProcessHandle
  -- | @since 0.1
  ReadCreateProcess :: EP.CreateProcess -> String -> Process m String
  -- | @since 0.1
  ReadProcess ::
    FilePath ->
    [String] ->
    String ->
    Process m String
  -- | @since 0.1
  ReadCreateProcessWithExitCode ::
    EP.CreateProcess ->
    String ->
    Process m (ExitCode, String, String)
  -- | @since 0.1
  ReadProcessWithExitCode ::
    FilePath ->
    [String] ->
    String ->
    Process m (ExitCode, String, String)
  -- | @since 0.1
  WithCreateProcess ::
    EP.CreateProcess ->
    (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> m a) ->
    Process m a
  -- | @since 0.1
  CleanupProcess ::
    (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) ->
    Process m ()
  -- | @since 0.1
  GetPid :: ProcessHandle -> Process m (Maybe Pid)
  -- | @since 0.1
  GetCurrentPid :: Process m Pid
  -- | @since 0.1
  WaitForProcess :: ProcessHandle -> Process m ExitCode
  -- | @since 0.1
  GetProcessExitCode :: ProcessHandle -> Process m (Maybe ExitCode)
  -- | @since 0.1
  TerminateProcess :: ProcessHandle -> Process m ()
  -- | @since 0.1
  InterruptProcessGroupOf :: ProcessHandle -> Process m ()
  -- | @since 0.1
  CreatePipe :: Process m (Handle, Handle)
  -- | @since 0.1
  CreatePipeFd :: Process m (FD, FD)

type instance DispatchOf Process = Dynamic

-- | @since 0.1
instance ShowEffect Process where
  showEffectCons = \case
    CreateProcess {} -> "CreateProcess"
    CreateProcess_ {} -> "CreateProcess_"
    CallProcess {} -> "CallProcess"
    CallCommand {} -> "CallCommand"
    SpawnProcess {} -> "SpawnProcess"
    SpawnCommand {} -> "SpawnCommand"
    ReadCreateProcess {} -> "ReadCreateProcess"
    ReadProcess {} -> "ReadProcess"
    ReadCreateProcessWithExitCode {} -> "ReadCreateProcessWithExitCode"
    ReadProcessWithExitCode {} -> "ReadProcessWithExitCode"
    WithCreateProcess {} -> "WithCreateProcess"
    CleanupProcess {} -> "CleanupProcess"
    GetPid {} -> "GetPid"
    GetCurrentPid {} -> "GetCurrentPid"
    WaitForProcess {} -> "WaitForProcess"
    GetProcessExitCode {} -> "GetProcessExitCode"
    TerminateProcess {} -> "TerminateProcess"
    InterruptProcessGroupOf {} -> "InterruptProcessGroupOf"
    CreatePipe {} -> "CreatePipe"
    CreatePipeFd {} -> "CreatePipeFd"
