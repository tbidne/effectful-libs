-- | Provides a dynamic effect for writing to a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleWriter.Dynamic
  ( -- * Effect
    HandleWriter (..),
    openBinaryFile,
    withBinaryFile,
    hClose,
    hFlush,
    hSetFileSize,
    hSetBuffering,
    hSeek,
    hTell,
    hSetEcho,
    hPut,
    hPutNonBlocking,
    hLockRaw,
    hTryLockRaw,
    hUnlockRaw,

    -- ** Handlers
    runHandleWriter,

    -- * File handles
    Handle,
    HandleMode (HandleModeWrite),
    CanWrite,

    -- * Locking
    -- $locking
    LockedHandle,
    Internal.liftLocked,
    withLockedFile,
    withTryLockedFile,
    hLock,
    hTryLock,
    hUnlock,

    -- ** Raw
    withLockedFileRaw,
    withTryLockedFileRaw,

    -- * UTF-8 Utils
    hPutUtf8,
    hPutNonBlockingUtf8,

    -- * Misc
    die,

    -- * Re-exports
    BufferMode (..),
    ByteString,
    IOMode (..),
    OsPath,
    SeekMode (..),
    Text,
  )
where

import Control.Exception.Utils (exitFailure)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( HasCallStack,
    localSeqUnlift,
    reinterpret,
    send,
  )
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.Exception (bracket, bracket_)
import Effectful.FileSystem.Handle qualified as Handle
import Effectful.FileSystem.Handle.Internal
  ( CanWrite,
    Handle,
    HandleMode (HandleModeWrite),
    LockedHandle,
  )
import Effectful.FileSystem.Handle.Internal qualified as Internal
import Effectful.FileSystem.HandleWriter.Static qualified as Static
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8
import System.IO
  ( BufferMode (BlockBuffering, LineBuffering, NoBuffering),
    IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode),
    SeekMode (AbsoluteSeek, RelativeSeek, SeekFromEnd),
  )

-- | @since 0.1
type instance DispatchOf HandleWriter = Dynamic

-- | Dynamic effect for writing to a handle.
--
-- @since 0.1
data HandleWriter :: Effect where
  OpenBinaryFile :: OsPath -> Bool -> HandleWriter m (Handle HandleModeWrite)
  WithBinaryFile :: OsPath -> Bool -> (Handle HandleModeWrite -> m a) -> HandleWriter m a
  HClose :: (CanWrite p) => Handle p -> HandleWriter m ()
  HFlush :: (CanWrite p) => Handle p -> HandleWriter m ()
  HSetFileSize :: (CanWrite p) => Handle p -> Integer -> HandleWriter m ()
  HSetBuffering :: (CanWrite p) => Handle p -> BufferMode -> HandleWriter m ()
  HSeek :: (CanWrite p) => Handle p -> SeekMode -> Integer -> HandleWriter m ()
  HTell :: (CanWrite p) => Handle p -> HandleWriter m Integer
  HSetEcho :: (CanWrite p) => Handle p -> Bool -> HandleWriter m ()
  HPut :: (CanWrite p) => Handle p -> ByteString -> HandleWriter m ()
  HPutNonBlocking :: (CanWrite p) => Handle p -> ByteString -> HandleWriter m ByteString
  HLockRaw :: (CanWrite p) => Handle p -> HandleWriter m ()
  HTryLockRaw :: (CanWrite p) => Handle p -> HandleWriter m Bool
  HUnlockRaw :: (CanWrite p) => Handle p -> HandleWriter m ()

-- | @since 0.1
instance ShowEffect HandleWriter where
  showEffectCons = \case
    OpenBinaryFile _ _ -> "OpenBinaryFile"
    WithBinaryFile {} -> "WithBinaryFile"
    HClose _ -> "HClose"
    HFlush _ -> "HFlush"
    HSetFileSize _ _ -> "HSetFileSize"
    HSetBuffering _ _ -> "HSetBuffering"
    HSeek {} -> "HSeek"
    HTell _ -> "HTell"
    HSetEcho _ _ -> "HSetEcho"
    HPut _ _ -> "HPut"
    HPutNonBlocking _ _ -> "HPutNonBlocking"
    HLockRaw _ -> "HLockRaw"
    HTryLockRaw _ -> "HTryLockRaw"
    HUnlockRaw _ -> "HUnlockRaw"

-- | Runs 'HandleWriter' in 'IO'.
--
-- @since 0.1
runHandleWriter ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (HandleWriter : es) a ->
  Eff es a
runHandleWriter = reinterpret Static.runHandleWriter $ \env -> \case
  OpenBinaryFile p m -> Static.openBinaryFile p m
  WithBinaryFile p m f -> localSeqUnlift env $ \runInStatic ->
    Static.withBinaryFile p m (runInStatic . f)
  HClose h -> Static.hClose h
  HFlush h -> Static.hFlush h
  HSetFileSize h i -> Static.hSetFileSize h i
  HSetBuffering h m -> Static.hSetBuffering h m
  HSeek h m i -> Static.hSeek h m i
  HTell h -> Static.hTell h
  HSetEcho h b -> Static.hSetEcho h b
  HPut h bs -> Static.hPut h bs
  HPutNonBlocking h bs -> Static.hPutNonBlocking h bs
  HLockRaw h -> Static.hLockRaw h
  HTryLockRaw h -> Static.hTryLockRaw h
  HUnlockRaw h -> Static.hUnlockRaw h

-- | Lifted 'IO.openBinaryFile'.
--
-- @since 0.1
openBinaryFile ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  OsPath ->
  Bool ->
  Eff es (Handle HandleModeWrite)
openBinaryFile p = send . OpenBinaryFile p

-- | Lifted 'IO.withBinaryFile'.
--
-- @since 0.1
withBinaryFile ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  OsPath ->
  Bool ->
  (Handle HandleModeWrite -> Eff es a) ->
  Eff es a
withBinaryFile p m = send . WithBinaryFile p m

-- | Lifted 'IO.hClose'.
--
-- @since 0.1
hClose ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es ()
hClose = send . HClose

-- | Lifted 'IO.hFlush'.
--
-- @since 0.1
hFlush ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es ()
hFlush = send . HFlush

-- | Lifted 'IO.hSetFileSize'.
--
-- @since 0.1
hSetFileSize ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p ->
  Integer ->
  Eff es ()
hSetFileSize h = send . HSetFileSize h

-- | Lifted 'IO.hSetBuffering'.
--
-- @since 0.1
hSetBuffering ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p ->
  BufferMode ->
  Eff es ()
hSetBuffering h = send . HSetBuffering h

-- | Lifted 'IO.hSeek'.
--
-- @since 0.1
hSeek ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p ->
  SeekMode ->
  Integer ->
  Eff es ()
hSeek h m = send . HSeek h m

-- | Lifted 'IO.hTell'.
--
-- @since 0.1
hTell ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Integer
hTell = send . HTell

-- | Lifted 'IO.hSetEcho'.
--
-- @since 0.1
hSetEcho ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p ->
  Bool ->
  Eff es ()
hSetEcho h = send . HSetEcho h

-- | Lifted 'BS.hPut'.
--
-- @since 0.1
hPut ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p ->
  ByteString ->
  Eff es ()
hPut h = send . HPut h

-- | Lifted 'BS.hPutNonBlocking'.
--
-- @since 0.1
hPutNonBlocking ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p ->
  ByteString ->
  Eff es ByteString
hPutNonBlocking h = send . HPutNonBlocking h

-- | Attempts to exclusively lock a file, blocking or throwing an exception
-- upon failure.
--
-- @since 0.1
hLockRaw ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p -> Eff es ()
hLockRaw = send . HLockRaw

-- | Attempts to exclusively lock a file.
--
-- @since 0.1
hTryLockRaw ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p -> Eff es Bool
hTryLockRaw = send . HTryLockRaw

-- | Unlocks a locked file.
--
-- @since 0.1
hUnlockRaw ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p -> Eff es ()
hUnlockRaw = send . HUnlockRaw

-- $locking
--
-- These functions bring some type-safety to file locking. Consider:
--
-- @
-- main :: (HandleWriter :> es) => Eff es ()
-- main = withBinaryFile path False $ \\h -> do
--   hLockRaw handle
--   bs <- writeBytes @HandleModeWrite h
--   hUnlockRaw handle
--   print bs
--
-- writeBytes :: (CanWrite p, HandleWriter :> es) => Handle p -> Eff es ByteString
-- writeBytes handle = hPut handle "some bytes"
-- @
--
-- In this example, we could remove all locking logic from @main@ and
-- everything would still compile. On the other hand:
--
-- @
-- main :: (HandleWriter :> es) => Eff es ()
-- main = withBinaryFile path False $ \\h -> withLockedFile h $ \\lh -> do
--   bs <- writeBytes @HandleModeWrite lh
--   print bs
--
-- writeBytes :: (CanWrite p, HandleWriter :> es) => LockedHandle p -> Eff es ByteString
-- writeBytes lockedHandle = liftLocked (\\h -\> hPut h "some bytes") lockedHandle
-- @
--
-- Removing @withLockedFile@ would cause a compilation error, since @writeBytes@
-- requires a @LockedHandle@. The idea is to write most of the program's
-- logic in terms of @LockedHandle@, using @liftLocked@ to lift @Handle@
-- functions.

-- | Like 'hLockRaw', but returns a 'LockedHandle'.
--
-- @since 0.1
hLock ::
  ( CanWrite p,
    HasCallStack,
    HandleWriter :> es
  ) =>
  -- | Handle to lock.
  Handle p ->
  Eff es (LockedHandle p)
hLock = Internal.liftLock hLockRaw

-- | Like 'hTryLockRaw', but returns a 'LockedHandle' if it succeeds.
--
-- @since 0.1
hTryLock ::
  ( CanWrite p,
    HasCallStack,
    HandleWriter :> es
  ) =>
  -- | Handle to lock.
  Handle p ->
  Eff es (Maybe (LockedHandle p))
hTryLock = Internal.liftTryLock hTryLockRaw

-- | Like 'hUnlockRaw', but returns the original handle.
--
-- @since 0.1
hUnlock ::
  ( CanWrite p,
    HasCallStack,
    HandleWriter :> es
  ) =>
  -- | Handle to unlock.
  LockedHandle p ->
  Eff es (Handle p)
hUnlock = Internal.liftUnlock hUnlockRaw

-- | Runs a computation with an exclusively locked file.
--
-- @since 0.1
withLockedFile ::
  ( CanWrite p,
    HasCallStack,
    HandleWriter :> es
  ) =>
  -- | Handle to lock.
  Handle p ->
  -- | Callback with locked handle.
  (LockedHandle p -> Eff es a) ->
  Eff es a
withLockedFile = Internal.withLockedFile hLockRaw hUnlockRaw

-- | Like 'withSharedLockedFile', except the lock attempt does not block.
--
-- @since 0.1
withTryLockedFile ::
  forall p a es.
  ( CanWrite p,
    HasCallStack,
    HandleWriter :> es
  ) =>
  -- | Handle to lock.
  Handle p ->
  -- | Handle callback.
  (LockedHandle p -> Eff es a) ->
  Eff es (Maybe a)
withTryLockedFile = Internal.withTryLockedFile hTryLockRaw hUnlockRaw

-- | 'withLockedFile' without 'LockedHandle'.
--
-- @since 0.1
withLockedFileRaw ::
  ( CanWrite p,
    HasCallStack,
    HandleWriter :> es
  ) =>
  -- | Handle to lock.
  Handle p ->
  -- | Callback with locked handle.
  Eff es a ->
  Eff es a
withLockedFileRaw h = bracket_ (hLockRaw h) (hUnlockRaw h)

-- | 'withTryLockedFileRaw' without 'LockedHandle'.
--
-- @since 0.1
withTryLockedFileRaw ::
  forall p a es.
  ( CanWrite p,
    HasCallStack,
    HandleWriter :> es
  ) =>
  -- | Handle to lock.
  Handle p ->
  -- | Handle callback.
  Eff es a ->
  Eff es (Maybe a)
withTryLockedFileRaw h m =
  bracket
    ( do
        locked <- hTryLockRaw h
        pure $
          if locked
            then Just ()
            else Nothing
    )
    (traverse (const (hUnlockRaw h)))
    (traverse (const m))

-- | 'hPut' and 'FS.UTF8.encodeUtf8'.
--
-- @since 0.1
hPutUtf8 ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p ->
  Text ->
  Eff es ()
hPutUtf8 h = hPut h . FS.UTF8.encodeUtf8

-- | 'hPutNonBlocking' and 'FS.UTF8.encodeUtf8'.
--
-- @since 0.1
hPutNonBlockingUtf8 ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p ->
  Text ->
  Eff es ByteString
hPutNonBlockingUtf8 h = hPutNonBlocking h . FS.UTF8.encodeUtf8

-- | Write given error message to `stderr` and terminate with `exitFailure`.
--
-- @since 0.1
die ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  String ->
  Eff es a
die err = hPut Handle.stderr err' *> exitFailure
  where
    err' = Char8.pack err
