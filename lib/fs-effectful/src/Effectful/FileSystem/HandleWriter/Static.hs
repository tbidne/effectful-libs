{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for writing to a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleWriter.Static
  ( -- * Effect
    HandleWriter,
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
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( HasCallStack,
    SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    seqUnliftIO,
    unsafeEff,
    unsafeEff_,
  )
import Effectful.Exception (bracket, bracket_)
import Effectful.FileSystem.Handle qualified as Handle
import Effectful.FileSystem.Handle.Internal
  ( CanWrite,
    Handle (MkHandle),
    HandleMode (HandleModeWrite),
    LockedHandle,
  )
import Effectful.FileSystem.Handle.Internal qualified as Internal
import FileSystem.IO (openBinaryFileIO, withBinaryFileIO)
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8
import GHC.IO.Handle.Lock qualified as Lock
import System.IO
  ( BufferMode (BlockBuffering, LineBuffering, NoBuffering),
    IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode),
    SeekMode (AbsoluteSeek, RelativeSeek, SeekFromEnd),
  )
import System.IO qualified as IO

-- | Static effect for writing to a handle.
--
-- @since 0.1
data HandleWriter :: Effect

type instance DispatchOf HandleWriter = Static WithSideEffects

data instance StaticRep HandleWriter = MkHandleWriter

-- | Runs 'HandleWriter' in 'IO'.
--
-- @since 0.1
runHandleWriter ::
  (HasCallStack, IOE :> es) =>
  Eff (HandleWriter : es) a ->
  Eff es a
runHandleWriter = evalStaticRep MkHandleWriter

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
openBinaryFile p =
  unsafeEff_
    . fmap MkHandle
    . openBinaryFileIO p
    . Internal.appendToMode

-- | Lifted 'IO.withBinaryFile'.
--
-- @since 0.1
withBinaryFile ::
  forall es a.
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  OsPath ->
  Bool ->
  (Handle HandleModeWrite -> Eff es a) ->
  Eff es a
withBinaryFile p append onHandle =
  unsafeEff $ \env -> seqUnliftIO env $ \runInIO ->
    withBinaryFileIO
      p
      (Internal.appendToMode append)
      (runInIO . onHandle . MkHandle)

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
hClose = unsafeEff_ . IO.hClose . Internal.unHandle

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
hFlush = unsafeEff_ . IO.hFlush . Internal.unHandle

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
hSetFileSize h = unsafeEff_ . IO.hSetFileSize (Internal.unHandle h)

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
hSetBuffering h = unsafeEff_ . IO.hSetBuffering (Internal.unHandle h)

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
hSeek h m = unsafeEff_ . IO.hSeek (Internal.unHandle h) m

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
hTell = unsafeEff_ . IO.hTell . Internal.unHandle

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
hSetEcho h = unsafeEff_ . IO.hSetEcho (Internal.unHandle h)

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
hPut h = unsafeEff_ . BS.hPut (Internal.unHandle h)

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
hPutNonBlocking h = unsafeEff_ . BS.hPutNonBlocking (Internal.unHandle h)

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
hLockRaw h = unsafeEff_ $ Lock.hLock (Internal.unHandle h) Lock.ExclusiveLock

-- | Attempts to exclusively lock a file.
--
-- @since 0.1
hTryLockRaw ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p -> Eff es Bool
hTryLockRaw h = unsafeEff_ $ Lock.hTryLock (Internal.unHandle h) Lock.ExclusiveLock

-- | Unlocks a locked file.
--
-- @since 0.1
hUnlockRaw ::
  ( CanWrite p,
    HandleWriter :> es,
    HasCallStack
  ) =>
  Handle p -> Eff es ()
hUnlockRaw = unsafeEff_ . Lock.hUnlock . Internal.unHandle

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
