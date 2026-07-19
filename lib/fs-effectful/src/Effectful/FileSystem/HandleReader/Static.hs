{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for reading a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleReader.Static
  ( -- * Effect
    HandleReader,
    openBinaryFile,
    withBinaryFile,
    hIsEOF,
    hGetBuffering,
    hIsOpen,
    hIsClosed,
    hIsReadable,
    hIsWritable,
    hIsSeekable,
    hIsTerminalDevice,
    hGetEcho,
    hWaitForInput,
    hReady,
    hGetChar,
    hGetLine,
    hGetContents,
    hGet,
    hGetSome,
    hGetNonBlocking,
    hLockRaw,
    hTryLockRaw,
    hUnlockRaw,

    -- ** Handlers
    runHandleReader,

    -- * File handles
    Handle,
    HandleMode (HandleModeRead),
    CanRead,

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

    -- ** GetLine
    hGetLineUtf8,
    hGetLineUtf8Lenient,
    hGetLineUtf8ThrowM,

    -- ** GetContents
    hGetContentsUtf8,
    hGetContentsUtf8Lenient,
    hGetContentsUtf8ThrowM,

    -- ** Get
    hGetUtf8,
    hGetUtf8Lenient,
    hGetUtf8ThrowM,

    -- ** GetSome
    hGetSomeUtf8,
    hGetSomeUtf8Lenient,
    hGetSomeUtf8ThrowM,

    -- ** GetNonBlocking
    hGetNonBlockingUtf8,
    hGetNonBlockingUtf8Lenient,
    hGetNonBlockingUtf8ThrowM,

    -- * Re-exports
    ByteString,
    OsPath,
    Text,
    UnicodeException,
  )
where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
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
import Effectful.FileSystem.Handle.Internal
  ( CanRead,
    Handle (MkHandle),
    HandleMode (HandleModeRead),
    LockedHandle,
  )
import Effectful.FileSystem.Handle.Internal qualified as Internal
import FileSystem.IO (openBinaryFileIO, withBinaryFileIO)
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8
import GHC.IO.Handle.Lock qualified as Lock
import System.IO (BufferMode, IOMode (ReadMode))
import System.IO qualified as IO

-- | Static effect for reading a handle.
--
-- @since 0.1
data HandleReader :: Effect

type instance DispatchOf HandleReader = Static WithSideEffects

data instance StaticRep HandleReader = MkHandleReader

-- | Runs 'HandleReader' in 'IO'.
--
-- @since 0.1
runHandleReader ::
  (HasCallStack, IOE :> es) =>
  Eff (HandleReader : es) a ->
  Eff es a
runHandleReader = evalStaticRep MkHandleReader

-- | Lifted 'IO.openBinaryFile'.
--
-- @since 0.1
openBinaryFile ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es (Handle HandleModeRead)
openBinaryFile =
  unsafeEff_
    . fmap MkHandle
    . (\p -> openBinaryFileIO p ReadMode)

-- | Lifted 'IO.withBinaryFile'.
--
-- @since 0.1
withBinaryFile ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  (Handle HandleModeRead -> Eff es a) ->
  Eff es a
withBinaryFile p onHandle =
  unsafeEff $ \env -> seqUnliftIO env $ \unlift ->
    withBinaryFileIO p ReadMode (unlift . onHandle . MkHandle)

-- | Lifted 'IO.hIsEof'.
--
-- @since 0.1
hIsEOF ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Bool
hIsEOF = unsafeEff_ . IO.hIsEOF . Internal.unHandle

-- | Lifted 'IO.hGetBuffering'.
--
-- @since 0.1
hGetBuffering ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es BufferMode
hGetBuffering = unsafeEff_ . IO.hGetBuffering . Internal.unHandle

-- | Lifted 'IO.hIsOpen'.
--
-- @since 0.1
hIsOpen ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Bool
hIsOpen = unsafeEff_ . IO.hIsOpen . Internal.unHandle

-- | Lifted 'IO.hIsClosed'.
--
-- @since 0.1
hIsClosed ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Bool
hIsClosed = unsafeEff_ . IO.hIsClosed . Internal.unHandle

-- | Lifted 'IO.hIsReadable'.
--
-- @since 0.1
hIsReadable ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Bool
hIsReadable = unsafeEff_ . IO.hIsReadable . Internal.unHandle

-- | Lifted 'IO.hIsWritable'.
--
-- @since 0.1
hIsWritable ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Bool
hIsWritable = unsafeEff_ . IO.hIsWritable . Internal.unHandle

-- | Lifted 'IO.hIsSeekable'.
--
-- @since 0.1
hIsSeekable ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Bool
hIsSeekable = unsafeEff_ . IO.hIsSeekable . Internal.unHandle

-- | Lifted 'IO.hIsTerminalDevice'.
--
-- @since 0.1
hIsTerminalDevice ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Bool
hIsTerminalDevice = unsafeEff_ . IO.hIsTerminalDevice . Internal.unHandle

-- | Lifted 'IO.hGetEcho'.
--
-- @since 0.1
hGetEcho ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Bool
hGetEcho = unsafeEff_ . IO.hGetEcho . Internal.unHandle

-- | Lifted 'IO.hWaitForInput'.
--
-- @since 0.1
hWaitForInput ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es Bool
hWaitForInput h = unsafeEff_ . IO.hWaitForInput (Internal.unHandle h)

-- | Lifted 'IO.hReady'.
--
-- @since 0.1
hReady ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Bool
hReady = unsafeEff_ . IO.hReady . Internal.unHandle

-- | Lifted 'IO.hGetChar'.
--
-- @since 0.1
hGetChar ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Char
hGetChar = unsafeEff_ . IO.hGetChar . Internal.unHandle

-- | Lifted 'BS.hGetLine'.
--
-- @since 0.1
hGetLine ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es ByteString
hGetLine = unsafeEff_ . C8.hGetLine . Internal.unHandle

-- | Lifted 'BS.hGetContents'.
--
-- @since 0.1
hGetContents ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es ByteString
hGetContents = unsafeEff_ . C8.hGetContents . Internal.unHandle

-- | Lifted 'BS.hGet'.
--
-- @since 0.1
hGet ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es ByteString
hGet h = unsafeEff_ . C8.hGet (Internal.unHandle h)

-- | Lifted 'BS.hGetSome'.
--
-- @since 0.1
hGetSome ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es ByteString
hGetSome h = unsafeEff_ . C8.hGetSome (Internal.unHandle h)

-- | Lifted 'BS.hGetNonBlocking'.
--
-- @since 0.1
hGetNonBlocking ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es ByteString
hGetNonBlocking h = unsafeEff_ . C8.hGetNonBlocking (Internal.unHandle h)

-- | Attempts to shared lock a file, blocking or throwing an exception
-- upon failure.
--
-- @since 0.1
hLockRaw ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es ()
hLockRaw h = unsafeEff_ $ Lock.hLock (Internal.unHandle h) Lock.SharedLock

-- | Attempts to shared lock a file.
--
-- @since 0.1
hTryLockRaw ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Bool
hTryLockRaw h = unsafeEff_ $ Lock.hTryLock (Internal.unHandle h) Lock.SharedLock

-- | Unlocks a locked file.
--
-- @since 0.1
hUnlockRaw ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es ()
hUnlockRaw = unsafeEff_ . Lock.hUnlock . Internal.unHandle

-- $locking
--
-- These functions bring some type-safety to file locking. Consider:
--
-- @
-- main :: (HandleReader :> es) => Eff es ()
-- main = withBinaryFile path False $ \\h -> do
--   hLockRaw handle
--   bs <- readBytes @HandleModeRead h
--   hUnlockRaw handle
--   print bs
--
-- readBytes :: (CanRead p, HandleReader :> es) => Handle p -> Eff es ByteString
-- readBytes handle = hGet handle 1024
-- @
--
-- In this example, we could remove all locking logic from @main@ and
-- everything would still compile. On the other hand:
--
-- @
-- main :: (HandleReader :> es) => Eff es ()
-- main = withBinaryFile path False $ \\h -> withLockedFile h $ \\lh -> do
--   bs <- readBytes @HandleModeRead lh
--   print bs
--
-- readBytes :: (CanRead p, HandleReader :> es) => LockedHandle p -> Eff es ByteString
-- readBytes lockedHandle = liftLocked (\\h -\> hGet h 1024) lockedHandle
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
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  -- | Handle to lock.
  Handle p ->
  Eff es (LockedHandle p)
hLock = Internal.liftLock hLockRaw

-- | Like 'hTryLockRaw', but returns a 'LockedHandle' if it succeeds.
--
-- @since 0.1
hTryLock ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  -- | Handle to lock.
  Handle p ->
  Eff es (Maybe (LockedHandle p))
hTryLock = Internal.liftTryLock hTryLockRaw

-- | Like 'hUnlockRaw', but returns the original handle.
--
-- @since 0.1
hUnlock ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  -- | Handle to unlock.
  LockedHandle p ->
  Eff es (Handle p)
hUnlock = Internal.liftUnlock hUnlockRaw

-- | Runs a computation with a shared locked file.
--
-- @since 0.1
withLockedFile ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
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
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
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
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
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
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
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

-- | 'hGetLine' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetLineUtf8 ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es (Either UnicodeException Text)
hGetLineUtf8 = fmap FS.UTF8.decodeUtf8 . hGetLine

-- | 'hGetLine' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetLineUtf8Lenient ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Text
hGetLineUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . hGetLine

-- | 'hGetLine' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetLineUtf8ThrowM ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Text
hGetLineUtf8ThrowM = hGetLine >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGetContents' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetContentsUtf8 ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es (Either UnicodeException Text)
hGetContentsUtf8 = fmap FS.UTF8.decodeUtf8 . hGetContents

-- | 'hGetContents' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetContentsUtf8Lenient ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Text
hGetContentsUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . hGetContents

-- | 'hGetContents' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetContentsUtf8ThrowM ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Eff es Text
hGetContentsUtf8ThrowM = hGetContents >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGet' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetUtf8 ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetUtf8 h = fmap FS.UTF8.decodeUtf8 . hGet h

-- | 'hGet' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetUtf8Lenient ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es Text
hGetUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGet h

-- | 'hGet' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetUtf8ThrowM ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es Text
hGetUtf8ThrowM h = hGet h >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGetSome' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetSomeUtf8 ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetSomeUtf8 h = fmap FS.UTF8.decodeUtf8 . hGetSome h

-- | 'hGetSome' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetSomeUtf8Lenient ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es Text
hGetSomeUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGetSome h

-- | 'hGetSome' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetSomeUtf8ThrowM ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es Text
hGetSomeUtf8ThrowM h = hGetSome h >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGetNonBlocking' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetNonBlockingUtf8 ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetNonBlockingUtf8 h = fmap FS.UTF8.decodeUtf8 . hGetNonBlocking h

-- | 'hGetNonBlocking' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetNonBlockingUtf8Lenient ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGetNonBlocking h

-- | 'hGetNonBlocking' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetNonBlockingUtf8ThrowM ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8ThrowM h = hGetNonBlocking h >=> FS.UTF8.decodeUtf8ThrowM
