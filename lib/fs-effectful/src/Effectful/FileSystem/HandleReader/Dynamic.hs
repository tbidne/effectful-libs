-- | Provides a dynamic effect for reading a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleReader.Dynamic
  ( -- * Effect
    HandleReader (..),
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
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (HasCallStack, localSeqUnlift, reinterpret, send)
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.Exception (bracket, bracket_)
import Effectful.FileSystem.Handle.Internal
  ( CanRead,
    Handle,
    HandleMode (HandleModeRead),
    LockedHandle,
  )
import Effectful.FileSystem.Handle.Internal qualified as Internal
import Effectful.FileSystem.HandleReader.Static qualified as Static
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8
import System.IO (BufferMode)

-- | Dynamic effect for reading a handle.
--
-- @since 0.1
data HandleReader :: Effect where
  OpenBinaryFile :: OsPath -> HandleReader m (Handle HandleModeRead)
  WithBinaryFile :: OsPath -> (Handle HandleModeRead -> m a) -> HandleReader m a
  HIsEOF :: (CanRead p) => Handle p -> HandleReader m Bool
  HGetBuffering :: (CanRead p) => Handle p -> HandleReader m BufferMode
  HIsOpen :: (CanRead p) => Handle p -> HandleReader m Bool
  HIsClosed :: (CanRead p) => Handle p -> HandleReader m Bool
  HIsReadable :: (CanRead p) => Handle p -> HandleReader m Bool
  HIsWritable :: (CanRead p) => Handle p -> HandleReader m Bool
  HIsSeekable :: (CanRead p) => Handle p -> HandleReader m Bool
  HIsTerminalDevice :: (CanRead p) => Handle p -> HandleReader m Bool
  HGetEcho :: (CanRead p) => Handle p -> HandleReader m Bool
  HWaitForInput :: (CanRead p) => Handle p -> Int -> HandleReader m Bool
  HReady :: (CanRead p) => Handle p -> HandleReader m Bool
  HGetChar :: (CanRead p) => Handle p -> HandleReader m Char
  HGetLine :: (CanRead p) => Handle p -> HandleReader m ByteString
  HGetContents :: (CanRead p) => Handle p -> HandleReader m ByteString
  HGet :: (CanRead p) => Handle p -> Int -> HandleReader m ByteString
  HGetSome :: (CanRead p) => Handle p -> Int -> HandleReader m ByteString
  HGetNonBlocking :: (CanRead p) => Handle p -> Int -> HandleReader m ByteString
  HLockRaw :: (CanRead p) => Handle p -> HandleReader m ()
  HTryLockRaw :: (CanRead p) => Handle p -> HandleReader m Bool
  HUnlockRaw :: (CanRead p) => Handle p -> HandleReader m ()

-- | @since 0.1
type instance DispatchOf HandleReader = Dynamic

-- | @since 0.1
instance ShowEffect HandleReader where
  showEffectCons = \case
    OpenBinaryFile _ -> "OpenBinaryFile"
    WithBinaryFile {} -> "WithBinaryFile"
    HIsEOF _ -> "HIsEOF"
    HGetBuffering _ -> "HGetBuffering"
    HIsOpen _ -> "HIsOpen"
    HIsClosed _ -> "HIsClosed"
    HIsReadable _ -> "HIsReadable"
    HIsWritable _ -> "HIsWritable"
    HIsSeekable _ -> "HIsSeekable"
    HIsTerminalDevice _ -> "HIsTerminalDevice"
    HGetEcho _ -> "HGetEcho"
    HWaitForInput _ _ -> "HWaitForInput"
    HReady _ -> "HReady"
    HGetChar _ -> "HGetChar"
    HGetLine _ -> "HGetLine"
    HGetContents _ -> "HGetContents"
    HGet _ _ -> "HGet"
    HGetSome _ _ -> "HGetSome"
    HGetNonBlocking _ _ -> "HGetNonBlocking"
    HLockRaw _ -> "HLockRaw"
    HTryLockRaw _ -> "HTryLockRaw"
    HUnlockRaw _ -> "HUnlockRaw"

-- | Runs 'HandleReader' in 'IO'.
--
-- @since 0.1
runHandleReader ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (HandleReader : es) a ->
  Eff es a
runHandleReader = reinterpret Static.runHandleReader $ \env -> \case
  OpenBinaryFile p -> Static.openBinaryFile p
  WithBinaryFile p k -> localSeqUnlift env $ \unlift ->
    Static.withBinaryFile p (unlift . k)
  HIsEOF h -> Static.hIsEOF h
  HGetBuffering h -> Static.hGetBuffering h
  HIsOpen h -> Static.hIsOpen h
  HIsClosed h -> Static.hIsClosed h
  HIsReadable h -> Static.hIsReadable h
  HIsWritable h -> Static.hIsWritable h
  HIsSeekable h -> Static.hIsSeekable h
  HIsTerminalDevice h -> Static.hIsTerminalDevice h
  HGetEcho h -> Static.hGetEcho h
  HWaitForInput h i -> Static.hWaitForInput h i
  HReady h -> Static.hReady h
  HGetChar h -> Static.hGetChar h
  HGetLine h -> Static.hGetLine h
  HGetContents h -> Static.hGetContents h
  HGet h i -> Static.hGet h i
  HGetSome h i -> Static.hGetSome h i
  HGetNonBlocking h i -> Static.hGetNonBlocking h i
  HLockRaw h -> Static.hLockRaw h
  HTryLockRaw h -> Static.hTryLockRaw h
  HUnlockRaw h -> Static.hUnlockRaw h

-- | Lifted 'IO.openBinaryFile'.
--
-- @since 0.1
openBinaryFile ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es (Handle HandleModeRead)
openBinaryFile = send . OpenBinaryFile

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
withBinaryFile p = send . WithBinaryFile p

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
hIsEOF = send . HIsEOF

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
hGetBuffering = send . HGetBuffering

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
hIsOpen = send . HIsOpen

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
hIsClosed = send . HIsClosed

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
hIsReadable = send . HIsReadable

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
hIsWritable = send . HIsWritable

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
hIsSeekable = send . HIsSeekable

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
hIsTerminalDevice = send . HIsTerminalDevice

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
hGetEcho = send . HGetEcho

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
hWaitForInput h = send . HWaitForInput h

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
hReady = send . HReady

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
hGetChar = send . HGetChar

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
hGetLine = send . HGetLine

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
hGetContents = send . HGetContents

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
hGet h = send . HGet h

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
hGetSome h = send . HGetSome h

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
hGetNonBlocking h = send . HGetNonBlocking h

-- | Attempts to shared lock a file, blocking or throwing an exception
-- upon failure.
--
-- @since 0.1
hLockRaw ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p -> Eff es ()
hLockRaw = send . HLockRaw

-- | Attempts to shared lock a file.
--
-- @since 0.1
hTryLockRaw ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p -> Eff es Bool
hTryLockRaw = send . HTryLockRaw

-- | Unlocks a locked file.
--
-- @since 0.1
hUnlockRaw ::
  ( CanRead p,
    HandleReader :> es,
    HasCallStack
  ) =>
  Handle p -> Eff es ()
hUnlockRaw = send . HUnlockRaw

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
    HasCallStack,
    HandleReader :> es
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
    HasCallStack,
    HandleReader :> es
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
    HasCallStack,
    HandleReader :> es
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
    HasCallStack,
    HandleReader :> es
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
    HasCallStack,
    HandleReader :> es
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
    HasCallStack,
    HandleReader :> es
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
    HasCallStack,
    HandleReader :> es
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

-- | 'hGetContents' and 'decodeUtf8ThrowM'.
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

-- | 'hGet' and 'decodeUtf8ThrowM'.
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
