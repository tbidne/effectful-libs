-- | Provides a dynamic effect for reading a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleReader.Dynamic
  ( -- * Effect
    HandleReader (..),
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

    -- ** Handlers
    runHandleReader,

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
    Handle,
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
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret_, send)
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.FileSystem.HandleReader.Static qualified as Static
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8
import System.IO (BufferMode, Handle)

-- | Dynamic effect for reading a handle.
--
-- @since 0.1
data HandleReader :: Effect where
  HIsEOF :: Handle -> HandleReader m Bool
  HGetBuffering :: Handle -> HandleReader m BufferMode
  HIsOpen :: Handle -> HandleReader m Bool
  HIsClosed :: Handle -> HandleReader m Bool
  HIsReadable :: Handle -> HandleReader m Bool
  HIsWritable :: Handle -> HandleReader m Bool
  HIsSeekable :: Handle -> HandleReader m Bool
  HIsTerminalDevice :: Handle -> HandleReader m Bool
  HGetEcho :: Handle -> HandleReader m Bool
  HWaitForInput :: Handle -> Int -> HandleReader m Bool
  HReady :: Handle -> HandleReader m Bool
  HGetChar :: Handle -> HandleReader m Char
  HGetLine :: Handle -> HandleReader m ByteString
  HGetContents :: Handle -> HandleReader m ByteString
  HGet :: Handle -> Int -> HandleReader m ByteString
  HGetSome :: Handle -> Int -> HandleReader m ByteString
  HGetNonBlocking :: Handle -> Int -> HandleReader m ByteString

-- | @since 0.1
type instance DispatchOf HandleReader = Dynamic

-- | @since 0.1
instance ShowEffect HandleReader where
  showEffectCons = \case
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

-- | Runs 'HandleReader' in 'IO'.
--
-- @since 0.1
runHandleReader ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (HandleReader : es) a ->
  Eff es a
runHandleReader = reinterpret_ Static.runHandleReader $ \case
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

-- | Lifted 'IO.hIsEof'.
--
-- @since 0.1
hIsEOF :: (HandleReader :> es, HasCallStack) => Handle -> Eff es Bool
hIsEOF = send . HIsEOF

-- | Lifted 'IO.hGetBuffering'.
--
-- @since 0.1
hGetBuffering ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es BufferMode
hGetBuffering = send . HGetBuffering

-- | Lifted 'IO.hIsOpen'.
--
-- @since 0.1
hIsOpen ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsOpen = send . HIsOpen

-- | Lifted 'IO.hIsClosed'.
--
-- @since 0.1
hIsClosed ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsClosed = send . HIsClosed

-- | Lifted 'IO.hIsReadable'.
--
-- @since 0.1
hIsReadable ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsReadable = send . HIsReadable

-- | Lifted 'IO.hIsWritable'.
--
-- @since 0.1
hIsWritable ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsWritable = send . HIsWritable

-- | Lifted 'IO.hIsSeekable'.
--
-- @since 0.1
hIsSeekable ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsSeekable = send . HIsSeekable

-- | Lifted 'IO.hIsTerminalDevice'.
--
-- @since 0.1
hIsTerminalDevice ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hIsTerminalDevice = send . HIsTerminalDevice

-- | Lifted 'IO.hGetEcho'.
--
-- @since 0.1
hGetEcho ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hGetEcho = send . HGetEcho

-- | Lifted 'IO.hWaitForInput'.
--
-- @since 0.1
hWaitForInput ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Bool
hWaitForInput h = send . HWaitForInput h

-- | Lifted 'IO.hReady'.
--
-- @since 0.1
hReady ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Bool
hReady = send . HReady

-- | Lifted 'IO.hGetChar'.
--
-- @since 0.1
hGetChar ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Char
hGetChar = send . HGetChar

-- | Lifted 'BS.hGetLine'.
--
-- @since 0.1
hGetLine ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ByteString
hGetLine = send . HGetLine

-- | Lifted 'BS.hGetContents'.
--
-- @since 0.1
hGetContents ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ByteString
hGetContents = send . HGetContents

-- | Lifted 'BS.hGet'.
--
-- @since 0.1
hGet ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGet h = send . HGet h

-- | Lifted 'BS.hGetSome'.
--
-- @since 0.1
hGetSome ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGetSome h = send . HGetSome h

-- | Lifted 'BS.hGetNonBlocking'.
--
-- @since 0.1
hGetNonBlocking ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGetNonBlocking h = send . HGetNonBlocking h

-- | 'hGetLine' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetLineUtf8 ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es (Either UnicodeException Text)
hGetLineUtf8 = fmap FS.UTF8.decodeUtf8 . hGetLine

-- | 'hGetLine' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetLineUtf8Lenient ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetLineUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . hGetLine

-- | 'hGetLine' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetLineUtf8ThrowM ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetLineUtf8ThrowM = hGetLine >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGetContents' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetContentsUtf8 ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es (Either UnicodeException Text)
hGetContentsUtf8 = fmap FS.UTF8.decodeUtf8 . hGetContents

-- | 'hGetContents' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetContentsUtf8Lenient ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetContentsUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . hGetContents

-- | 'hGetContents' and 'decodeUtf8ThrowM'.
--
-- @since 0.1
hGetContentsUtf8ThrowM ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Text
hGetContentsUtf8ThrowM = hGetContents >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGet' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetUtf8 ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetUtf8 h = fmap FS.UTF8.decodeUtf8 . hGet h

-- | 'hGet' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetUtf8Lenient ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGet h

-- | 'hGet' and 'decodeUtf8ThrowM'.
--
-- @since 0.1
hGetUtf8ThrowM ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetUtf8ThrowM h = hGet h >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGetSome' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetSomeUtf8 ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetSomeUtf8 h = fmap FS.UTF8.decodeUtf8 . hGetSome h

-- | 'hGetSome' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetSomeUtf8Lenient ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetSomeUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGetSome h

-- | 'hGetSome' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetSomeUtf8ThrowM ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetSomeUtf8ThrowM h = hGetSome h >=> FS.UTF8.decodeUtf8ThrowM

-- | 'hGetNonBlocking' and 'FS.UTF8.decodeUtf8'.
--
-- @since 0.1
hGetNonBlockingUtf8 ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetNonBlockingUtf8 h = fmap FS.UTF8.decodeUtf8 . hGetNonBlocking h

-- | 'hGetNonBlocking' and 'FS.UTF8.decodeUtf8Lenient'.
--
-- @since 0.1
hGetNonBlockingUtf8Lenient ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGetNonBlocking h

-- | 'hGetNonBlocking' and 'FS.UTF8.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetNonBlockingUtf8ThrowM ::
  ( HandleReader :> es,
    HasCallStack
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8ThrowM h = hGetNonBlocking h >=> FS.UTF8.decodeUtf8ThrowM
