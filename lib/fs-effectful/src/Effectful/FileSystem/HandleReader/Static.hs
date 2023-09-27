{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for reading a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleReader.Static
  ( -- * Class
    MonadHandleReader (..),

    -- * Effect
    HandleReaderStatic,

    -- ** Handlers
    runHandleReaderStaticIO,

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
import Data.ByteString qualified as BS
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
  ( SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    unsafeEff_,
  )
import Effectful.Exception (MonadThrow)
import Effectful.FileSystem.Utils (OsPath)
import Effectful.FileSystem.Utils qualified as Utils
import System.IO (BufferMode, Handle)
import System.IO qualified as IO

-- | Represents handle reader effects.
--
-- @since 0.1
class (Monad m) => MonadHandleReader m where
  -- | Lifted 'IO.hIsEOF'.
  --
  -- @since 0.1
  hIsEOF :: Handle -> m Bool

  -- | Lifted 'IO.hGetBuffering'.
  --
  -- @since 0.1
  hGetBuffering :: Handle -> m BufferMode

  -- | Lifted 'IO.hIsOpen'.
  --
  -- @since 0.1
  hIsOpen :: Handle -> m Bool

  -- | Lifted 'IO.hIsClosed'.
  --
  -- @since 0.1
  hIsClosed :: Handle -> m Bool

  -- | Lifted 'IO.hIsReadable'.
  --
  -- @since 0.1
  hIsReadable :: Handle -> m Bool

  -- | Lifted 'IO.hIsWritable'.
  --
  -- @since 0.1
  hIsWritable :: Handle -> m Bool

  -- | Lifted 'IO.hIsSeekable'.
  --
  -- @since 0.1
  hIsSeekable :: Handle -> m Bool

  -- | Lifted 'IO.hIsTerminalDevice'.
  --
  -- @since 0.1
  hIsTerminalDevice :: Handle -> m Bool

  -- | Lifted 'IO.hGetEcho'.
  --
  -- @since 0.1
  hGetEcho :: Handle -> m Bool

  -- | Lifted 'IO.hWaitForInput'.
  --
  -- @since 0.1
  hWaitForInput :: Handle -> Int -> m Bool

  -- | Lifted 'IO.hReady'.
  --
  -- @since 0.1
  hReady :: Handle -> m Bool

  -- | Lifted 'IO.hGetChar'.
  --
  -- @since 0.1
  hGetChar :: Handle -> m Char

  -- | Lifted 'BS.hGetLine'.
  --
  -- @since 0.1
  hGetLine :: Handle -> m ByteString

  -- | Lifted 'BS.hGetContents'.
  --
  -- @since 0.1
  hGetContents :: Handle -> m ByteString

  -- | Lifted 'BS.hGet'.
  --
  -- @since 0.1
  hGet :: Handle -> Int -> m ByteString

  -- | Lifted 'BS.hGetSome'.
  --
  -- @since 0.1
  hGetSome :: Handle -> Int -> m ByteString

  -- | Lifted 'BS.hGetNonBlocking'.
  --
  -- @since 0.1
  hGetNonBlocking :: Handle -> Int -> m ByteString

-- | @since 0.1
instance MonadHandleReader IO where
  hIsEOF = IO.hIsEOF
  hGetBuffering = IO.hGetBuffering
  hIsOpen = IO.hIsOpen
  hIsClosed = IO.hIsClosed
  hIsReadable = IO.hIsReadable
  hIsWritable = IO.hIsWritable
  hIsSeekable = IO.hIsSeekable
  hIsTerminalDevice = IO.hIsTerminalDevice
  hGetEcho = IO.hGetEcho
  hWaitForInput = IO.hWaitForInput
  hReady = IO.hReady
  hGetChar = IO.hGetChar
  hGetLine = BS.hGetLine
  hGetContents = BS.hGetContents
  hGet = BS.hGet
  hGetSome = BS.hGetSome
  hGetNonBlocking = BS.hGetNonBlocking

-- | Static effect for reading a handle.
--
-- @since 0.1
data HandleReaderStatic :: Effect

type instance DispatchOf HandleReaderStatic = Static WithSideEffects

data instance StaticRep HandleReaderStatic = MkHandleReaderStatic

-- | Runs 'HandleReaderStatic' in 'IO'.
--
-- @since 0.1
runHandleReaderStaticIO ::
  (IOE :> es) =>
  Eff (HandleReaderStatic : es) a ->
  Eff es a
runHandleReaderStaticIO = evalStaticRep MkHandleReaderStatic

-- | @since 0.1
instance (HandleReaderStatic :> es) => MonadHandleReader (Eff es) where
  hIsEOF = unsafeEff_ . IO.hIsEOF
  hGetBuffering = unsafeEff_ . IO.hGetBuffering
  hIsOpen = unsafeEff_ . IO.hIsOpen
  hIsClosed = unsafeEff_ . IO.hIsClosed
  hIsReadable = unsafeEff_ . IO.hIsReadable
  hIsWritable = unsafeEff_ . IO.hIsWritable
  hIsSeekable = unsafeEff_ . IO.hIsSeekable
  hIsTerminalDevice = unsafeEff_ . IO.hIsTerminalDevice
  hGetEcho = unsafeEff_ . IO.hGetEcho
  hWaitForInput h = unsafeEff_ . IO.hWaitForInput h
  hReady = unsafeEff_ . IO.hReady
  hGetChar = unsafeEff_ . IO.hGetChar
  hGetLine = unsafeEff_ . BS.hGetLine
  hGetContents = unsafeEff_ . BS.hGetContents
  hGet h = unsafeEff_ . BS.hGet h
  hGetSome h = unsafeEff_ . BS.hGetSome h
  hGetNonBlocking h = unsafeEff_ . BS.hGetNonBlocking h

-- | 'hGetLine' and 'Utils.decodeUtf8'.
--
-- @since 0.1
hGetLineUtf8 ::
  ( MonadHandleReader m
  ) =>
  Handle ->
  m (Either UnicodeException Text)
hGetLineUtf8 = fmap Utils.decodeUtf8 . hGetLine

-- | 'hGetLine' and 'Utils.decodeUtf8Lenient'.
--
-- @since 0.1
hGetLineUtf8Lenient ::
  ( MonadHandleReader m
  ) =>
  Handle ->
  m Text
hGetLineUtf8Lenient = fmap Utils.decodeUtf8Lenient . hGetLine

-- | 'hGetLine' and 'Utils.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetLineUtf8ThrowM ::
  ( MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle ->
  m Text
hGetLineUtf8ThrowM = hGetLine >=> Utils.decodeUtf8ThrowM

-- | 'hGetContents' and 'Utils.decodeUtf8'.
--
-- @since 0.1
hGetContentsUtf8 ::
  ( MonadHandleReader m
  ) =>
  Handle ->
  m (Either UnicodeException Text)
hGetContentsUtf8 = fmap Utils.decodeUtf8 . hGetContents

-- | 'hGetContents' and 'Utils.decodeUtf8Lenient'.
--
-- @since 0.1
hGetContentsUtf8Lenient ::
  ( MonadHandleReader m
  ) =>
  Handle ->
  m Text
hGetContentsUtf8Lenient = fmap Utils.decodeUtf8Lenient . hGetContents

-- | 'hGetContents' and 'decodeUtf8ThrowM'.
--
-- @since 0.1
hGetContentsUtf8ThrowM ::
  ( MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle ->
  m Text
hGetContentsUtf8ThrowM = hGetContents >=> Utils.decodeUtf8ThrowM

-- | 'hGet' and 'Utils.decodeUtf8'.
--
-- @since 0.1
hGetUtf8 ::
  ( MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m (Either UnicodeException Text)
hGetUtf8 h = fmap Utils.decodeUtf8 . hGet h

-- | 'hGet' and 'Utils.decodeUtf8Lenient'.
--
-- @since 0.1
hGetUtf8Lenient ::
  ( MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetUtf8Lenient h = fmap Utils.decodeUtf8Lenient . hGet h

-- | 'hGet' and 'decodeUtf8ThrowM'.
--
-- @since 0.1
hGetUtf8ThrowM ::
  ( MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle ->
  Int ->
  m Text
hGetUtf8ThrowM h = hGet h >=> Utils.decodeUtf8ThrowM

-- | 'hGetSome' and 'Utils.decodeUtf8'.
--
-- @since 0.1
hGetSomeUtf8 ::
  ( MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m (Either UnicodeException Text)
hGetSomeUtf8 h = fmap Utils.decodeUtf8 . hGetSome h

-- | 'hGetSome' and 'Utils.decodeUtf8Lenient'.
--
-- @since 0.1
hGetSomeUtf8Lenient ::
  ( MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetSomeUtf8Lenient h = fmap Utils.decodeUtf8Lenient . hGetSome h

-- | 'hGetSome' and 'Utils.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetSomeUtf8ThrowM ::
  ( MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle ->
  Int ->
  m Text
hGetSomeUtf8ThrowM h = hGetSome h >=> Utils.decodeUtf8ThrowM

-- | 'hGetNonBlocking' and 'Utils.decodeUtf8'.
--
-- @since 0.1
hGetNonBlockingUtf8 ::
  ( MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m (Either UnicodeException Text)
hGetNonBlockingUtf8 h = fmap Utils.decodeUtf8 . hGetNonBlocking h

-- | 'hGetNonBlocking' and 'Utils.decodeUtf8Lenient'.
--
-- @since 0.1
hGetNonBlockingUtf8Lenient ::
  ( MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetNonBlockingUtf8Lenient h = fmap Utils.decodeUtf8Lenient . hGetNonBlocking h

-- | 'hGetNonBlocking' and 'Utils.decodeUtf8ThrowM'.
--
-- @since 0.1
hGetNonBlockingUtf8ThrowM ::
  ( MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle ->
  Int ->
  m Text
hGetNonBlockingUtf8ThrowM h = hGetNonBlocking h >=> Utils.decodeUtf8ThrowM
