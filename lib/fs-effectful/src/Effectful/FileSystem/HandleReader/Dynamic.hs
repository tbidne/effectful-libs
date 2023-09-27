{-# LANGUAGE UndecidableInstances #-}

-- | Provides a dynamic effect for reading a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleReader.Dynamic
  ( -- * Class
    MonadHandleReader (..),

    -- * Effect
    HandleReaderDynamic (..),

    -- ** Handlers
    runHandleReaderDynamicIO,

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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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
import Effectful.Dispatch.Dynamic (interpret, send)
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

-- | Dynamic effect for reading a handle.
--
-- @since 0.1
data HandleReaderDynamic :: Effect where
  HIsEOF :: Handle -> HandleReaderDynamic m Bool
  HGetBuffering :: Handle -> HandleReaderDynamic m BufferMode
  HIsOpen :: Handle -> HandleReaderDynamic m Bool
  HIsClosed :: Handle -> HandleReaderDynamic m Bool
  HIsReadable :: Handle -> HandleReaderDynamic m Bool
  HIsWritable :: Handle -> HandleReaderDynamic m Bool
  HIsSeekable :: Handle -> HandleReaderDynamic m Bool
  HIsTerminalDevice :: Handle -> HandleReaderDynamic m Bool
  HGetEcho :: Handle -> HandleReaderDynamic m Bool
  HWaitForInput :: Handle -> Int -> HandleReaderDynamic m Bool
  HReady :: Handle -> HandleReaderDynamic m Bool
  HGetChar :: Handle -> HandleReaderDynamic m Char
  HGetLine :: Handle -> HandleReaderDynamic m ByteString
  HGetContents :: Handle -> HandleReaderDynamic m ByteString
  HGet :: Handle -> Int -> HandleReaderDynamic m ByteString
  HGetSome :: Handle -> Int -> HandleReaderDynamic m ByteString
  HGetNonBlocking :: Handle -> Int -> HandleReaderDynamic m ByteString

-- | @since 0.1
type instance DispatchOf HandleReaderDynamic = Dynamic

-- | Runs 'HandleReaderDynamic' in 'IO'.
--
-- @since 0.1
runHandleReaderDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (HandleReaderDynamic : es) a ->
  Eff es a
runHandleReaderDynamicIO = interpret $ \_ -> \case
  HIsEOF h -> liftIO $ IO.hIsEOF h
  HGetBuffering h -> liftIO $ IO.hGetBuffering h
  HIsOpen h -> liftIO $ IO.hIsOpen h
  HIsClosed h -> liftIO $ IO.hIsClosed h
  HIsReadable h -> liftIO $ IO.hIsReadable h
  HIsWritable h -> liftIO $ IO.hIsWritable h
  HIsSeekable h -> liftIO $ IO.hIsSeekable h
  HIsTerminalDevice h -> liftIO $ IO.hIsTerminalDevice h
  HGetEcho h -> liftIO $ IO.hGetEcho h
  HWaitForInput h i -> liftIO $ IO.hWaitForInput h i
  HReady h -> liftIO $ IO.hReady h
  HGetChar h -> liftIO $ IO.hGetChar h
  HGetLine h -> liftIO $ BS.hGetLine h
  HGetContents h -> liftIO $ BS.hGetContents h
  HGet h i -> liftIO $ BS.hGet h i
  HGetSome h i -> liftIO $ BS.hGetSome h i
  HGetNonBlocking h i -> liftIO $ BS.hGetNonBlocking h i

-- | @since 0.1
instance (HandleReaderDynamic :> es) => MonadHandleReader (Eff es) where
  hIsEOF = send . HIsEOF
  hGetBuffering = send . HGetBuffering
  hIsOpen = send . HIsOpen
  hIsClosed = send . HIsClosed
  hIsReadable = send . HIsReadable
  hIsWritable = send . HIsWritable
  hIsSeekable = send . HIsSeekable
  hIsTerminalDevice = send . HIsTerminalDevice
  hGetEcho = send . HGetEcho
  hWaitForInput h = send . HWaitForInput h
  hReady = send . HReady
  hGetChar = send . HGetChar
  hGetLine = send . HGetLine
  hGetContents = send . HGetContents
  hGet h = send . HGet h
  hGetSome h = send . HGetSome h
  hGetNonBlocking h = send . HGetNonBlocking h

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
