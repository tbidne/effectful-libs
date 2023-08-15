-- | Provides an effect for reading a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleReader.Dynamic
  ( -- * Effect
    HandleReaderEffect (..),
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
    runHandleReaderIO,

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
import Effectful.FileSystem.FileReader.Dynamic
  ( decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,
  )
import System.IO (BufferMode, Handle)
import System.IO qualified as IO

-- | Effect for reading a handle.
--
-- @since 0.1
data HandleReaderEffect :: Effect where
  HIsEOF :: Handle -> HandleReaderEffect m Bool
  HGetBuffering :: Handle -> HandleReaderEffect m BufferMode
  HIsOpen :: Handle -> HandleReaderEffect m Bool
  HIsClosed :: Handle -> HandleReaderEffect m Bool
  HIsReadable :: Handle -> HandleReaderEffect m Bool
  HIsWritable :: Handle -> HandleReaderEffect m Bool
  HIsSeekable :: Handle -> HandleReaderEffect m Bool
  HIsTerminalDevice :: Handle -> HandleReaderEffect m Bool
  HGetEcho :: Handle -> HandleReaderEffect m Bool
  HWaitForInput :: Handle -> Int -> HandleReaderEffect m Bool
  HReady :: Handle -> HandleReaderEffect m Bool
  HGetChar :: Handle -> HandleReaderEffect m Char
  HGetLine :: Handle -> HandleReaderEffect m ByteString
  HGetContents :: Handle -> HandleReaderEffect m ByteString
  HGet :: Handle -> Int -> HandleReaderEffect m ByteString
  HGetSome :: Handle -> Int -> HandleReaderEffect m ByteString
  HGetNonBlocking :: Handle -> Int -> HandleReaderEffect m ByteString

-- | @since 0.1
type instance DispatchOf HandleReaderEffect = Dynamic

-- | Runs 'HandleReaderEffect' in 'IO'.
--
-- @since 0.1
runHandleReaderIO ::
  ( IOE :> es
  ) =>
  Eff (HandleReaderEffect : es) a ->
  Eff es a
runHandleReaderIO = interpret $ \_ -> \case
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

-- | Lifted 'IO.hIsEof'.
--
-- @since 0.1
hIsEOF :: (HandleReaderEffect :> es) => Handle -> Eff es Bool
hIsEOF = send . HIsEOF

-- | Lifted 'IO.hGetBuffering'.
--
-- @since 0.1
hGetBuffering ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es BufferMode
hGetBuffering = send . HGetBuffering

-- | Lifted 'IO.hIsOpen'.
--
-- @since 0.1
hIsOpen ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Bool
hIsOpen = send . HIsOpen

-- | Lifted 'IO.hIsClosed'.
--
-- @since 0.1
hIsClosed ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Bool
hIsClosed = send . HIsClosed

-- | Lifted 'IO.hIsReadable'.
--
-- @since 0.1
hIsReadable ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Bool
hIsReadable = send . HIsReadable

-- | Lifted 'IO.hIsWritable'.
--
-- @since 0.1
hIsWritable ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Bool
hIsWritable = send . HIsWritable

-- | Lifted 'IO.hIsSeekable'.
--
-- @since 0.1
hIsSeekable ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Bool
hIsSeekable = send . HIsSeekable

-- | Lifted 'IO.hIsTerminalDevice'.
--
-- @since 0.1
hIsTerminalDevice ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Bool
hIsTerminalDevice = send . HIsTerminalDevice

-- | Lifted 'IO.hGetEcho'.
--
-- @since 0.1
hGetEcho ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Bool
hGetEcho = send . HGetEcho

-- | Lifted 'IO.hWaitForInput'.
--
-- @since 0.1
hWaitForInput ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es Bool
hWaitForInput h = send . HWaitForInput h

-- | Lifted 'IO.hReady'.
--
-- @since 0.1
hReady ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Bool
hReady = send . HReady

-- | Lifted 'IO.hGetChar'.
--
-- @since 0.1
hGetChar ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Char
hGetChar = send . HGetChar

-- | Lifted 'BS.hGetLine'.
--
-- @since 0.1
hGetLine ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es ByteString
hGetLine = send . HGetLine

-- | Lifted 'BS.hGetContents'.
--
-- @since 0.1
hGetContents ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es ByteString
hGetContents = send . HGetContents

-- | Lifted 'BS.hGet'.
--
-- @since 0.1
hGet ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGet h = send . HGet h

-- | Lifted 'BS.hGetSome'.
--
-- @since 0.1
hGetSome ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGetSome h = send . HGetSome h

-- | Lifted 'BS.hGetNonBlocking'.
--
-- @since 0.1
hGetNonBlocking ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es ByteString
hGetNonBlocking h = send . HGetNonBlocking h

-- | 'hGetLine' and 'decodeUtf8'.
--
-- @since 0.1
hGetLineUtf8 ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es (Either UnicodeException Text)
hGetLineUtf8 = fmap decodeUtf8 . hGetLine

-- | 'hGetLine' and 'decodeUtf8Lenient'.
--
-- @since 0.1
hGetLineUtf8Lenient ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Text
hGetLineUtf8Lenient = fmap decodeUtf8Lenient . hGetLine

-- | 'hGetLine' and 'decodeUtf8ThrowM'.
--
-- @since 0.1
hGetLineUtf8ThrowM ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Text
hGetLineUtf8ThrowM = hGetLine >=> decodeUtf8ThrowM

-- | 'hGetContents' and 'decodeUtf8'.
--
-- @since 0.1
hGetContentsUtf8 ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es (Either UnicodeException Text)
hGetContentsUtf8 = fmap decodeUtf8 . hGetContents

-- | 'hGetContents' and 'decodeUtf8Lenient'.
--
-- @since 0.1
hGetContentsUtf8Lenient ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Text
hGetContentsUtf8Lenient = fmap decodeUtf8Lenient . hGetContents

-- | 'hGetContents' and 'decodeUtf8ThrowM'.
--
-- @since 0.1
hGetContentsUtf8ThrowM ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Eff es Text
hGetContentsUtf8ThrowM = hGetContents >=> decodeUtf8ThrowM

-- | 'hGet' and 'decodeUtf8'.
--
-- @since 0.1
hGetUtf8 ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetUtf8 h = fmap decodeUtf8 . hGet h

-- | 'hGet' and 'decodeUtf8Lenient'.
--
-- @since 0.1
hGetUtf8Lenient ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetUtf8Lenient h = fmap decodeUtf8Lenient . hGet h

-- | 'hGet' and 'decodeUtf8ThrowM'.
--
-- @since 0.1
hGetUtf8ThrowM ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetUtf8ThrowM h = hGet h >=> decodeUtf8ThrowM

-- | 'hGetSome' and 'decodeUtf8'.
--
-- @since 0.1
hGetSomeUtf8 ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetSomeUtf8 h = fmap decodeUtf8 . hGetSome h

-- | 'hGetSome' and 'decodeUtf8Lenient'.
--
-- @since 0.1
hGetSomeUtf8Lenient ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetSomeUtf8Lenient h = fmap decodeUtf8Lenient . hGetSome h

-- | 'hGetSome' and 'decodeUtf8ThrowM'.
--
-- @since 0.1
hGetSomeUtf8ThrowM ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetSomeUtf8ThrowM h = hGetSome h >=> decodeUtf8ThrowM

-- | 'hGetNonBlocking' and 'decodeUtf8'.
--
-- @since 0.1
hGetNonBlockingUtf8 ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetNonBlockingUtf8 h = fmap decodeUtf8 . hGetNonBlocking h

-- | 'hGetNonBlocking' and 'decodeUtf8Lenient'.
--
-- @since 0.1
hGetNonBlockingUtf8Lenient ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8Lenient h = fmap decodeUtf8Lenient . hGetNonBlocking h

-- | 'hGetNonBlocking' and 'decodeUtf8ThrowM'.
--
-- @since 0.1
hGetNonBlockingUtf8ThrowM ::
  ( HandleReaderEffect :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8ThrowM h = hGetNonBlocking h >=> decodeUtf8ThrowM
