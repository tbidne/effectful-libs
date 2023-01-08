{-# LANGUAGE TemplateHaskell #-}

-- | Provides an effect for reading a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleReader
  ( -- * Effect
    HandleReader (..),

    -- * Handler
    runHandleReaderIO,

    -- * Functions
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

    -- * UTF-8 Utils
    hGetLineUtf8,
    hGetLineUtf8Lenient,
    hGetLineUtf8ThrowM,
    hGetContentsUtf8,
    hGetContentsUtf8Lenient,
    hGetContentsUtf8ThrowM,
    hGetUtf8,
    hGetUtf8Lenient,
    hGetUtf8ThrowM,
    hGetSomeUtf8,
    hGetSomeUtf8Lenient,
    hGetSomeUtf8ThrowM,
    hGetNonBlockingUtf8,
    hGetNonBlockingUtf8Lenient,
    hGetNonBlockingUtf8ThrowM,

    -- * Reexports
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
import Effectful.CallStack
  ( ECallStack,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.FileSystem.FileReader
  ( decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,
  )
import Effectful.TH (makeEffect_)
import GHC.Stack (HasCallStack)
import System.IO (BufferMode, Handle)
import System.IO qualified as IO

-- | Effect for reading a handle.
--
-- @since 0.1
data HandleReader :: Effect where
  HIsEOF :: HasCallStack => Handle -> HandleReader m Bool
  HGetBuffering :: HasCallStack => Handle -> HandleReader m BufferMode
  HIsOpen :: HasCallStack => Handle -> HandleReader m Bool
  HIsClosed :: HasCallStack => Handle -> HandleReader m Bool
  HIsReadable :: HasCallStack => Handle -> HandleReader m Bool
  HIsWritable :: HasCallStack => Handle -> HandleReader m Bool
  HIsSeekable :: HasCallStack => Handle -> HandleReader m Bool
  HIsTerminalDevice :: HasCallStack => Handle -> HandleReader m Bool
  HGetEcho :: HasCallStack => Handle -> HandleReader m Bool
  HWaitForInput :: HasCallStack => Handle -> Int -> HandleReader m Bool
  HReady :: HasCallStack => Handle -> HandleReader m Bool
  HGetChar :: HasCallStack => Handle -> HandleReader m Char
  HGetLine :: HasCallStack => Handle -> HandleReader m ByteString
  HGetContents :: HasCallStack => Handle -> HandleReader m ByteString
  HGet :: HasCallStack => Handle -> Int -> HandleReader m ByteString
  HGetSome :: HasCallStack => Handle -> Int -> HandleReader m ByteString
  HGetNonBlocking :: HasCallStack => Handle -> Int -> HandleReader m ByteString

-- | @since 0.1
type instance DispatchOf HandleReader = Dynamic

-- | Runs 'HandleReader' in 'IO'.
--
-- @since 0.1
runHandleReaderIO ::
  ( ECallStack :> es,
    IOE :> es
  ) =>
  Eff (HandleReader : es) a ->
  Eff es a
runHandleReaderIO = interpret $ \_ -> \case
  HIsEOF h -> addCallStack $ liftIO $ IO.hIsEOF h
  HGetBuffering h -> addCallStack $ liftIO $ IO.hGetBuffering h
  HIsOpen h -> addCallStack $ liftIO $ IO.hIsOpen h
  HIsClosed h -> addCallStack $ liftIO $ IO.hIsClosed h
  HIsReadable h -> addCallStack $ liftIO $ IO.hIsReadable h
  HIsWritable h -> addCallStack $ liftIO $ IO.hIsWritable h
  HIsSeekable h -> addCallStack $ liftIO $ IO.hIsSeekable h
  HIsTerminalDevice h -> addCallStack $ liftIO $ IO.hIsTerminalDevice h
  HGetEcho h -> addCallStack $ liftIO $ IO.hGetEcho h
  HWaitForInput h i -> addCallStack $ liftIO $ IO.hWaitForInput h i
  HReady h -> addCallStack $ liftIO $ IO.hReady h
  HGetChar h -> addCallStack $ liftIO $ IO.hGetChar h
  HGetLine h -> addCallStack $ liftIO $ BS.hGetLine h
  HGetContents h -> addCallStack $ liftIO $ BS.hGetContents h
  HGet h i -> addCallStack $ liftIO $ BS.hGet h i
  HGetSome h i -> addCallStack $ liftIO $ BS.hGetSome h i
  HGetNonBlocking h i -> addCallStack $ liftIO $ BS.hGetNonBlocking h i

makeEffect_ ''HandleReader

-- | @since 0.1
hIsEOF :: (HasCallStack, HandleReader :> es) => Handle -> Eff es Bool

-- | @since 0.1
hGetBuffering :: (HasCallStack, HandleReader :> es) => Handle -> Eff es BufferMode

-- | @since 0.1
hIsOpen :: (HasCallStack, HandleReader :> es) => Handle -> Eff es Bool

-- | @since 0.1
hIsClosed :: (HasCallStack, HandleReader :> es) => Handle -> Eff es Bool

-- | @since 0.1
hIsReadable :: (HasCallStack, HandleReader :> es) => Handle -> Eff es Bool

-- | @since 0.1
hIsWritable :: (HasCallStack, HandleReader :> es) => Handle -> Eff es Bool

-- | @since 0.1
hIsSeekable :: (HasCallStack, HandleReader :> es) => Handle -> Eff es Bool

-- | @since 0.1
hIsTerminalDevice :: (HasCallStack, HandleReader :> es) => Handle -> Eff es Bool

-- | @since 0.1
hGetEcho :: (HasCallStack, HandleReader :> es) => Handle -> Eff es Bool

-- | @since 0.1
hWaitForInput :: (HasCallStack, HandleReader :> es) => Handle -> Int -> Eff es Bool

-- | @since 0.1
hReady :: (HasCallStack, HandleReader :> es) => Handle -> Eff es Bool

-- | @since 0.1
hGetChar :: (HasCallStack, HandleReader :> es) => Handle -> Eff es Char

-- | @since 0.1
hGetLine :: (HasCallStack, HandleReader :> es) => Handle -> Eff es ByteString

-- | @since 0.1
hGetContents :: (HasCallStack, HandleReader :> es) => Handle -> Eff es ByteString

-- | @since 0.1
hGet :: (HasCallStack, HandleReader :> es) => Handle -> Int -> Eff es ByteString

-- | @since 0.1
hGetSome :: (HasCallStack, HandleReader :> es) => Handle -> Int -> Eff es ByteString

-- | @since 0.1
hGetNonBlocking :: (HasCallStack, HandleReader :> es) => Handle -> Int -> Eff es ByteString

-- | @since 0.1
hGetLineUtf8 ::
  ( HasCallStack,
    HandleReader :> es
  ) =>
  Handle ->
  Eff es (Either UnicodeException Text)
hGetLineUtf8 = fmap decodeUtf8 . hGetLine

-- | @since 0.1
hGetLineUtf8Lenient ::
  ( HasCallStack,
    HandleReader :> es
  ) =>
  Handle ->
  Eff es Text
hGetLineUtf8Lenient = fmap decodeUtf8Lenient . hGetLine

-- | @since 0.1
hGetLineUtf8ThrowM ::
  ( HasCallStack,
    ECallStack :> es,
    HandleReader :> es
  ) =>
  Handle ->
  Eff es Text
hGetLineUtf8ThrowM = hGetLine >=> decodeUtf8ThrowM

-- | @since 0.1
hGetContentsUtf8 ::
  ( HasCallStack,
    HandleReader :> es
  ) =>
  Handle ->
  Eff es (Either UnicodeException Text)
hGetContentsUtf8 = fmap decodeUtf8 . hGetContents

-- | @since 0.1
hGetContentsUtf8Lenient ::
  ( HasCallStack,
    HandleReader :> es
  ) =>
  Handle ->
  Eff es Text
hGetContentsUtf8Lenient = fmap decodeUtf8Lenient . hGetContents

-- | @since 0.1
hGetContentsUtf8ThrowM ::
  ( HasCallStack,
    ECallStack :> es,
    HandleReader :> es
  ) =>
  Handle ->
  Eff es Text
hGetContentsUtf8ThrowM = hGetContents >=> decodeUtf8ThrowM

-- | @since 0.1
hGetUtf8 ::
  ( HasCallStack,
    HandleReader :> es
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetUtf8 h = fmap decodeUtf8 . hGet h

-- | @since 0.1
hGetUtf8Lenient ::
  ( HasCallStack,
    HandleReader :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetUtf8Lenient h = fmap decodeUtf8Lenient . hGet h

-- | @since 0.1
hGetUtf8ThrowM ::
  ( HasCallStack,
    ECallStack :> es,
    HandleReader :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetUtf8ThrowM h = hGet h >=> decodeUtf8ThrowM

-- | @since 0.1
hGetSomeUtf8 ::
  ( HasCallStack,
    HandleReader :> es
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetSomeUtf8 h = fmap decodeUtf8 . hGetSome h

-- | @since 0.1
hGetSomeUtf8Lenient ::
  ( HasCallStack,
    HandleReader :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetSomeUtf8Lenient h = fmap decodeUtf8Lenient . hGetSome h

-- | @since 0.1
hGetSomeUtf8ThrowM ::
  ( HasCallStack,
    ECallStack :> es,
    HandleReader :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetSomeUtf8ThrowM h = hGetSome h >=> decodeUtf8ThrowM

-- | @since 0.1
hGetNonBlockingUtf8 ::
  ( HasCallStack,
    HandleReader :> es
  ) =>
  Handle ->
  Int ->
  Eff es (Either UnicodeException Text)
hGetNonBlockingUtf8 h = fmap decodeUtf8 . hGetNonBlocking h

-- | @since 0.1
hGetNonBlockingUtf8Lenient ::
  ( HasCallStack,
    HandleReader :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8Lenient h = fmap decodeUtf8Lenient . hGetNonBlocking h

-- | @since 0.1
hGetNonBlockingUtf8ThrowM ::
  ( HasCallStack,
    ECallStack :> es,
    HandleReader :> es
  ) =>
  Handle ->
  Int ->
  Eff es Text
hGetNonBlockingUtf8ThrowM h = hGetNonBlocking h >=> decodeUtf8ThrowM
