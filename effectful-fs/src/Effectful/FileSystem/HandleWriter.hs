{-# LANGUAGE TemplateHaskell #-}

-- | Provides an effect for writing to a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleWriter
  ( -- * Effect
    HandleWriter (..),
    Path,

    -- * Handler
    runHandleWriterIO,

    -- * Functions
    hOpenBinaryFile,
    hWithBinaryFile,
    hClose,
    hFlush,
    hSetFileSize,
    hSetBuffering,
    hSeek,
    hTell,
    hSetEcho,
    hPut,
    hPutNonBlocking,

    -- * UTF-8 Utils
    hPutUtf8,
    hPutNonBlockingUtf8',
    hPutNonBlockingUtf8,
    hPutNonBlockingUtf8Lenient,
    hPutNonBlockingUtf8ThrowM,

    -- * Reexports
    BufferMode (..),
    ByteString,
    IOMode (..),
    Handle,
    SeekMode (..),
    Text,
  )
where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    UnliftStrategy (SeqUnlift),
    type (:>),
  )
import Effectful.CallStack
  ( ECallStack,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO)
import Effectful.FileSystem.FileReader
  ( UnicodeException,
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,
  )
import Effectful.FileSystem.FileWriter (encodeUtf8)
import Effectful.FileSystem.Path (Path, openBinaryFileIO, withBinaryFileIO)
import Effectful.TH (makeEffect_)
import GHC.Stack (HasCallStack)
import System.IO (BufferMode (..), Handle, IOMode (..), SeekMode (..))
import System.IO qualified as IO

-- | @since 0.1
type instance DispatchOf HandleWriter = Dynamic

-- | Effect for writing to a handle.
--
-- @since 0.1
data HandleWriter :: Effect where
  HOpenBinaryFile :: HasCallStack => Path -> IOMode -> HandleWriter m Handle
  HWithBinaryFile :: HasCallStack => Path -> IOMode -> (Handle -> m a) -> HandleWriter m a
  HClose :: HasCallStack => Handle -> HandleWriter m ()
  HFlush :: HasCallStack => Handle -> HandleWriter m ()
  HSetFileSize :: HasCallStack => Handle -> Integer -> HandleWriter m ()
  HSetBuffering :: HasCallStack => Handle -> BufferMode -> HandleWriter m ()
  HSeek :: HasCallStack => Handle -> SeekMode -> Integer -> HandleWriter m ()
  HTell :: HasCallStack => Handle -> HandleWriter m Integer
  HSetEcho :: HasCallStack => Handle -> Bool -> HandleWriter m ()
  HPut :: HasCallStack => Handle -> ByteString -> HandleWriter m ()
  HPutNonBlocking :: HasCallStack => Handle -> ByteString -> HandleWriter m ByteString

-- | Runs 'HandleWriter' in 'IO'.
--
-- @since 0.1
runHandleWriterIO ::
  ( ECallStack :> es,
    IOE :> es
  ) =>
  Eff (HandleWriter : es) a ->
  Eff es a
runHandleWriterIO = interpret $ \env -> \case
  HOpenBinaryFile p m -> addCallStack $ liftIO $ openBinaryFileIO p m
  HWithBinaryFile p m f -> addCallStack $ localUnliftIO env SeqUnlift $ \runInIO ->
    liftIO $ withBinaryFileIO p m (runInIO . f)
  HClose h -> addCallStack $ liftIO $ IO.hClose h
  HFlush h -> addCallStack $ liftIO $ IO.hFlush h
  HSetFileSize h i -> addCallStack $ liftIO $ IO.hSetFileSize h i
  HSetBuffering h m -> addCallStack $ liftIO $ IO.hSetBuffering h m
  HSeek h m i -> addCallStack $ liftIO $ IO.hSeek h m i
  HTell h -> addCallStack $ liftIO $ IO.hTell h
  HSetEcho h b -> addCallStack $ liftIO $ IO.hSetEcho h b
  HPut h bs -> addCallStack $ liftIO $ BS.hPut h bs
  HPutNonBlocking h bs -> addCallStack $ liftIO $ BS.hPutNonBlocking h bs

makeEffect_ ''HandleWriter

-- | @since 0.1
hOpenBinaryFile :: (HasCallStack, HandleWriter :> es) => Path -> IOMode -> Eff es Handle

-- | @since 0.1
hWithBinaryFile :: (HasCallStack, HandleWriter :> es) => Path -> IOMode -> (Handle -> Eff es a) -> Eff es a

-- | @since 0.1
hClose :: (HasCallStack, HandleWriter :> es) => Handle -> Eff es ()

-- | @since 0.1
hFlush :: (HasCallStack, HandleWriter :> es) => Handle -> Eff es ()

-- | @since 0.1
hSetFileSize :: (HasCallStack, HandleWriter :> es) => Handle -> Integer -> Eff es ()

-- | @since 0.1
hSetBuffering :: (HasCallStack, HandleWriter :> es) => Handle -> BufferMode -> Eff es ()

-- | @since 0.1
hSeek :: (HasCallStack, HandleWriter :> es) => Handle -> SeekMode -> Integer -> Eff es ()

-- | @since 0.1
hTell :: (HasCallStack, HandleWriter :> es) => Handle -> Eff es Integer

-- | @since 0.1
hSetEcho :: (HasCallStack, HandleWriter :> es) => Handle -> Bool -> Eff es ()

-- | @since 0.1
hPut :: (HasCallStack, HandleWriter :> es) => Handle -> ByteString -> Eff es ()

-- | @since 0.1
hPutNonBlocking :: (HasCallStack, HandleWriter :> es) => Handle -> ByteString -> Eff es ByteString

-- | @since 0.1
hPutUtf8 :: (HasCallStack, HandleWriter :> es) => Handle -> Text -> Eff es ()
hPutUtf8 h = hPut h . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8' ::
  ( HasCallStack,
    HandleWriter :> es
  ) =>
  Handle ->
  Text ->
  Eff es ByteString
hPutNonBlockingUtf8' h = hPutNonBlocking h . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8 ::
  ( HasCallStack,
    HandleWriter :> es
  ) =>
  Handle ->
  Text ->
  Eff es (Either UnicodeException Text)
hPutNonBlockingUtf8 h = fmap decodeUtf8 . hPutNonBlocking h . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8Lenient ::
  ( HasCallStack,
    HandleWriter :> es
  ) =>
  Handle ->
  Text ->
  Eff es Text
hPutNonBlockingUtf8Lenient h =
  fmap decodeUtf8Lenient
    . hPutNonBlocking h
    . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8ThrowM ::
  ( HasCallStack,
    ECallStack :> es,
    HandleWriter :> es
  ) =>
  Handle ->
  Text ->
  Eff es Text
hPutNonBlockingUtf8ThrowM h =
  (hPutNonBlocking h . encodeUtf8) >=> decodeUtf8ThrowM
