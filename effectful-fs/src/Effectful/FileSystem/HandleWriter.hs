-- | Provides an effect for writing to a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleWriter
  ( -- * Effect
    HandleWriterEffect (..),
    Path,

    -- ** Functions
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

    -- ** Handlers
    runHandleWriterIO,

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
    type (:>),
  )
import Effectful.Exception
  ( CallStackEffect,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.FileSystem.FileReader
  ( UnicodeException,
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,
  )
import Effectful.FileSystem.FileWriter (encodeUtf8)
import Effectful.FileSystem.Path (Path, openBinaryFileIO, withBinaryFileIO)
import GHC.Stack (HasCallStack)
import System.IO (BufferMode (..), Handle, IOMode (..), SeekMode (..))
import System.IO qualified as IO

-- | @since 0.1
type instance DispatchOf HandleWriterEffect = Dynamic

-- | Effect for writing to a handle.
--
-- @since 0.1
data HandleWriterEffect :: Effect where
  HOpenBinaryFile :: (HasCallStack) => Path -> IOMode -> HandleWriterEffect m Handle
  HWithBinaryFile :: (HasCallStack) => Path -> IOMode -> (Handle -> m a) -> HandleWriterEffect m a
  HClose :: (HasCallStack) => Handle -> HandleWriterEffect m ()
  HFlush :: (HasCallStack) => Handle -> HandleWriterEffect m ()
  HSetFileSize :: (HasCallStack) => Handle -> Integer -> HandleWriterEffect m ()
  HSetBuffering :: (HasCallStack) => Handle -> BufferMode -> HandleWriterEffect m ()
  HSeek :: (HasCallStack) => Handle -> SeekMode -> Integer -> HandleWriterEffect m ()
  HTell :: (HasCallStack) => Handle -> HandleWriterEffect m Integer
  HSetEcho :: (HasCallStack) => Handle -> Bool -> HandleWriterEffect m ()
  HPut :: (HasCallStack) => Handle -> ByteString -> HandleWriterEffect m ()
  HPutNonBlocking :: (HasCallStack) => Handle -> ByteString -> HandleWriterEffect m ByteString

-- | Runs 'HandleWriterEffect' in 'IO'.
--
-- @since 0.1
runHandleWriterIO ::
  ( CallStackEffect :> es,
    IOE :> es
  ) =>
  Eff (HandleWriterEffect : es) a ->
  Eff es a
runHandleWriterIO = interpret $ \env -> \case
  HOpenBinaryFile p m -> addCallStack $ liftIO $ openBinaryFileIO p m
  HWithBinaryFile p m f -> addCallStack $ localSeqUnliftIO env $ \runInIO ->
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

-- | @since 0.1
hOpenBinaryFile ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Path ->
  IOMode ->
  Eff es Handle
hOpenBinaryFile p = send . HOpenBinaryFile p

-- | @since 0.1
hWithBinaryFile ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Path ->
  IOMode ->
  (Handle -> Eff es a) ->
  Eff es a
hWithBinaryFile p m = send . HWithBinaryFile p m

-- | @since 0.1
hClose ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ()
hClose = send . HClose

-- | @since 0.1
hFlush ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ()
hFlush = send . HFlush

-- | @since 0.1
hSetFileSize ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Integer ->
  Eff es ()
hSetFileSize h = send . HSetFileSize h

-- | @since 0.1
hSetBuffering ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  BufferMode ->
  Eff es ()
hSetBuffering h = send . HSetBuffering h

-- | @since 0.1
hSeek ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  SeekMode ->
  Integer ->
  Eff es ()
hSeek h m = send . HSeek h m

-- | @since 0.1
hTell ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Integer
hTell = send . HTell

-- | @since 0.1
hSetEcho ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Bool ->
  Eff es ()
hSetEcho h = send . HSetEcho h

-- | @since 0.1
hPut ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  ByteString ->
  Eff es ()
hPut h = send . HPut h

-- | @since 0.1
hPutNonBlocking ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  ByteString ->
  Eff es ByteString
hPutNonBlocking h = send . HPutNonBlocking h

-- | @since 0.1
hPutUtf8 ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Text ->
  Eff es ()
hPutUtf8 h = hPut h . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8' ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Text ->
  Eff es ByteString
hPutNonBlockingUtf8' h = hPutNonBlocking h . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8 ::
  ( HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Text ->
  Eff es (Either UnicodeException Text)
hPutNonBlockingUtf8 h = fmap decodeUtf8 . hPutNonBlocking h . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8Lenient ::
  ( HandleWriterEffect :> es,
    HasCallStack
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
  ( CallStackEffect :> es,
    HandleWriterEffect :> es,
    HasCallStack
  ) =>
  Handle ->
  Text ->
  Eff es Text
hPutNonBlockingUtf8ThrowM h =
  (hPutNonBlocking h . encodeUtf8) >=> decodeUtf8ThrowM
