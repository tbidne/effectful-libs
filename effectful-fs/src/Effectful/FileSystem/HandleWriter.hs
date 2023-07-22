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
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.FileSystem.FileReader
  ( UnicodeException,
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,
  )
import Effectful.FileSystem.FileWriter (encodeUtf8)
import Effectful.FileSystem.Path (Path, openBinaryFileIO, withBinaryFileIO)
import System.IO (BufferMode (..), Handle, IOMode (..), SeekMode (..))
import System.IO qualified as IO

-- | @since 0.1
type instance DispatchOf HandleWriterEffect = Dynamic

-- | Effect for writing to a handle.
--
-- @since 0.1
data HandleWriterEffect :: Effect where
  HOpenBinaryFile :: Path -> IOMode -> HandleWriterEffect m Handle
  HWithBinaryFile :: Path -> IOMode -> (Handle -> m a) -> HandleWriterEffect m a
  HClose :: Handle -> HandleWriterEffect m ()
  HFlush :: Handle -> HandleWriterEffect m ()
  HSetFileSize :: Handle -> Integer -> HandleWriterEffect m ()
  HSetBuffering :: Handle -> BufferMode -> HandleWriterEffect m ()
  HSeek :: Handle -> SeekMode -> Integer -> HandleWriterEffect m ()
  HTell :: Handle -> HandleWriterEffect m Integer
  HSetEcho :: Handle -> Bool -> HandleWriterEffect m ()
  HPut :: Handle -> ByteString -> HandleWriterEffect m ()
  HPutNonBlocking :: Handle -> ByteString -> HandleWriterEffect m ByteString

-- | Runs 'HandleWriterEffect' in 'IO'.
--
-- @since 0.1
runHandleWriterIO ::
  ( IOE :> es
  ) =>
  Eff (HandleWriterEffect : es) a ->
  Eff es a
runHandleWriterIO = interpret $ \env -> \case
  HOpenBinaryFile p m -> liftIO $ openBinaryFileIO p m
  HWithBinaryFile p m f -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ withBinaryFileIO p m (runInIO . f)
  HClose h -> liftIO $ IO.hClose h
  HFlush h -> liftIO $ IO.hFlush h
  HSetFileSize h i -> liftIO $ IO.hSetFileSize h i
  HSetBuffering h m -> liftIO $ IO.hSetBuffering h m
  HSeek h m i -> liftIO $ IO.hSeek h m i
  HTell h -> liftIO $ IO.hTell h
  HSetEcho h b -> liftIO $ IO.hSetEcho h b
  HPut h bs -> liftIO $ BS.hPut h bs
  HPutNonBlocking h bs -> liftIO $ BS.hPutNonBlocking h bs

-- | @since 0.1
hOpenBinaryFile ::
  ( HandleWriterEffect :> es
  ) =>
  Path ->
  IOMode ->
  Eff es Handle
hOpenBinaryFile p = send . HOpenBinaryFile p

-- | @since 0.1
hWithBinaryFile ::
  ( HandleWriterEffect :> es
  ) =>
  Path ->
  IOMode ->
  (Handle -> Eff es a) ->
  Eff es a
hWithBinaryFile p m = send . HWithBinaryFile p m

-- | @since 0.1
hClose ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Eff es ()
hClose = send . HClose

-- | @since 0.1
hFlush ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Eff es ()
hFlush = send . HFlush

-- | @since 0.1
hSetFileSize ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Integer ->
  Eff es ()
hSetFileSize h = send . HSetFileSize h

-- | @since 0.1
hSetBuffering ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  BufferMode ->
  Eff es ()
hSetBuffering h = send . HSetBuffering h

-- | @since 0.1
hSeek ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  SeekMode ->
  Integer ->
  Eff es ()
hSeek h m = send . HSeek h m

-- | @since 0.1
hTell ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Eff es Integer
hTell = send . HTell

-- | @since 0.1
hSetEcho ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Bool ->
  Eff es ()
hSetEcho h = send . HSetEcho h

-- | @since 0.1
hPut ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  ByteString ->
  Eff es ()
hPut h = send . HPut h

-- | @since 0.1
hPutNonBlocking ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  ByteString ->
  Eff es ByteString
hPutNonBlocking h = send . HPutNonBlocking h

-- | @since 0.1
hPutUtf8 ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Text ->
  Eff es ()
hPutUtf8 h = hPut h . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8' ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Text ->
  Eff es ByteString
hPutNonBlockingUtf8' h = hPutNonBlocking h . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8 ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Text ->
  Eff es (Either UnicodeException Text)
hPutNonBlockingUtf8 h = fmap decodeUtf8 . hPutNonBlocking h . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8Lenient ::
  ( HandleWriterEffect :> es
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
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Text ->
  Eff es Text
hPutNonBlockingUtf8ThrowM h =
  (hPutNonBlocking h . encodeUtf8) >=> decodeUtf8ThrowM
