-- | Provides an effect for writing to a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleWriter
  ( -- * Effect
    HandleWriterEffect (..),
    Path,
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
    hPutNonBlockingUtf8,

    -- * Re-exports
    BufferMode (..),
    ByteString,
    IOMode (..),
    Handle,
    SeekMode (..),
    Text,
  )
where

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
import Effectful.FileSystem.FileWriter (encodeUtf8)
import Effectful.FileSystem.Internal (Path, openBinaryFileIO, withBinaryFileIO)
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

-- | Lifted 'IO.openBinaryFile'.
--
-- @since 0.1
hOpenBinaryFile ::
  ( HandleWriterEffect :> es
  ) =>
  Path ->
  IOMode ->
  Eff es Handle
hOpenBinaryFile p = send . HOpenBinaryFile p

-- | Lifted 'IO.withBinaryFile'.
--
-- @since 0.1
hWithBinaryFile ::
  ( HandleWriterEffect :> es
  ) =>
  Path ->
  IOMode ->
  (Handle -> Eff es a) ->
  Eff es a
hWithBinaryFile p m = send . HWithBinaryFile p m

-- | Lifted 'IO.hClose'.
--
-- @since 0.1
hClose ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Eff es ()
hClose = send . HClose

-- | Lifted 'IO.hFlush'.
--
-- @since 0.1
hFlush ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Eff es ()
hFlush = send . HFlush

-- | Lifted 'IO.hSetFileSize'.
--
-- @since 0.1
hSetFileSize ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Integer ->
  Eff es ()
hSetFileSize h = send . HSetFileSize h

-- | Lifted 'IO.hSetBuffering'.
--
-- @since 0.1
hSetBuffering ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  BufferMode ->
  Eff es ()
hSetBuffering h = send . HSetBuffering h

-- | Lifted 'IO.hSeek'.
--
-- @since 0.1
hSeek ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  SeekMode ->
  Integer ->
  Eff es ()
hSeek h m = send . HSeek h m

-- | Lifted 'IO.hTell'.
--
-- @since 0.1
hTell ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Eff es Integer
hTell = send . HTell

-- | Lifted 'IO.hSetEcho'.
--
-- @since 0.1
hSetEcho ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Bool ->
  Eff es ()
hSetEcho h = send . HSetEcho h

-- | Lifted 'BS.hPut'.
--
-- @since 0.1
hPut ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  ByteString ->
  Eff es ()
hPut h = send . HPut h

-- | Lifted 'BS.hPutNonBlocking'.
--
-- @since 0.1
hPutNonBlocking ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  ByteString ->
  Eff es ByteString
hPutNonBlocking h = send . HPutNonBlocking h

-- | 'hPut' and 'encodeUtf8'.
--
-- @since 0.1
hPutUtf8 ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Text ->
  Eff es ()
hPutUtf8 h = hPut h . encodeUtf8

-- | 'hPutNonBlocking' and 'encodeUtf8'.
--
-- @since 0.1
hPutNonBlockingUtf8 ::
  ( HandleWriterEffect :> es
  ) =>
  Handle ->
  Text ->
  Eff es ByteString
hPutNonBlockingUtf8 h = hPutNonBlocking h . encodeUtf8
