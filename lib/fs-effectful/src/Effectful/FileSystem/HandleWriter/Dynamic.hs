-- | Provides a dynamic effect for writing to a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleWriter.Dynamic
  ( -- * Effect
    HandleWriterDynamic (..),
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
    runHandleWriterDynamicIO,

    -- * UTF-8 Utils
    hPutUtf8,
    hPutNonBlockingUtf8,

    -- * Re-exports
    BufferMode (..),
    ByteString,
    Handle,
    IOMode (..),
    OsPath,
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
import Effectful.FileSystem.FileWriter.Dynamic (encodeUtf8)
import Effectful.FileSystem.Internal (OsPath, openBinaryFileIO, withBinaryFileIO)
import System.IO (BufferMode (..), Handle, IOMode (..), SeekMode (..))
import System.IO qualified as IO

-- | @since 0.1
type instance DispatchOf HandleWriterDynamic = Dynamic

-- | Dynamic effect for writing to a handle.
--
-- @since 0.1
data HandleWriterDynamic :: Effect where
  HOpenBinaryFile :: OsPath -> IOMode -> HandleWriterDynamic m Handle
  HWithBinaryFile :: OsPath -> IOMode -> (Handle -> m a) -> HandleWriterDynamic m a
  HClose :: Handle -> HandleWriterDynamic m ()
  HFlush :: Handle -> HandleWriterDynamic m ()
  HSetFileSize :: Handle -> Integer -> HandleWriterDynamic m ()
  HSetBuffering :: Handle -> BufferMode -> HandleWriterDynamic m ()
  HSeek :: Handle -> SeekMode -> Integer -> HandleWriterDynamic m ()
  HTell :: Handle -> HandleWriterDynamic m Integer
  HSetEcho :: Handle -> Bool -> HandleWriterDynamic m ()
  HPut :: Handle -> ByteString -> HandleWriterDynamic m ()
  HPutNonBlocking :: Handle -> ByteString -> HandleWriterDynamic m ByteString

-- | Runs 'HandleWriterDynamic' in 'IO'.
--
-- @since 0.1
runHandleWriterDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (HandleWriterDynamic : es) a ->
  Eff es a
runHandleWriterDynamicIO = interpret $ \env -> \case
  HOpenBinaryFile p m -> liftIO $ openBinaryFileIO p m
  HWithBinaryFile p m f -> localSeqUnliftIO env $ \runInDynamicIO ->
    liftIO $ withBinaryFileIO p m (runInDynamicIO . f)
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
  ( HandleWriterDynamic :> es
  ) =>
  OsPath ->
  IOMode ->
  Eff es Handle
hOpenBinaryFile p = send . HOpenBinaryFile p

-- | Lifted 'IO.withBinaryFile'.
--
-- @since 0.1
hWithBinaryFile ::
  ( HandleWriterDynamic :> es
  ) =>
  OsPath ->
  IOMode ->
  (Handle -> Eff es a) ->
  Eff es a
hWithBinaryFile p m = send . HWithBinaryFile p m

-- | Lifted 'IO.hClose'.
--
-- @since 0.1
hClose ::
  ( HandleWriterDynamic :> es
  ) =>
  Handle ->
  Eff es ()
hClose = send . HClose

-- | Lifted 'IO.hFlush'.
--
-- @since 0.1
hFlush ::
  ( HandleWriterDynamic :> es
  ) =>
  Handle ->
  Eff es ()
hFlush = send . HFlush

-- | Lifted 'IO.hSetFileSize'.
--
-- @since 0.1
hSetFileSize ::
  ( HandleWriterDynamic :> es
  ) =>
  Handle ->
  Integer ->
  Eff es ()
hSetFileSize h = send . HSetFileSize h

-- | Lifted 'IO.hSetBuffering'.
--
-- @since 0.1
hSetBuffering ::
  ( HandleWriterDynamic :> es
  ) =>
  Handle ->
  BufferMode ->
  Eff es ()
hSetBuffering h = send . HSetBuffering h

-- | Lifted 'IO.hSeek'.
--
-- @since 0.1
hSeek ::
  ( HandleWriterDynamic :> es
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
  ( HandleWriterDynamic :> es
  ) =>
  Handle ->
  Eff es Integer
hTell = send . HTell

-- | Lifted 'IO.hSetEcho'.
--
-- @since 0.1
hSetEcho ::
  ( HandleWriterDynamic :> es
  ) =>
  Handle ->
  Bool ->
  Eff es ()
hSetEcho h = send . HSetEcho h

-- | Lifted 'BS.hPut'.
--
-- @since 0.1
hPut ::
  ( HandleWriterDynamic :> es
  ) =>
  Handle ->
  ByteString ->
  Eff es ()
hPut h = send . HPut h

-- | Lifted 'BS.hPutNonBlocking'.
--
-- @since 0.1
hPutNonBlocking ::
  ( HandleWriterDynamic :> es
  ) =>
  Handle ->
  ByteString ->
  Eff es ByteString
hPutNonBlocking h = send . HPutNonBlocking h

-- | 'hPut' and 'encodeUtf8'.
--
-- @since 0.1
hPutUtf8 ::
  ( HandleWriterDynamic :> es
  ) =>
  Handle ->
  Text ->
  Eff es ()
hPutUtf8 h = hPut h . encodeUtf8

-- | 'hPutNonBlocking' and 'encodeUtf8'.
--
-- @since 0.1
hPutNonBlockingUtf8 ::
  ( HandleWriterDynamic :> es
  ) =>
  Handle ->
  Text ->
  Eff es ByteString
hPutNonBlockingUtf8 h = hPutNonBlocking h . encodeUtf8
