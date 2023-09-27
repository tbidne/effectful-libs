{-# LANGUAGE UndecidableInstances #-}

-- | Provides a dynamic effect for writing to a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleWriter.Dynamic
  ( -- * Class
    MonadHandleWriter (..),

    -- * Effect
    HandleWriterDynamic (..),

    -- ** Handlers
    runHandleWriterDynamicIO,

    -- * UTF-8 Utils
    hPutUtf8,
    hPutNonBlockingUtf8,

    -- * Misc
    die,

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
import Data.ByteString.Char8 qualified as Char8
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
import Effectful.Exception (exitFailure)
import Effectful.FileSystem.Utils (OsPath, openBinaryFileIO, withBinaryFileIO)
import Effectful.FileSystem.Utils qualified as Utils
import System.IO
  ( BufferMode (BlockBuffering, LineBuffering, NoBuffering),
    Handle,
    IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode),
    SeekMode (AbsoluteSeek, RelativeSeek, SeekFromEnd),
  )
import System.IO qualified as IO

-- | Represents handle writer effects.
--
-- @since 0.1
class (Monad m) => MonadHandleWriter m where
  -- | Lifted 'openBinaryFile'.
  --
  -- @since 0.1
  openBinaryFile :: OsPath -> IOMode -> m Handle

  -- | Lifted 'withBinaryFile'.
  --
  -- @since 0.1
  withBinaryFile :: OsPath -> IOMode -> (Handle -> m a) -> m a

  -- | Lifted 'IO.hClose'.
  --
  -- @since 0.1
  hClose :: Handle -> m ()

  -- | Lifted 'IO.hFlush'.
  --
  -- @since 0.1
  hFlush :: Handle -> m ()

  -- | Lifted 'IO.hSetFileSize'.
  --
  -- @since 0.1
  hSetFileSize :: Handle -> Integer -> m ()

  -- | Lifted 'IO.hSetBuffering'.
  --
  -- @since 0.1
  hSetBuffering :: Handle -> BufferMode -> m ()

  -- | Lifted 'IO.hSeek'.
  --
  -- @since 0.1
  hSeek :: Handle -> SeekMode -> Integer -> m ()

  -- | Lifted 'IO.hTell'.
  --
  -- @since 0.1
  hTell :: Handle -> m Integer

  -- | Lifted 'IO.hSetEcho'.
  --
  -- @since 0.1
  hSetEcho :: Handle -> Bool -> m ()

  -- | Lifted 'BS.hPut'.
  --
  -- @since 0.1
  hPut :: Handle -> ByteString -> m ()

  -- | Lifted 'BS.hPutNonBlocking'.
  --
  -- @since 0.1
  hPutNonBlocking :: Handle -> ByteString -> m ByteString

-- | @since 0.1
instance MonadHandleWriter IO where
  openBinaryFile = openBinaryFileIO
  withBinaryFile = withBinaryFileIO
  hClose = IO.hClose
  hFlush = IO.hFlush
  hSetFileSize = IO.hSetFileSize
  hSetBuffering = IO.hSetBuffering
  hSeek = IO.hSeek
  hTell = IO.hTell
  hSetEcho = IO.hSetEcho
  hPut = BS.hPut
  hPutNonBlocking = BS.hPutNonBlocking

-- | @since 0.1
type instance DispatchOf HandleWriterDynamic = Dynamic

-- | Dynamic effect for writing to a handle.
--
-- @since 0.1
data HandleWriterDynamic :: Effect where
  OpenBinaryFile :: OsPath -> IOMode -> HandleWriterDynamic m Handle
  WithBinaryFile :: OsPath -> IOMode -> (Handle -> m a) -> HandleWriterDynamic m a
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
  OpenBinaryFile p m -> liftIO $ openBinaryFileIO p m
  WithBinaryFile p m f -> localSeqUnliftIO env $ \runInIO ->
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
instance (HandleWriterDynamic :> es) => MonadHandleWriter (Eff es) where
  openBinaryFile p = send . OpenBinaryFile p
  withBinaryFile p m = send . WithBinaryFile p m
  hClose = send . HClose
  hFlush = send . HFlush
  hSetFileSize h = send . HSetFileSize h
  hSetBuffering h = send . HSetBuffering h
  hSeek h m = send . HSeek h m
  hTell = send . HTell
  hSetEcho h = send . HSetEcho h
  hPut h = send . HPut h
  hPutNonBlocking h = send . HPutNonBlocking h

-- | 'hPut' and 'Utils.encodeUtf8'.
--
-- @since 0.1
hPutUtf8 ::
  ( MonadHandleWriter m
  ) =>
  Handle ->
  Text ->
  m ()
hPutUtf8 h = hPut h . Utils.encodeUtf8

-- | 'hPutNonBlocking' and 'Utils.encodeUtf8'.
--
-- @since 0.1
hPutNonBlockingUtf8 ::
  ( MonadHandleWriter m
  ) =>
  Handle ->
  Text ->
  m ByteString
hPutNonBlockingUtf8 h = hPutNonBlocking h . Utils.encodeUtf8

-- | Write given error message to `stderr` and terminate with `exitFailure`.
--
-- @since 0.1
die :: (HandleWriterDynamic :> es) => String -> Eff es a
die err = hPut IO.stderr err' *> exitFailure
  where
    err' = Char8.pack err
