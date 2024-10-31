-- | Provides a dynamic effect for writing to a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleWriter.Dynamic
  ( -- * Effect
    HandleWriter (..),
    openBinaryFile,
    withBinaryFile,
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
    runHandleWriter,

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

import Control.Exception.Utils (exitFailure)
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
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, localSeqUnliftIO, send)
import FileSystem.IO (openBinaryFileIO, withBinaryFileIO)
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8
import System.IO
  ( BufferMode (BlockBuffering, LineBuffering, NoBuffering),
    Handle,
    IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode),
    SeekMode (AbsoluteSeek, RelativeSeek, SeekFromEnd),
  )
import System.IO qualified as IO

-- | @since 0.1
type instance DispatchOf HandleWriter = Dynamic

-- | Dynamic effect for writing to a handle.
--
-- @since 0.1
data HandleWriter :: Effect where
  OpenBinaryFile :: OsPath -> IOMode -> HandleWriter m Handle
  WithBinaryFile :: OsPath -> IOMode -> (Handle -> m a) -> HandleWriter m a
  HClose :: Handle -> HandleWriter m ()
  HFlush :: Handle -> HandleWriter m ()
  HSetFileSize :: Handle -> Integer -> HandleWriter m ()
  HSetBuffering :: Handle -> BufferMode -> HandleWriter m ()
  HSeek :: Handle -> SeekMode -> Integer -> HandleWriter m ()
  HTell :: Handle -> HandleWriter m Integer
  HSetEcho :: Handle -> Bool -> HandleWriter m ()
  HPut :: Handle -> ByteString -> HandleWriter m ()
  HPutNonBlocking :: Handle -> ByteString -> HandleWriter m ByteString

-- | Runs 'HandleWriter' in 'IO'.
--
-- @since 0.1
runHandleWriter ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (HandleWriter : es) a ->
  Eff es a
runHandleWriter = interpret $ \env -> \case
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

-- | Lifted 'IO.openBinaryFile'.
--
-- @since 0.1
openBinaryFile ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  OsPath ->
  IOMode ->
  Eff es Handle
openBinaryFile p = send . OpenBinaryFile p

-- | Lifted 'IO.withBinaryFile'.
--
-- @since 0.1
withBinaryFile ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  OsPath ->
  IOMode ->
  (Handle -> Eff es a) ->
  Eff es a
withBinaryFile p m = send . WithBinaryFile p m

-- | Lifted 'IO.hClose'.
--
-- @since 0.1
hClose ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ()
hClose = send . HClose

-- | Lifted 'IO.hFlush'.
--
-- @since 0.1
hFlush ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ()
hFlush = send . HFlush

-- | Lifted 'IO.hSetFileSize'.
--
-- @since 0.1
hSetFileSize ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  Integer ->
  Eff es ()
hSetFileSize h = send . HSetFileSize h

-- | Lifted 'IO.hSetBuffering'.
--
-- @since 0.1
hSetBuffering ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  BufferMode ->
  Eff es ()
hSetBuffering h = send . HSetBuffering h

-- | Lifted 'IO.hSeek'.
--
-- @since 0.1
hSeek ::
  ( HandleWriter :> es,
    HasCallStack
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
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Integer
hTell = send . HTell

-- | Lifted 'IO.hSetEcho'.
--
-- @since 0.1
hSetEcho ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  Bool ->
  Eff es ()
hSetEcho h = send . HSetEcho h

-- | Lifted 'BS.hPut'.
--
-- @since 0.1
hPut ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  ByteString ->
  Eff es ()
hPut h = send . HPut h

-- | Lifted 'BS.hPutNonBlocking'.
--
-- @since 0.1
hPutNonBlocking ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  ByteString ->
  Eff es ByteString
hPutNonBlocking h = send . HPutNonBlocking h

-- | 'hPut' and 'FS.UTF8.encodeUtf8'.
--
-- @since 0.1
hPutUtf8 ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  Text ->
  Eff es ()
hPutUtf8 h = hPut h . FS.UTF8.encodeUtf8

-- | 'hPutNonBlocking' and 'FS.UTF8.encodeUtf8'.
--
-- @since 0.1
hPutNonBlockingUtf8 ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  Text ->
  Eff es ByteString
hPutNonBlockingUtf8 h = hPutNonBlocking h . FS.UTF8.encodeUtf8

-- | Write given error message to `stderr` and terminate with `exitFailure`.
--
-- @since 0.1
die ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  String ->
  Eff es a
die err = hPut IO.stderr err' *> exitFailure
  where
    err' = Char8.pack err
