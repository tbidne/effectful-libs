{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for writing to a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleWriter.Static
  ( -- * Effect
    HandleWriterStatic,
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
    runHandleWriterStaticIO,

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

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    seqUnliftIO,
    unsafeEff,
    unsafeEff_,
  )
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

-- | Static effect for writing to a handle.
--
-- @since 0.1
data HandleWriterStatic :: Effect

type instance DispatchOf HandleWriterStatic = Static WithSideEffects

data instance StaticRep HandleWriterStatic = MkHandleWriterStatic

-- | Runs 'HandleWriterStatic' in 'IO'.
--
-- @since 0.1
runHandleWriterStaticIO ::
  (IOE :> es) =>
  Eff (HandleWriterStatic : es) a ->
  Eff es a
runHandleWriterStaticIO = evalStaticRep MkHandleWriterStatic

-- | @since 0.1
instance (HandleWriterStatic :> es) => MonadHandleWriter (Eff es) where
  openBinaryFile p = unsafeEff_ . openBinaryFileIO p
  withBinaryFile p m onHandle =
    unsafeEff $ \env -> seqUnliftIO env $
      \runInIO -> withBinaryFileIO p m (runInIO . onHandle)
  hClose = unsafeEff_ . IO.hClose
  hFlush = unsafeEff_ . IO.hFlush
  hSetFileSize h = unsafeEff_ . IO.hSetFileSize h
  hSetBuffering h = unsafeEff_ . IO.hSetBuffering h
  hSeek h m = unsafeEff_ . IO.hSeek h m
  hTell = unsafeEff_ . IO.hTell
  hSetEcho h = unsafeEff_ . IO.hSetEcho h
  hPut h = unsafeEff_ . BS.hPut h
  hPutNonBlocking h = unsafeEff_ . BS.hPutNonBlocking h

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
die :: (HandleWriterStatic :> es) => String -> Eff es a
die err = hPut IO.stderr err' *> exitFailure
  where
    err' = Char8.pack err
