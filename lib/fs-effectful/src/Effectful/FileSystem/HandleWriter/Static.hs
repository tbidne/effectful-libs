{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for writing to a handle.
--
-- @since 0.1
module Effectful.FileSystem.HandleWriter.Static
  ( -- * Effect
    HandleWriter,
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
  ( HasCallStack,
    SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    seqUnliftIO,
    unsafeEff,
    unsafeEff_,
  )
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

-- | Static effect for writing to a handle.
--
-- @since 0.1
data HandleWriter :: Effect

type instance DispatchOf HandleWriter = Static WithSideEffects

data instance StaticRep HandleWriter = MkHandleWriter

-- | Runs 'HandleWriter' in 'IO'.
--
-- @since 0.1
runHandleWriter ::
  (HasCallStack, IOE :> es) =>
  Eff (HandleWriter : es) a ->
  Eff es a
runHandleWriter = evalStaticRep MkHandleWriter

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
openBinaryFile p = unsafeEff_ . openBinaryFileIO p

-- | Lifted 'IO.withBinaryFile'.
--
-- @since 0.1
withBinaryFile ::
  forall es a.
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  OsPath ->
  IOMode ->
  (Handle -> Eff es a) ->
  Eff es a
withBinaryFile p m onHandle =
  unsafeEff $ \env -> seqUnliftIO env $
    \runInIO -> withBinaryFileIO p m (runInIO . onHandle)

-- | Lifted 'IO.hClose'.
--
-- @since 0.1
hClose ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ()
hClose = unsafeEff_ . IO.hClose

-- | Lifted 'IO.hFlush'.
--
-- @since 0.1
hFlush ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es ()
hFlush = unsafeEff_ . IO.hFlush

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
hSetFileSize h = unsafeEff_ . IO.hSetFileSize h

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
hSetBuffering h = unsafeEff_ . IO.hSetBuffering h

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
hSeek h m = unsafeEff_ . IO.hSeek h m

-- | Lifted 'IO.hTell'.
--
-- @since 0.1
hTell ::
  ( HandleWriter :> es,
    HasCallStack
  ) =>
  Handle ->
  Eff es Integer
hTell = unsafeEff_ . IO.hTell

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
hSetEcho h = unsafeEff_ . IO.hSetEcho h

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
hPut h = unsafeEff_ . BS.hPut h

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
hPutNonBlocking h = unsafeEff_ . BS.hPutNonBlocking h

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
