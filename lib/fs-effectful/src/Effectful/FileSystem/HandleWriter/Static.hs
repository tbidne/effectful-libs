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
import Effectful.FileSystem.Utils (OsPath, openBinaryFileIO, withBinaryFileIO)
import Effectful.FileSystem.Utils qualified as Utils
import System.IO (BufferMode (..), Handle, IOMode (..), SeekMode (..))
import System.IO qualified as IO

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

-- | Lifted 'IO.openBinaryFile'.
--
-- @since 0.1
openBinaryFile ::
  ( HandleWriterStatic :> es
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
  ( HandleWriterStatic :> es
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
  ( HandleWriterStatic :> es
  ) =>
  Handle ->
  Eff es ()
hClose = unsafeEff_ . IO.hClose

-- | Lifted 'IO.hFlush'.
--
-- @since 0.1
hFlush ::
  ( HandleWriterStatic :> es
  ) =>
  Handle ->
  Eff es ()
hFlush = unsafeEff_ . IO.hFlush

-- | Lifted 'IO.hSetFileSize'.
--
-- @since 0.1
hSetFileSize ::
  ( HandleWriterStatic :> es
  ) =>
  Handle ->
  Integer ->
  Eff es ()
hSetFileSize h = unsafeEff_ . IO.hSetFileSize h

-- | Lifted 'IO.hSetBuffering'.
--
-- @since 0.1
hSetBuffering ::
  ( HandleWriterStatic :> es
  ) =>
  Handle ->
  BufferMode ->
  Eff es ()
hSetBuffering h = unsafeEff_ . IO.hSetBuffering h

-- | Lifted 'IO.hSeek'.
--
-- @since 0.1
hSeek ::
  ( HandleWriterStatic :> es
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
  ( HandleWriterStatic :> es
  ) =>
  Handle ->
  Eff es Integer
hTell = unsafeEff_ . IO.hTell

-- | Lifted 'IO.hSetEcho'.
--
-- @since 0.1
hSetEcho ::
  ( HandleWriterStatic :> es
  ) =>
  Handle ->
  Bool ->
  Eff es ()
hSetEcho h = unsafeEff_ . IO.hSetEcho h

-- | Lifted 'BS.hPut'.
--
-- @since 0.1
hPut ::
  ( HandleWriterStatic :> es
  ) =>
  Handle ->
  ByteString ->
  Eff es ()
hPut h = unsafeEff_ . BS.hPut h

-- | Lifted 'BS.hPutNonBlocking'.
--
-- @since 0.1
hPutNonBlocking ::
  ( HandleWriterStatic :> es
  ) =>
  Handle ->
  ByteString ->
  Eff es ByteString
hPutNonBlocking h = unsafeEff_ . BS.hPutNonBlocking h

-- | 'hPut' and 'Utils.encodeUtf8'.
--
-- @since 0.1
hPutUtf8 ::
  ( HandleWriterStatic :> es
  ) =>
  Handle ->
  Text ->
  Eff es ()
hPutUtf8 h = hPut h . Utils.encodeUtf8

-- | 'hPutNonBlocking' and 'Utils.encodeUtf8'.
--
-- @since 0.1
hPutNonBlockingUtf8 ::
  ( HandleWriterStatic :> es
  ) =>
  Handle ->
  Text ->
  Eff es ByteString
hPutNonBlockingUtf8 h = hPutNonBlocking h . Utils.encodeUtf8
