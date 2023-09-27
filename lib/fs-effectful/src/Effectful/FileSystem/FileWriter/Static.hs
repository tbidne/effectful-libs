{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for writing files.
--
-- @since 0.1
module Effectful.FileSystem.FileWriter.Static
  ( -- * Class
    MonadFileWriter (..),

    -- * Effect
    FileWriterStatic,

    -- ** Handlers
    runFileWriterStaticIO,

    -- * UTF-8 Utils
    writeFileUtf8,
    appendFileUtf8,

    -- * Re-exports
    ByteString,
    OsPath,
    Text,
  )
where

import Data.ByteString (ByteString)
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
    unsafeEff_,
  )
import Effectful.FileSystem.Utils
  ( OsPath,
    appendBinaryFileIO,
    writeBinaryFileIO,
  )
import Effectful.FileSystem.Utils qualified as Utils

-- | Represents file-system writer effects.
--
-- @since 0.1
class (Monad m) => MonadFileWriter m where
  -- | Writes to a file.
  --
  -- @since 0.1
  writeBinaryFile :: OsPath -> ByteString -> m ()

  -- | Appends to a file.
  --
  -- @since 0.1
  appendBinaryFile :: OsPath -> ByteString -> m ()

-- | @since 0.1
instance MonadFileWriter IO where
  writeBinaryFile = writeBinaryFileIO
  appendBinaryFile = appendBinaryFileIO

-- | Static effect for reading files.
--
-- @since 0.1
data FileWriterStatic :: Effect

type instance DispatchOf FileWriterStatic = Static WithSideEffects

data instance StaticRep FileWriterStatic = MkFileWriterStatic

-- | Runs 'FileWriterStatic' in 'IO'.
--
-- @since 0.1
runFileWriterStaticIO ::
  (IOE :> es) =>
  Eff (FileWriterStatic : es) a ->
  Eff es a
runFileWriterStaticIO = evalStaticRep MkFileWriterStatic

-- | @since 0.1
instance (FileWriterStatic :> es) => MonadFileWriter (Eff es) where
  writeBinaryFile p = unsafeEff_ . writeBinaryFileIO p
  appendBinaryFile p = unsafeEff_ . appendBinaryFileIO p

-- | Writes the 'Text' to the file, encoding as UTF-8.
--
-- @since 0.1
writeFileUtf8 ::
  ( MonadFileWriter m
  ) =>
  OsPath ->
  Text ->
  m ()
writeFileUtf8 f = writeBinaryFile f . Utils.encodeUtf8

-- | Appends the 'Text' to the file, encoding as UTF-8.
--
-- @since 0.1
appendFileUtf8 ::
  ( MonadFileWriter m
  ) =>
  OsPath ->
  Text ->
  m ()
appendFileUtf8 f = appendBinaryFile f . Utils.encodeUtf8
