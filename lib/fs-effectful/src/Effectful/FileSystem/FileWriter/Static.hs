{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for writing files.
--
-- @since 0.1
module Effectful.FileSystem.FileWriter.Static
  ( -- * Effect
    FileWriterStatic,
    writeBinaryFile,
    appendBinaryFile,

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
import FileSystem.IO (appendBinaryFileIO, writeBinaryFileIO)
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8

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

-- | Writes a 'ByteString' to a file.
--
-- @since 0.1
writeBinaryFile ::
  ( FileWriterStatic :> es
  ) =>
  OsPath ->
  ByteString ->
  Eff es ()
writeBinaryFile p = unsafeEff_ . writeBinaryFileIO p

-- | Appends a 'ByteString' to a file.
--
-- @since 0.1
appendBinaryFile ::
  ( FileWriterStatic :> es
  ) =>
  OsPath ->
  ByteString ->
  Eff es ()
appendBinaryFile p = unsafeEff_ . appendBinaryFileIO p

-- | Writes the 'Text' to the file, encoding as UTF-8.
--
-- @since 0.1
writeFileUtf8 ::
  ( FileWriterStatic :> es
  ) =>
  OsPath ->
  Text ->
  Eff es ()
writeFileUtf8 f = writeBinaryFile f . FS.UTF8.encodeUtf8

-- | Appends the 'Text' to the file, encoding as UTF-8.
--
-- @since 0.1
appendFileUtf8 ::
  ( FileWriterStatic :> es
  ) =>
  OsPath ->
  Text ->
  Eff es ()
appendFileUtf8 f = appendBinaryFile f . FS.UTF8.encodeUtf8
