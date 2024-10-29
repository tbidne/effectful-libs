{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for writing files.
--
-- @since 0.1
module Effectful.FileSystem.FileWriter.Static
  ( -- * Effect
    FileWriter,
    writeBinaryFile,
    appendBinaryFile,

    -- ** Handlers
    runFileWriter,

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
data FileWriter :: Effect

type instance DispatchOf FileWriter = Static WithSideEffects

data instance StaticRep FileWriter = MkFileWriter

-- | Runs 'FileWriter' in 'IO'.
--
-- @since 0.1
runFileWriter ::
  (IOE :> es) =>
  Eff (FileWriter : es) a ->
  Eff es a
runFileWriter = evalStaticRep MkFileWriter

-- | Writes a 'ByteString' to a file.
--
-- @since 0.1
writeBinaryFile ::
  ( FileWriter :> es
  ) =>
  OsPath ->
  ByteString ->
  Eff es ()
writeBinaryFile p = unsafeEff_ . writeBinaryFileIO p

-- | Appends a 'ByteString' to a file.
--
-- @since 0.1
appendBinaryFile ::
  ( FileWriter :> es
  ) =>
  OsPath ->
  ByteString ->
  Eff es ()
appendBinaryFile p = unsafeEff_ . appendBinaryFileIO p

-- | Writes the 'Text' to the file, encoding as UTF-8.
--
-- @since 0.1
writeFileUtf8 ::
  ( FileWriter :> es
  ) =>
  OsPath ->
  Text ->
  Eff es ()
writeFileUtf8 f = writeBinaryFile f . FS.UTF8.encodeUtf8

-- | Appends the 'Text' to the file, encoding as UTF-8.
--
-- @since 0.1
appendFileUtf8 ::
  ( FileWriter :> es
  ) =>
  OsPath ->
  Text ->
  Eff es ()
appendFileUtf8 f = appendBinaryFile f . FS.UTF8.encodeUtf8
