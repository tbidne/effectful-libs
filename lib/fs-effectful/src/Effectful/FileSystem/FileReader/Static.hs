{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for reading files.
--
-- @since 0.1
module Effectful.FileSystem.FileReader.Static
  ( -- * Effect
    FileReaderStatic,
    readBinaryFile,

    -- ** Handlers
    runFileReaderStaticIO,

    -- * UTF-8 Utils
    readFileUtf8,
    readFileUtf8Lenient,
    readFileUtf8ThrowM,
    FS.UTF8.decodeUtf8,
    FS.UTF8.decodeUtf8Lenient,
    FS.UTF8.decodeUtf8ThrowM,

    -- * Re-exports
    ByteString,
    OsPath,
    Text,
    UnicodeException,
  )
where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
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
import FileSystem.IO (readBinaryFileIO)
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8

-- | Static effect for reading files.
--
-- @since 0.1
data FileReaderStatic :: Effect

type instance DispatchOf FileReaderStatic = Static WithSideEffects

data instance StaticRep FileReaderStatic = MkFileReaderStatic

-- | Runs 'FileReaderStatic' in 'IO'.
--
-- @since 0.1
runFileReaderStaticIO ::
  (IOE :> es) =>
  Eff (FileReaderStatic : es) a ->
  Eff es a
runFileReaderStaticIO = evalStaticRep MkFileReaderStatic

-- | @since 0.1
readBinaryFile ::
  ( FileReaderStatic :> es
  ) =>
  OsPath ->
  Eff es ByteString
readBinaryFile = unsafeEff_ . readBinaryFileIO

-- | Reads a file as UTF-8.
--
-- @since 0.1
readFileUtf8 ::
  ( FileReaderStatic :> es
  ) =>
  OsPath ->
  Eff es (Either UnicodeException Text)
readFileUtf8 = fmap FS.UTF8.decodeUtf8 . readBinaryFile

-- | Reads a file as UTF-8 in lenient mode.
--
-- @since 0.1
readFileUtf8Lenient ::
  ( FileReaderStatic :> es
  ) =>
  OsPath ->
  Eff es Text
readFileUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . readBinaryFile

-- | Decodes a file as UTF-8. Throws 'UnicodeException' for decode errors.
--
-- @since 0.1
readFileUtf8ThrowM ::
  ( FileReaderStatic :> es
  ) =>
  OsPath ->
  Eff es Text
readFileUtf8ThrowM = readBinaryFile >=> FS.UTF8.decodeUtf8ThrowM
