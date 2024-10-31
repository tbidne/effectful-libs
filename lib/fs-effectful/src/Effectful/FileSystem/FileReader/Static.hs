{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for reading files.
--
-- @since 0.1
module Effectful.FileSystem.FileReader.Static
  ( -- * Effect
    FileReader,
    readBinaryFile,

    -- ** Handlers
    runFileReader,

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
  ( HasCallStack,
    SideEffects (WithSideEffects),
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
data FileReader :: Effect

type instance DispatchOf FileReader = Static WithSideEffects

data instance StaticRep FileReader = MkFileReader

-- | Runs 'FileReader' in 'IO'.
--
-- @since 0.1
runFileReader ::
  (HasCallStack, IOE :> es) =>
  Eff (FileReader : es) a ->
  Eff es a
runFileReader = evalStaticRep MkFileReader

-- | @since 0.1
readBinaryFile ::
  ( FileReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es ByteString
readBinaryFile = unsafeEff_ . readBinaryFileIO

-- | Reads a file as UTF-8.
--
-- @since 0.1
readFileUtf8 ::
  ( FileReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es (Either UnicodeException Text)
readFileUtf8 = fmap FS.UTF8.decodeUtf8 . readBinaryFile

-- | Reads a file as UTF-8 in lenient mode.
--
-- @since 0.1
readFileUtf8Lenient ::
  ( FileReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es Text
readFileUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . readBinaryFile

-- | Decodes a file as UTF-8. Throws 'UnicodeException' for decode errors.
--
-- @since 0.1
readFileUtf8ThrowM ::
  ( FileReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es Text
readFileUtf8ThrowM = readBinaryFile >=> FS.UTF8.decodeUtf8ThrowM
