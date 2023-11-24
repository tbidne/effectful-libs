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
    decodeUtf8ThrowM,

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
import Data.Text.Encoding qualified as TEnc
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
import Effectful.Exception (throwM)
import Effectful.FileSystem.Utils (OsPath, readBinaryFileIO, (>.>))
import Effectful.FileSystem.Utils qualified as Utils

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

-- | Decodes a 'ByteString' to UTF-8. Can throw 'UnicodeException'.
--
-- @since 0.1
decodeUtf8ThrowM ::
  ByteString ->
  Eff es Text
decodeUtf8ThrowM =
  TEnc.decodeUtf8' >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | Reads a file as UTF-8.
--
-- @since 0.1
readFileUtf8 ::
  ( FileReaderStatic :> es
  ) =>
  OsPath ->
  Eff es (Either UnicodeException Text)
readFileUtf8 = fmap Utils.decodeUtf8 . readBinaryFile

-- | Reads a file as UTF-8 in lenient mode.
--
-- @since 0.1
readFileUtf8Lenient ::
  ( FileReaderStatic :> es
  ) =>
  OsPath ->
  Eff es Text
readFileUtf8Lenient = fmap Utils.decodeUtf8Lenient . readBinaryFile

-- | Decodes a file as UTF-8. Throws 'UnicodeException' for decode errors.
--
-- @since 0.1
readFileUtf8ThrowM ::
  ( FileReaderStatic :> es
  ) =>
  OsPath ->
  Eff es Text
readFileUtf8ThrowM = readBinaryFile >=> decodeUtf8ThrowM
