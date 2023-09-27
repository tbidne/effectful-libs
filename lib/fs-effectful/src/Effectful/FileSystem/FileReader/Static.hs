{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for reading files.
--
-- @since 0.1
module Effectful.FileSystem.FileReader.Static
  ( -- * Class
    MonadFileReader (..),

    -- * Effect
    FileReaderStatic,

    -- ** Handlers
    runFileReaderStaticIO,

    -- * UTF-8 Utils
    readFileUtf8,
    readFileUtf8Lenient,
    readFileUtf8ThrowM,

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
import Effectful.Exception (MonadThrow)
import Effectful.FileSystem.Utils (OsPath, readBinaryFileIO)
import Effectful.FileSystem.Utils qualified as Utils

-- | Represents file-system reader effects.
--
-- @since 0.1
class (Monad m) => MonadFileReader m where
  -- | Reads a file.
  --
  -- @since 0.1
  readBinaryFile :: OsPath -> m ByteString

-- | @since 0.1
instance MonadFileReader IO where
  readBinaryFile = readBinaryFileIO

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
instance (FileReaderStatic :> es) => MonadFileReader (Eff es) where
  readBinaryFile = unsafeEff_ . readBinaryFileIO

-- | Reads a file as UTF-8.
--
-- @since 0.1
readFileUtf8 ::
  ( MonadFileReader m
  ) =>
  OsPath ->
  m (Either UnicodeException Text)
readFileUtf8 = fmap Utils.decodeUtf8 . readBinaryFile

-- | Reads a file as UTF-8 in lenient mode.
--
-- @since 0.1
readFileUtf8Lenient ::
  ( MonadFileReader m
  ) =>
  OsPath ->
  m Text
readFileUtf8Lenient = fmap Utils.decodeUtf8Lenient . readBinaryFile

-- | Decodes a file as UTF-8. Throws 'UnicodeException' for decode errors.
--
-- @since 0.1
readFileUtf8ThrowM ::
  ( MonadFileReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m Text
readFileUtf8ThrowM = readBinaryFile >=> Utils.decodeUtf8ThrowM
