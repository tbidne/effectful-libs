{-# LANGUAGE UndecidableInstances #-}

-- | Provides a dynamic effect for reading files.
--
-- @since 0.1
module Effectful.FileSystem.FileReader.Dynamic
  ( -- * Class
    MonadFileReader (..),

    -- * Effect
    FileReaderDynamic (..),

    -- ** Handlers
    runFileReaderDynamicIO,

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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, send)
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

-- | Dynamic effect for reading files.
--
-- @since 0.1
data FileReaderDynamic :: Effect where
  ReadBinaryFile :: OsPath -> FileReaderDynamic m ByteString

-- | @since 0.1
type instance DispatchOf FileReaderDynamic = Dynamic

-- | Runs 'FileReaderDynamic' in 'IO'.
--
-- @since 0.1
runFileReaderDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (FileReaderDynamic : es) a ->
  Eff es a
runFileReaderDynamicIO = interpret $ \_ -> \case
  ReadBinaryFile p -> liftIO $ readBinaryFileIO p

-- | @since 0.1
instance (FileReaderDynamic :> es) => MonadFileReader (Eff es) where
  readBinaryFile = send . ReadBinaryFile

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
