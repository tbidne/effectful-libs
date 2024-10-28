-- | Provides a dynamic effect for reading files.
--
-- @since 0.1
module Effectful.FileSystem.FileReader.Dynamic
  ( -- * Effect
    FileReaderDynamic (..),
    readBinaryFile,

    -- ** Handlers
    runFileReaderDynamicIO,

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
import FileSystem.IO (readBinaryFileIO)
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8

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
readBinaryFile ::
  ( FileReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es ByteString
readBinaryFile = send . ReadBinaryFile

-- | Reads a file as UTF-8.
--
-- @since 0.1
readFileUtf8 ::
  ( FileReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es (Either UnicodeException Text)
readFileUtf8 = fmap FS.UTF8.decodeUtf8 . readBinaryFile

-- | Reads a file as UTF-8 in lenient mode.
--
-- @since 0.1
readFileUtf8Lenient ::
  ( FileReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Text
readFileUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . readBinaryFile

-- | Decodes a file as UTF-8. Throws 'UnicodeException' for decode errors.
--
-- @since 0.1
readFileUtf8ThrowM ::
  ( FileReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Text
readFileUtf8ThrowM = readBinaryFile >=> FS.UTF8.decodeUtf8ThrowM
