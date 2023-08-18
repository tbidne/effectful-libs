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
import Effectful.FileSystem.Utils (OsPath, readBinaryFileIO)
import Effectful.FileSystem.Utils qualified as Utils

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
readFileUtf8 = fmap Utils.decodeUtf8 . readBinaryFile

-- | Reads a file as UTF-8 in lenient mode.
--
-- @since 0.1
readFileUtf8Lenient ::
  ( FileReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Text
readFileUtf8Lenient = fmap Utils.decodeUtf8Lenient . readBinaryFile

-- | Decodes a file as UTF-8. Throws 'UnicodeException' for decode errors.
--
-- @since 0.1
readFileUtf8ThrowM ::
  ( FileReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es Text
readFileUtf8ThrowM = readBinaryFile >=> Utils.decodeUtf8ThrowM
