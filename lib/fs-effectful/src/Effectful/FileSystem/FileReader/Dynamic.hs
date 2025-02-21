-- | Provides a dynamic effect for reading files.
--
-- @since 0.1
module Effectful.FileSystem.FileReader.Dynamic
  ( -- * Effect
    FileReader (..),
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
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret_, send)
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.FileSystem.FileReader.Static qualified as Static
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8

-- | Dynamic effect for reading files.
--
-- @since 0.1
data FileReader :: Effect where
  ReadBinaryFile :: OsPath -> FileReader m ByteString

-- | @since 0.1
type instance DispatchOf FileReader = Dynamic

-- | @since 0.1
instance ShowEffect FileReader where
  showEffectCons = \case
    ReadBinaryFile _ -> "ReadBinaryFile"

-- | Runs 'FileReader' in 'IO'.
--
-- @since 0.1
runFileReader ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (FileReader : es) a ->
  Eff es a
runFileReader = reinterpret_ Static.runFileReader $ \case
  ReadBinaryFile p -> Static.readBinaryFile p

-- | @since 0.1
readBinaryFile ::
  ( FileReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es ByteString
readBinaryFile = send . ReadBinaryFile

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
