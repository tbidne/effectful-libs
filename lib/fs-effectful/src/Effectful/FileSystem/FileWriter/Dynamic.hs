-- | Provides a dynamic effect for writing files.
--
-- @since 0.1
module Effectful.FileSystem.FileWriter.Dynamic
  ( -- * Effect
    FileWriter (..),
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
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret_, send)
import Effectful.FileSystem.FileWriter.Static qualified as Static
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8

-- | Dynamic effect for reading files.
--
-- @since 0.1
data FileWriter :: Effect where
  WriteBinaryFile :: OsPath -> ByteString -> FileWriter m ()
  AppendBinaryFile :: OsPath -> ByteString -> FileWriter m ()

-- | @since 0.1
type instance DispatchOf FileWriter = Dynamic

-- | Runs 'FileWriter' in 'IO'.
--
-- @since 0.1
runFileWriter ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (FileWriter : es) a ->
  Eff es a
runFileWriter = reinterpret_ Static.runFileWriter $ \case
  WriteBinaryFile p bs -> Static.writeBinaryFile p bs
  AppendBinaryFile p bs -> Static.appendBinaryFile p bs

-- | @since 0.1
writeBinaryFile ::
  ( FileWriter :> es,
    HasCallStack
  ) =>
  OsPath ->
  ByteString ->
  Eff es ()
writeBinaryFile p = send . WriteBinaryFile p

-- | @since 0.1
appendBinaryFile ::
  ( FileWriter :> es,
    HasCallStack
  ) =>
  OsPath ->
  ByteString ->
  Eff es ()
appendBinaryFile p = send . AppendBinaryFile p

-- | Writes to a file.
--
-- @since 0.1
writeFileUtf8 ::
  ( FileWriter :> es,
    HasCallStack
  ) =>
  OsPath ->
  Text ->
  Eff es ()
writeFileUtf8 f = writeBinaryFile f . FS.UTF8.encodeUtf8

-- | Appends to a file.
--
-- @since 0.1
appendFileUtf8 ::
  ( FileWriter :> es,
    HasCallStack
  ) =>
  OsPath ->
  Text ->
  Eff es ()
appendFileUtf8 f = appendBinaryFile f . FS.UTF8.encodeUtf8
