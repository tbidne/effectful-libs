-- | Provides an effect for writing files.
--
-- @since 0.1
module Effectful.FileSystem.FileWriter.Dynamic
  ( -- * Effect
    FileWriterDynamic (..),
    Path,
    writeBinaryFile,
    appendBinaryFile,

    -- ** Handlers
    runFileWriterDynamicIO,

    -- * UTF-8 Utils
    writeFileUtf8,
    appendFileUtf8,
    encodeUtf8,

    -- * Re-exports
    ByteString,
    Text,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.FileSystem.Internal
  ( Path,
    appendBinaryFileIO,
    writeBinaryFileIO,
  )

-- | Effect for reading files.
--
-- @since 0.1
data FileWriterDynamic :: Effect where
  WriteBinaryFile :: Path -> ByteString -> FileWriterDynamic m ()
  AppendBinaryFile :: Path -> ByteString -> FileWriterDynamic m ()

-- | @since 0.1
type instance DispatchOf FileWriterDynamic = Dynamic

-- | Runs 'FileWriter' in 'IO'.
--
-- @since 0.1
runFileWriterDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (FileWriterDynamic : es) a ->
  Eff es a
runFileWriterDynamicIO = interpret $ \_ -> \case
  WriteBinaryFile p bs -> liftIO $ writeBinaryFileIO p bs
  AppendBinaryFile p bs -> liftIO $ appendBinaryFileIO p bs

-- | @since 0.1
writeBinaryFile ::
  ( FileWriterDynamic :> es
  ) =>
  Path ->
  ByteString ->
  Eff es ()
writeBinaryFile p = send . WriteBinaryFile p

-- | @since 0.1
appendBinaryFile ::
  ( FileWriterDynamic :> es
  ) =>
  Path ->
  ByteString ->
  Eff es ()
appendBinaryFile p = send . AppendBinaryFile p

-- | Encodes a 'Text' to 'ByteString'.
--
-- @since 0.1
encodeUtf8 :: Text -> ByteString
encodeUtf8 = TEnc.encodeUtf8

-- | Writes to a file.
--
-- @since 0.1
writeFileUtf8 ::
  ( FileWriterDynamic :> es
  ) =>
  Path ->
  Text ->
  Eff es ()
writeFileUtf8 f = writeBinaryFile f . encodeUtf8

-- | Appends to a file.
--
-- @since 0.1
appendFileUtf8 ::
  ( FileWriterDynamic :> es
  ) =>
  Path ->
  Text ->
  Eff es ()
appendFileUtf8 f = appendBinaryFile f . encodeUtf8
