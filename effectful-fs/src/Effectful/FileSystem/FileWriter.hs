-- | Provides an effect for writing files.
--
-- @since 0.1
module Effectful.FileSystem.FileWriter
  ( -- * Class
    FileWriterEffect (..),
    Path,

    -- * Handler
    runFileWriterIO,

    -- * Functions
    writeBinaryFile,
    appendBinaryFile,

    -- * UTF-8 Utils
    writeFileUtf8,
    appendFileUtf8,
    encodeUtf8,

    -- * Reexports
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
import Effectful.CallStack
  ( CallStackEffect,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.FileSystem.Path (Path, appendBinaryFileIO, writeBinaryFileIO)
import GHC.Stack (HasCallStack)

-- | Effect for reading files.
--
-- @since 0.1
data FileWriterEffect :: Effect where
  WriteBinaryFile :: HasCallStack => Path -> ByteString -> FileWriterEffect m ()
  AppendBinaryFile :: HasCallStack => Path -> ByteString -> FileWriterEffect m ()

-- | @since 0.1
type instance DispatchOf FileWriterEffect = Dynamic

-- | Runs 'FileWriter' in 'IO'.
--
-- @since 0.1
runFileWriterIO ::
  ( CallStackEffect :> es,
    IOE :> es
  ) =>
  Eff (FileWriterEffect : es) a ->
  Eff es a
runFileWriterIO = interpret $ \_ -> \case
  WriteBinaryFile p bs -> addCallStack $ liftIO $ writeBinaryFileIO p bs
  AppendBinaryFile p bs -> addCallStack $ liftIO $ appendBinaryFileIO p bs

-- | @since 0.1
writeBinaryFile ::
  ( FileWriterEffect :> es,
    HasCallStack
  ) =>
  Path ->
  ByteString ->
  Eff es ()
writeBinaryFile p = send . WriteBinaryFile p

-- | @since 0.1
appendBinaryFile ::
  ( FileWriterEffect :> es,
    HasCallStack
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
  ( FileWriterEffect :> es,
    HasCallStack
  ) =>
  Path ->
  Text ->
  Eff es ()
writeFileUtf8 f = writeBinaryFile f . encodeUtf8

-- | Appends to a file.
--
-- @since 0.1
appendFileUtf8 ::
  ( FileWriterEffect :> es,
    HasCallStack
  ) =>
  Path ->
  Text ->
  Eff es ()
appendFileUtf8 f = appendBinaryFile f . encodeUtf8
