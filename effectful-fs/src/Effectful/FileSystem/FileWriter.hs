{-# LANGUAGE TemplateHaskell #-}

-- | Provides an effect for writing files.
--
-- @since 0.1
module Effectful.FileSystem.FileWriter
  ( -- * Class
    FileWriter (..),
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
  ( EffectCallStack,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.FileSystem.Path (Path, appendBinaryFileIO, writeBinaryFileIO)
import Effectful.TH (makeEffect_)
import GHC.Stack (HasCallStack)

-- | Effect for reading files.
--
-- @since 0.1
data FileWriter :: Effect where
  WriteBinaryFile :: HasCallStack => Path -> ByteString -> FileWriter m ()
  AppendBinaryFile :: HasCallStack => Path -> ByteString -> FileWriter m ()

-- | @since 0.1
type instance DispatchOf FileWriter = Dynamic

-- | Runs 'FileWriter' in 'IO'.
--
-- @since 0.1
runFileWriterIO ::
  ( EffectCallStack :> es,
    IOE :> es
  ) =>
  Eff (FileWriter : es) a ->
  Eff es a
runFileWriterIO = interpret $ \_ -> \case
  WriteBinaryFile p bs -> addCallStack $ liftIO $ writeBinaryFileIO p bs
  AppendBinaryFile p bs -> addCallStack $ liftIO $ appendBinaryFileIO p bs

makeEffect_ ''FileWriter

-- | @since 0.1
writeBinaryFile ::
  ( HasCallStack,
    FileWriter :> es
  ) =>
  Path ->
  ByteString ->
  Eff es ()

-- | @since 0.1
appendBinaryFile ::
  ( HasCallStack,
    FileWriter :> es
  ) =>
  Path ->
  ByteString ->
  Eff es ()

{-}
-- | Represents file-system writer effects.
--
-- @since 0.1
class Monad m => MonadFileWriter m where
  -- | Writes to a file.
  --
  -- @since 0.1
  writeBinaryFile :: HasCallStack => Path -> ByteString -> m ()

  -- | Appends to a file.
  --
  -- @since 0.1
  appendBinaryFile :: HasCallStack => Path -> ByteString -> m ()

-- | @since 0.1
instance MonadFileWriter IO where
  writeBinaryFile f = addCallStack . BS.writeFile f
  appendBinaryFile f = addCallStack . BS.appendFile f

-- | @since 0.1
instance MonadFileWriter m => MonadFileWriter (ReaderT env m) where
  writeBinaryFile f = lift . writeBinaryFile f
  appendBinaryFile f = lift . appendBinaryFile f-}

-- | Encodes a 'Text' to 'ByteString'.
--
-- @since 0.1
encodeUtf8 :: Text -> ByteString
encodeUtf8 = TEnc.encodeUtf8

-- | Writes to a file.
--
-- @since 0.1
writeFileUtf8 :: (HasCallStack, FileWriter :> es) => Path -> Text -> Eff es ()
writeFileUtf8 f = writeBinaryFile f . encodeUtf8

-- | Appends to a file.
--
-- @since 0.1
appendFileUtf8 :: (HasCallStack, FileWriter :> es) => Path -> Text -> Eff es ()
appendFileUtf8 f = appendBinaryFile f . encodeUtf8
