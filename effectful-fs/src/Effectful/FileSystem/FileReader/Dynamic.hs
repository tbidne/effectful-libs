-- | Provides an effect for reading files.
--
-- @since 0.1
module Effectful.FileSystem.FileReader.Dynamic
  ( -- * Effect
    FileReaderEffect (..),
    Path,
    readBinaryFile,

    -- ** Handlers
    runFileReaderIO,

    -- * UTF-8 Utils
    readFileUtf8,
    readFileUtf8Lenient,
    readFileUtf8ThrowM,
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,

    -- * Re-exports
    ByteString,
    Text,
    UnicodeException,
  )
where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Encoding.Error qualified as TEncError
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Exception (throwM)
import Effectful.FileSystem.Internal (Path, readBinaryFileIO)

-- | Effect for reading files.
--
-- @since 0.1
data FileReaderEffect :: Effect where
  ReadBinaryFile :: Path -> FileReaderEffect m ByteString

-- | @since 0.1
type instance DispatchOf FileReaderEffect = Dynamic

-- | Runs 'FileReader' in 'IO'.
--
-- @since 0.1
runFileReaderIO ::
  ( IOE :> es
  ) =>
  Eff (FileReaderEffect : es) a ->
  Eff es a
runFileReaderIO = interpret $ \_ -> \case
  ReadBinaryFile p -> liftIO $ readBinaryFileIO p

-- | @since 0.1
readBinaryFile ::
  ( FileReaderEffect :> es
  ) =>
  Path ->
  Eff es ByteString
readBinaryFile = send . ReadBinaryFile

-- | Decodes a 'ByteString' to UTF-8.
--
-- @since 0.1
decodeUtf8 :: ByteString -> Either UnicodeException Text
decodeUtf8 = TEnc.decodeUtf8'

-- | Leniently decodes a 'ByteString' to UTF-8.
--
-- @since 0.1
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TEnc.decodeUtf8With TEncError.lenientDecode

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
  ( FileReaderEffect :> es
  ) =>
  Path ->
  Eff es (Either UnicodeException Text)
readFileUtf8 = fmap decodeUtf8 . readBinaryFile

-- | Reads a file as UTF-8 in lenient mode.
--
-- @since 0.1
readFileUtf8Lenient ::
  ( FileReaderEffect :> es
  ) =>
  Path ->
  Eff es Text
readFileUtf8Lenient = fmap decodeUtf8Lenient . readBinaryFile

-- | Decodes a file as UTF-8. Throws 'UnicodeException' for decode errors.
--
-- @since 0.1
readFileUtf8ThrowM ::
  ( FileReaderEffect :> es
  ) =>
  Path ->
  Eff es Text
readFileUtf8ThrowM = readBinaryFile >=> decodeUtf8ThrowM

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixl 9 >.>
