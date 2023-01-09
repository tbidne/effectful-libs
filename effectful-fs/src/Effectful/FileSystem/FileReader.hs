-- | Provides an effect for reading files.
--
-- @since 0.1
module Effectful.FileSystem.FileReader
  ( -- * Effect
    EffectFileReader (..),
    Path,

    -- * Handler
    runFileReaderIO,

    -- * Functions
    readBinaryFile,

    -- * UTF-8 Utils
    readFileUtf8,
    readFileUtf8Lenient,
    readFileUtf8ThrowM,
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,

    -- * Reexports
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
import Effectful.CallStack
  ( EffectCallStack,
    addCallStack,
    throwWithCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.FileSystem.Path (Path, readBinaryFileIO)
import GHC.Stack (HasCallStack)

-- | Effect for reading files.
--
-- @since 0.1
data EffectFileReader :: Effect where
  ReadBinaryFile :: HasCallStack => Path -> EffectFileReader m ByteString

-- | @since 0.1
type instance DispatchOf EffectFileReader = Dynamic

-- | Runs 'FileReader' in 'IO'.
--
-- @since 0.1
runFileReaderIO ::
  ( EffectCallStack :> es,
    IOE :> es
  ) =>
  Eff (EffectFileReader : es) a ->
  Eff es a
runFileReaderIO = interpret $ \_ -> \case
  ReadBinaryFile p -> addCallStack $ liftIO $ readBinaryFileIO p

-- | @since 0.1
readBinaryFile ::
  ( EffectFileReader :> es,
    HasCallStack
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
  ( EffectCallStack :> es,
    HasCallStack
  ) =>
  ByteString ->
  Eff es Text
decodeUtf8ThrowM =
  TEnc.decodeUtf8' >.> \case
    Right txt -> pure txt
    Left ex -> throwWithCallStack ex

-- | Reads a file as UTF-8.
--
-- @since 0.1
readFileUtf8 ::
  ( EffectFileReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es (Either UnicodeException Text)
readFileUtf8 = fmap decodeUtf8 . readBinaryFile

-- | Reads a file as UTF-8 in lenient mode.
--
-- @since 0.1
readFileUtf8Lenient ::
  ( EffectFileReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Text
readFileUtf8Lenient = fmap decodeUtf8Lenient . readBinaryFile

-- | Decodes a file as UTF-8. Throws 'UnicodeException' for decode errors.
--
-- @since 0.1
readFileUtf8ThrowM ::
  ( EffectCallStack :> es,
    EffectFileReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Text
readFileUtf8ThrowM = readBinaryFile >=> decodeUtf8ThrowM

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixl 9 >.>
