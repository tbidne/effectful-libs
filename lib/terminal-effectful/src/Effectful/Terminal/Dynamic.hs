{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- ORMOLU_DISABLE -}

-- | Provides a dynamic terminal effect.
--
-- @since 0.1
module Effectful.Terminal.Dynamic
  ( -- * Effect
    TerminalDynamic (..),
    putStr,
    putStrLn,
    putBinary,
    getChar,
    getLine,
#if MIN_VERSION_base(4,15,0)
    getContents',
#endif
    getTerminalSize,
    supportsPretty,

    -- ** Handlers
    runTerminalDynamicIO,

    -- * Functions
    print,

    -- * Text
    putText,
    putTextLn,
    getTextLine,
#if MIN_VERSION_base(4,15,0)
    getTextContents',
#endif

    -- * Window
    getTerminalWidth,
    getTerminalHeight,

    -- * Re-exports
    Window (..),
    Text,
  )
where

{- ORMOLU_ENABLE -}

import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.IO.Exception
  ( IOErrorType (SystemError),
    IOException
      ( IOError,
        ioe_description,
        ioe_errno,
        ioe_filename,
        ioe_handle,
        ioe_location,
        ioe_type
      ),
  )
import System.Console.Pretty qualified as CPretty
import System.Console.Terminal.Size (Window (Window, height, width), size)
import System.IO qualified as IO
import Prelude
  ( Applicative (pure),
    Bool,
    Char,
    Integral,
    Maybe (Just, Nothing),
    Monad ((>>=)),
    Show (show),
    String,
    ($),
    (.),
    (<$>),
  )

{- ORMOLU_DISABLE -}

-- | Dynamic terminal effect.
--
-- @since 0.1
data TerminalDynamic :: Effect where
  PutStr :: String -> TerminalDynamic m ()
  PutStrLn :: String -> TerminalDynamic m ()
  PutBinary :: ByteString -> TerminalDynamic m ()
  GetChar :: TerminalDynamic m Char
  GetLine :: TerminalDynamic m String
#if MIN_VERSION_base(4,15,0)
  GetContents' :: TerminalDynamic m String
#endif
  GetTerminalSize :: Integral a => TerminalDynamic m (Window a)
  SupportsPretty :: TerminalDynamic m Bool

-- | @since 0.1
type instance DispatchOf TerminalDynamic = Dynamic

-- | Runs 'TerminalDynamic' in 'IO'.
--
-- @since 0.1
runTerminalDynamicIO :: (IOE :> es) => Eff (TerminalDynamic : es) a -> Eff es a
runTerminalDynamicIO = interpret $ \_ -> \case
  PutStr s -> liftIO $ IO.putStr s
  PutStrLn s -> liftIO $ IO.putStrLn s
  PutBinary s -> liftIO $ BS.putStr s
  GetChar -> liftIO IO.getChar
  GetLine -> liftIO IO.getLine
#if MIN_VERSION_base(4,15,0)
  GetContents' -> liftIO IO.getContents'
#endif
  GetTerminalSize ->
    liftIO size >>= \case
      Just h -> pure h
      Nothing ->
        throwM
          $ IOError
            { ioe_handle = Nothing,
              ioe_type = SystemError,
              ioe_location = "getTerminalSize",
              ioe_description = "Failed to detect the terminal size",
              ioe_errno = Nothing,
              ioe_filename = Nothing
            }
  SupportsPretty -> liftIO CPretty.supportsPretty

{- ORMOLU_ENABLE -}

-- | Lifted 'IO.putStr'.
--
-- @since 0.1
putStr :: (TerminalDynamic :> es) => String -> Eff es ()
putStr = send . PutStr

-- | Lifted 'IO.putStrLn'.
--
-- @since 0.1
putStrLn :: (TerminalDynamic :> es) => String -> Eff es ()
putStrLn = send . PutStrLn

-- | Lifted 'BS.putStr'.
--
-- @since 0.1
putBinary :: (TerminalDynamic :> es) => ByteString -> Eff es ()
putBinary = send . PutBinary

-- | Lifted 'IO.getChar'.
--
-- @since 0.1
getChar :: (TerminalDynamic :> es) => Eff es Char
getChar = send GetChar

-- | Lifted 'IO.getLine'.
--
-- @since 0.1
getLine :: (TerminalDynamic :> es) => Eff es String
getLine = send GetLine

#if MIN_VERSION_base(4,15,0)

-- | Lifted 'IO.getContents''.
--
-- @since 0.1
getContents' :: ( TerminalDynamic :> es) => Eff es String
getContents' = send GetContents'

#endif

-- | Retrieves the terminal size.
--
-- @since 0.1
getTerminalSize :: (Integral a, TerminalDynamic :> es) => Eff es (Window a)
getTerminalSize = send GetTerminalSize

-- | Determines if we support ANSI styling.
--
-- @since 0.1
supportsPretty :: (TerminalDynamic :> es) => Eff es Bool
supportsPretty = send SupportsPretty

-- | @since 0.1
print :: (Show a, TerminalDynamic :> es) => a -> Eff es ()
print = putStrLn . show

-- | 'Text' version of 'putStr'.
--
-- @since 0.1
putText :: (TerminalDynamic :> es) => Text -> Eff es ()
putText = putStr . T.unpack

-- | 'Text' version of 'putStrLn'.
--
-- @since 0.1
putTextLn :: (TerminalDynamic :> es) => Text -> Eff es ()
putTextLn = putStrLn . T.unpack

-- | @since 0.1
getTextLine :: (TerminalDynamic :> es) => Eff es Text
getTextLine = T.pack <$> getLine

#if MIN_VERSION_base(4,15,0)

-- | @since 0.1
getTextContents' :: (TerminalDynamic :> es) => Eff es Text
getTextContents' = T.pack <$> getContents'

#endif

-- | Retrieves the terminal width.
--
-- @since 0.1
getTerminalWidth :: (Integral a, TerminalDynamic :> es) => Eff es a
getTerminalWidth = width <$> getTerminalSize

-- | Retrieves the terminal height.
--
-- @since 0.1
getTerminalHeight :: (Integral a, TerminalDynamic :> es) => Eff es a
getTerminalHeight = height <$> getTerminalSize
