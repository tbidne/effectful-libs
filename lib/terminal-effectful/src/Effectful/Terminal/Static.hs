{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- ORMOLU_DISABLE -}

-- | Provides a static terminal effect.
--
-- @since 0.1
module Effectful.Terminal.Static
  ( -- * Effect
    Terminal,
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
    runTerminal,

    -- * Functions
    print,

    -- * Text
    putText,
    putTextLn,
    getTextLine,
#if MIN_VERSION_base(4,15,0)
    getTextContents',
#endif

    -- * ByteString
    putBinaryLn,

    -- * Window
    getTerminalWidth,
    getTerminalHeight,

    -- * Re-exports
    Window (..),
    Text,
  )
where

{- ORMOLU_ENABLE -}

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( HasCallStack,
    SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    unsafeEff_,
  )
import Effectful.Exception (throwIO)
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
    Semigroup ((<>)),
    Show (show),
    String,
    ($),
    (.),
    (<$>),
  )

-- | Static terminal effect.
--
-- @since 0.1
data Terminal :: Effect

type instance DispatchOf Terminal = Static WithSideEffects

data instance StaticRep Terminal = MkTerminal

-- | Runs 'Terminal' in 'Prelude.IO'.
--
-- @since 0.1
runTerminal :: (HasCallStack, IOE :> es) => Eff (Terminal : es) a -> Eff es a
runTerminal = evalStaticRep MkTerminal

-- | Lifted 'IO.putStr'.
--
-- @since 0.1
putStr :: (HasCallStack, Terminal :> es) => String -> Eff es ()
putStr = unsafeEff_ . IO.putStr

-- | Lifted 'IO.putStrLn'.
--
-- @since 0.1
putStrLn :: (HasCallStack, Terminal :> es) => String -> Eff es ()
putStrLn = unsafeEff_ . IO.putStrLn

-- | Lifted 'BS.putStr'.
--
-- @since 0.1
putBinary :: (HasCallStack, Terminal :> es) => ByteString -> Eff es ()
putBinary = unsafeEff_ . BS.putStr

-- | 'putBinary' with appended newline.
--
-- @since 0.1
putBinaryLn :: (HasCallStack, Terminal :> es) => ByteString -> Eff es ()
putBinaryLn = putBinary . (<> "\n")

-- | Lifted 'IO.getChar'.
--
-- @since 0.1
getChar :: (HasCallStack, Terminal :> es) => Eff es Char
getChar = unsafeEff_ IO.getChar

-- | Lifted 'IO.getLine'.
--
-- @since 0.1
getLine :: (HasCallStack, Terminal :> es) => Eff es String
getLine = unsafeEff_ IO.getLine

#if MIN_VERSION_base(4,15,0)

-- | Lifted 'IO.getContents''.
--
-- @since 0.1
getContents' :: (HasCallStack, Terminal :> es) => Eff es String
getContents' = unsafeEff_ IO.getContents'

#endif

-- | Retrieves the terminal size.
--
-- @since 0.1
getTerminalSize ::
  ( HasCallStack,
    Integral a,
    Terminal :> es
  ) =>
  Eff es (Window a)
getTerminalSize =
  unsafeEff_ size >>= \case
    Just h -> pure h
    Nothing ->
      throwIO $
        IOError
          { ioe_handle = Nothing,
            ioe_type = SystemError,
            ioe_location = "getTerminalSize",
            ioe_description = "Failed to detect the terminal size",
            ioe_errno = Nothing,
            ioe_filename = Nothing
          }

-- | Determines if we support ANSI styling.
--
-- @since 0.1
supportsPretty :: (HasCallStack, Terminal :> es) => Eff es Bool
supportsPretty = unsafeEff_ CPretty.supportsPretty

-- | @since 0.1
print :: (Show a, HasCallStack, Terminal :> es) => a -> Eff es ()
print = putStrLn . show

-- | 'Text' version of 'putStr'.
--
-- @since 0.1
putText :: (HasCallStack, Terminal :> es) => Text -> Eff es ()
putText = putStr . T.unpack

-- | 'Text' version of 'putStrLn'.
--
-- @since 0.1
putTextLn :: (HasCallStack, Terminal :> es) => Text -> Eff es ()
putTextLn = putStrLn . T.unpack

-- | @since 0.1
getTextLine :: (HasCallStack, Terminal :> es) => Eff es Text
getTextLine = T.pack <$> getLine

#if MIN_VERSION_base(4,15,0)

-- | @since 0.1
getTextContents' :: (HasCallStack, Terminal :> es) => Eff es Text
getTextContents' = T.pack <$> getContents'

#endif

-- | Retrieves the terminal width.
--
-- @since 0.1
getTerminalWidth :: (HasCallStack, Integral a, Terminal :> es) => Eff es a
getTerminalWidth = width <$> getTerminalSize

-- | Retrieves the terminal height.
--
-- @since 0.1
getTerminalHeight :: (HasCallStack, Integral a, Terminal :> es) => Eff es a
getTerminalHeight = height <$> getTerminalSize
