{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- ORMOLU_DISABLE -}

-- | Provides a dynamic terminal effect.
--
-- @since 0.1
module Effectful.Terminal.Dynamic
  ( -- * Effect
    Terminal (..),
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
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret_, send)
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.Terminal.Static qualified as Static
import System.Console.Terminal.Size (Window (Window, height, width))
import Prelude
  ( Bool,
    Char,
    Integral,
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
data Terminal :: Effect where
  PutStr :: String -> Terminal m ()
  PutStrLn :: String -> Terminal m ()
  PutBinary :: ByteString -> Terminal m ()
  GetChar :: Terminal m Char
  GetLine :: Terminal m String
#if MIN_VERSION_base(4,15,0)
  GetContents' :: Terminal m String
#endif
  GetTerminalSize :: Integral a => Terminal m (Window a)
  SupportsPretty :: Terminal m Bool


-- | @since 0.1
instance ShowEffect Terminal where
  showEffectCons = \case
    PutStr _ -> "PutStr"
    PutStrLn _ -> "PutStrLn"
    PutBinary _ -> "PutBinary"
    GetChar -> "GetChar"
    GetLine -> "GetLine"
#if MIN_VERSION_base(4,15,0)
    GetContents' -> "GetContents'"
#endif
    GetTerminalSize -> "GetTerminalSize"
    SupportsPretty -> "SupportsPretty"

-- | @since 0.1
type instance DispatchOf Terminal = Dynamic

-- | Runs 'Terminal' in 'Prelude.IO'.
--
-- @since 0.1
runTerminal :: (HasCallStack, IOE :> es) => Eff (Terminal : es) a -> Eff es a
runTerminal = reinterpret_ Static.runTerminal $ \case
  PutStr s -> Static.putStr s
  PutStrLn s -> Static.putStrLn s
  PutBinary s -> Static.putBinary s
  GetChar -> Static.getChar
  GetLine -> Static.getLine
#if MIN_VERSION_base(4,15,0)
  GetContents' -> Static.getContents'
#endif
  GetTerminalSize -> Static.getTerminalSize
  SupportsPretty -> Static.supportsPretty

{- ORMOLU_ENABLE -}

-- | Lifted 'IO.putStr'.
--
-- @since 0.1
putStr :: (HasCallStack, Terminal :> es) => String -> Eff es ()
putStr = send . PutStr

-- | Lifted 'IO.putStrLn'.
--
-- @since 0.1
putStrLn :: (HasCallStack, Terminal :> es) => String -> Eff es ()
putStrLn = send . PutStrLn

-- | Lifted 'BS.putStr'.
--
-- @since 0.1
putBinary :: (HasCallStack, Terminal :> es) => ByteString -> Eff es ()
putBinary = send . PutBinary

-- | Lifted 'IO.getChar'.
--
-- @since 0.1
getChar :: (HasCallStack, Terminal :> es) => Eff es Char
getChar = send GetChar

-- | Lifted 'IO.getLine'.
--
-- @since 0.1
getLine :: (HasCallStack, Terminal :> es) => Eff es String
getLine = send GetLine

#if MIN_VERSION_base(4,15,0)

-- | Lifted 'IO.getContents''.
--
-- @since 0.1
getContents' :: (HasCallStack, Terminal :> es) => Eff es String
getContents' = send GetContents'

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
getTerminalSize = send GetTerminalSize

-- | Determines if we support ANSI styling.
--
-- @since 0.1
supportsPretty :: (HasCallStack, Terminal :> es) => Eff es Bool
supportsPretty = send SupportsPretty

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
