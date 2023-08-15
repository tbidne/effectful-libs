{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Provides a Terminal effect.
--
-- @since 0.1
module Effectful.Terminal.Dynamic
  ( -- * Effect
    TerminalDynamic (..),
    TermSizeException (..),
    putStr,
    putStrLn,
    getChar,
    getLine,
#if MIN_VERSION_base(4,15,0)
    getContents',
#endif
    getTerminalSize,

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
    Natural,
    Window (..),
    Text,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad.IO.Class (MonadIO (liftIO))
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
import Effectful.Exception ( throwM)
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Natural (Natural)
import System.Console.Terminal.Size (Window (..), size)
import System.IO qualified as IO
import Prelude hiding (getChar, getLine, print, putStr, putStrLn)

-- | Terminal effect.
--
-- @since 0.1
data TerminalDynamic :: Effect where
  PutStr :: String -> TerminalDynamic m ()
  PutStrLn :: String -> TerminalDynamic m ()
  GetChar :: TerminalDynamic m Char
  GetLine :: TerminalDynamic m String
#if MIN_VERSION_base(4,15,0)
  GetContents' :: TerminalDynamic m String
#endif
  GetTerminalSize :: TerminalDynamic m (Window Natural)

{- ORMOLU_ENABLE -}

-- | @since 0.1
type instance DispatchOf TerminalDynamic = Dynamic

-- | @since 0.1
data TermSizeException = MkTermSizeException
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception TermSizeException where
  displayException = const "Failed to detect the terminal size."

{- ORMOLU_DISABLE -}

-- | Runs 'TerminalDynamic' in 'IO'.
--
-- @since 0.1
runTerminalDynamicIO ::
  ( 
    IOE :> es
  ) =>
  Eff (TerminalDynamic : es) a ->
  Eff es a
runTerminalDynamicIO = interpret $ \_ -> \case
  PutStr s -> liftIO $ IO.putStr s
  PutStrLn s -> liftIO $ IO.putStrLn s
  GetChar -> liftIO IO.getChar
  GetLine -> liftIO IO.getLine
#if MIN_VERSION_base(4,15,0)
  GetContents' -> liftIO IO.getContents'
#endif
  GetTerminalSize ->
    liftIO size >>= \case
      Just h -> pure h
      Nothing -> throwM MkTermSizeException

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

-- | @since 0.1
getTerminalSize :: (TerminalDynamic :> es) => Eff es (Window Natural)
getTerminalSize = send GetTerminalSize

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
getTextContents' :: ( TerminalDynamic :> es) => Eff es Text
getTextContents' = T.pack <$> getContents'

#endif

-- | Retrieves the terminal width.
--
-- @since 0.1
getTerminalWidth :: (TerminalDynamic :> es) => Eff es Natural
getTerminalWidth = width <$> getTerminalSize

-- | Retrieves the terminal height.
--
-- @since 0.1
getTerminalHeight :: (TerminalDynamic :> es) => Eff es Natural
getTerminalHeight = height <$> getTerminalSize
