{-# LANGUAGE CPP #-}

-- | Provides a Terminal effect.
--
-- @since 0.1
module Effectful.Terminal
  ( -- * Effect
    TerminalEffect (..),
    TermSizeException (..),

    -- * Handler
    runTerminalIO,

    -- * Functions
    putStr,
    putStrLn,
    getChar,
    getLine,
#if MIN_VERSION_base(4,15,0)
    getContents',
#endif
    getTerminalSize,

    -- ** Text
    putText,
    putTextLn,
    getTextLine,
#if MIN_VERSION_base(4,15,0)
    getTextContents',
#endif

    -- ** Window
    getTerminalWidth,
    getTerminalHeight,

    -- * Reexports
    Natural,
    Window (..),
    Text,
  )
where

import Control.Exception ( Exception(displayException) )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import System.Console.Terminal.Size (Window (..), size)
import System.IO qualified as IO
import Prelude hiding (getChar, getLine, putStr, putStrLn)
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
    throwWithCallStack
  )
import Effectful.Dispatch.Dynamic (interpret, send)

-- | Terminal effect.
--
-- @since 0.1
data TerminalEffect :: Effect where
  PutStr :: HasCallStack => String -> TerminalEffect m ()
  PutStrLn :: HasCallStack => String -> TerminalEffect m ()
  GetChar :: HasCallStack => TerminalEffect m Char
  GetLine :: HasCallStack => TerminalEffect m String
#if MIN_VERSION_base(4,15,0)
  GetContents' :: HasCallStack => TerminalEffect m String
#endif
  GetTerminalSize :: HasCallStack => TerminalEffect m (Window Natural)

-- | @since 0.1
type instance DispatchOf TerminalEffect = Dynamic

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

-- | Runs 'TerminalEffect' in 'IO'.
--
-- @since 0.1
runTerminalIO ::
  ( CallStackEffect :> es,
    IOE :> es
  ) =>
  Eff (TerminalEffect : es) a ->
  Eff es a
runTerminalIO = interpret $ \_ -> \case
  PutStr s -> addCallStack $ liftIO $ IO.putStr s
  PutStrLn s -> addCallStack $ liftIO $ IO.putStrLn s
  GetChar -> addCallStack $ liftIO IO.getChar
  GetLine -> addCallStack $ liftIO IO.getLine
#if MIN_VERSION_base(4,15,0)
  GetContents' -> addCallStack $ liftIO IO.getContents'
#endif
  GetTerminalSize ->
    liftIO size >>= \case
    Just h -> pure h
    Nothing -> throwWithCallStack MkTermSizeException

-- | @since 0.1
putStr :: (HasCallStack, TerminalEffect :> es) => String -> Eff es ()
putStr = send . PutStr

-- | @since 0.1
putStrLn :: (HasCallStack, TerminalEffect :> es) => String -> Eff es ()
putStrLn = send . PutStrLn

-- | @since 0.1
getChar :: (HasCallStack, TerminalEffect :> es) => Eff es Char
getChar = send GetChar

-- | @since 0.1
getLine :: (HasCallStack, TerminalEffect :> es) => Eff es String
getLine = send GetLine

#if MIN_VERSION_base(4,15,0)
-- | @since 0.1
getContents' :: (HasCallStack, TerminalEffect :> es) => Eff es String
getContents' = send GetContents'
#endif

-- | @since 0.1
getTerminalSize :: (HasCallStack, TerminalEffect :> es) => Eff es (Window Natural)
getTerminalSize = send GetTerminalSize

-- | 'Text' version of 'putStr'.
--
-- @since 0.1
putText :: (HasCallStack, TerminalEffect :> es) => Text -> Eff es ()
putText = putStr . T.unpack

-- | 'Text' version of 'putStrLn'.
--
-- @since 0.1
putTextLn :: (HasCallStack, TerminalEffect :> es) => Text -> Eff es ()
putTextLn = putStrLn . T.unpack

-- | @since 0.1
getTextLine :: (HasCallStack, TerminalEffect :> es) => Eff es Text
getTextLine = T.pack <$> getLine

#if MIN_VERSION_base(4,15,0)
-- | @since 0.1
getTextContents' :: (HasCallStack, TerminalEffect :> es) => Eff es Text
getTextContents' = T.pack <$> getContents'
#endif

-- | Retrieves the terminal width.
--
-- @since 0.1
getTerminalWidth :: (HasCallStack, TerminalEffect :> es) => Eff es Natural
getTerminalWidth = width <$> getTerminalSize

-- | Retrieves the terminal height.
--
-- @since 0.1
getTerminalHeight :: (HasCallStack, TerminalEffect :> es) => Eff es Natural
getTerminalHeight = height <$> getTerminalSize