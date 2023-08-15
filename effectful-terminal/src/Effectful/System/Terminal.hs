{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Provides a Terminal effect.
--
-- @since 0.1
module Effectful.System.Terminal
  ( -- * Effect
    TerminalEffect (..),
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
    runTerminalIO,

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
data TerminalEffect :: Effect where
  PutStr :: String -> TerminalEffect m ()
  PutStrLn :: String -> TerminalEffect m ()
  GetChar :: TerminalEffect m Char
  GetLine :: TerminalEffect m String
#if MIN_VERSION_base(4,15,0)
  GetContents' :: TerminalEffect m String
#endif
  GetTerminalSize :: TerminalEffect m (Window Natural)

{- ORMOLU_ENABLE -}

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

{- ORMOLU_DISABLE -}

-- | Runs 'TerminalEffect' in 'IO'.
--
-- @since 0.1
runTerminalIO ::
  ( 
    IOE :> es
  ) =>
  Eff (TerminalEffect : es) a ->
  Eff es a
runTerminalIO = interpret $ \_ -> \case
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
putStr :: (TerminalEffect :> es) => String -> Eff es ()
putStr = send . PutStr

-- | Lifted 'IO.putStrLn'.
--
-- @since 0.1
putStrLn :: (TerminalEffect :> es) => String -> Eff es ()
putStrLn = send . PutStrLn

-- | Lifted 'IO.getChar'.
--
-- @since 0.1
getChar :: (TerminalEffect :> es) => Eff es Char
getChar = send GetChar

-- | Lifted 'IO.getLine'.
--
-- @since 0.1
getLine :: (TerminalEffect :> es) => Eff es String
getLine = send GetLine

#if MIN_VERSION_base(4,15,0)

-- | Lifted 'IO.getContents''.
--
-- @since 0.1
getContents' :: ( TerminalEffect :> es) => Eff es String
getContents' = send GetContents'

#endif

-- | @since 0.1
getTerminalSize :: (TerminalEffect :> es) => Eff es (Window Natural)
getTerminalSize = send GetTerminalSize

-- | @since 0.1
print :: (Show a, TerminalEffect :> es) => a -> Eff es ()
print = putStrLn . show

-- | 'Text' version of 'putStr'.
--
-- @since 0.1
putText :: (TerminalEffect :> es) => Text -> Eff es ()
putText = putStr . T.unpack

-- | 'Text' version of 'putStrLn'.
--
-- @since 0.1
putTextLn :: (TerminalEffect :> es) => Text -> Eff es ()
putTextLn = putStrLn . T.unpack

-- | @since 0.1
getTextLine :: (TerminalEffect :> es) => Eff es Text
getTextLine = T.pack <$> getLine

#if MIN_VERSION_base(4,15,0)

-- | @since 0.1
getTextContents' :: ( TerminalEffect :> es) => Eff es Text
getTextContents' = T.pack <$> getContents'

#endif

-- | Retrieves the terminal width.
--
-- @since 0.1
getTerminalWidth :: (TerminalEffect :> es) => Eff es Natural
getTerminalWidth = width <$> getTerminalSize

-- | Retrieves the terminal height.
--
-- @since 0.1
getTerminalHeight :: (TerminalEffect :> es) => Eff es Natural
getTerminalHeight = height <$> getTerminalSize
