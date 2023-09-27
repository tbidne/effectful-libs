{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- ORMOLU_DISABLE -}

-- | Provides a dynamic terminal effect.
--
-- @since 0.1
module Effectful.Terminal.Dynamic
  ( -- * Class
    MonadTerminal (..),

    -- * Effect
    TerminalDynamic (..),
    TermSizeException (..),

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
import Effectful.Exception (throwM)
import Effectful.Terminal.TermSizeException
  ( TermSizeException (MkTermSizeException),
  )
import System.Console.Terminal.Size (Window (Window, height, width), size)
import System.IO qualified as IO
import Prelude
  ( Applicative (pure),
    Char,
    IO,
    Integral,
    Maybe (Just, Nothing),
    Monad ((>>=)),
    Show (show),
    String,
    ($),
    (++),
    (.),
    (<$>),
  )

{- ORMOLU_DISABLE -}

-- | Represents a terminal.
--
-- @since 0.1
class Monad m => MonadTerminal m where
  -- | Lifted 'IO.putStr'.
--
-- @since 0.1
  putStr :: String -> m ()

  -- | Lifted 'IO.putStrLn'.
--
-- @since 0.1
  putStrLn :: String -> m ()
  putStrLn = putStr . (++ "\n")

  -- | Lifted 'BS.putStr'.
--
-- @since 0.1
  putBinary :: ByteString -> m ()

  -- | Lifted 'IO.getChar'.
--
-- @since 0.1
  getChar :: m Char

  -- | Lifted 'IO.getLine'.
--
-- @since 0.1
  getLine :: m String

#if MIN_VERSION_base(4,15,0)
  -- | Lifted 'IO.getContents'.
--
-- @since 0.1
  getContents' :: m String
#endif

  -- | Lifted 'size'.
--
-- @since 0.1
  getTerminalSize :: Integral a => m (Window a)

#if MIN_VERSION_base(4,15,0)
  {-# MINIMAL putStr, putBinary, getChar, getLine, getContents', getTerminalSize #-}
#else
  {-# MINIMAL putStr , putBinary, getChar, getLine, getTerminalSize #-}
#endif

-- | @since 0.1
instance MonadTerminal IO where
  putStr = IO.putStr
  putStrLn = IO.putStrLn
  putBinary = BS.putStr
  getChar = IO.getChar
  getLine = IO.getLine
#if MIN_VERSION_base(4,15,0)
  getContents' = IO.getContents'
#endif
  getTerminalSize =
    size >>= \case
      Just h -> pure h
      Nothing -> throwM MkTermSizeException

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
      Nothing -> throwM MkTermSizeException

-- | @since 0.1
instance (TerminalDynamic :> es) => MonadTerminal (Eff es) where
  putStr = send . PutStr
  putStrLn = send . PutStrLn
  putBinary = send . PutBinary
  getChar = send GetChar
  getLine = send GetLine

#if MIN_VERSION_base(4,15,0)
  getContents' = send GetContents'
#endif

  getTerminalSize = send GetTerminalSize

{- ORMOLU_ENABLE -}

-- | @since 0.1
print :: (MonadTerminal m, Show a) => a -> m ()
print = putStrLn . show

-- | 'Text' version of 'putStr'.
--
-- @since 0.1
putText :: (MonadTerminal m) => Text -> m ()
putText = putStr . T.unpack

-- | 'Text' version of 'putStrLn'.
--
-- @since 0.1
putTextLn :: (MonadTerminal m) => Text -> m ()
putTextLn = putStrLn . T.unpack

-- | @since 0.1
getTextLine :: (MonadTerminal m) => m Text
getTextLine = T.pack <$> getLine

#if MIN_VERSION_base(4,15,0)

-- | @since 0.1
getTextContents' :: (MonadTerminal m) => m Text
getTextContents' = T.pack <$> getContents'

#endif

-- | Retrieves the terminal width.
--
-- @since 0.1
getTerminalWidth :: (Integral a, MonadTerminal m) => m a
getTerminalWidth = width <$> getTerminalSize

-- | Retrieves the terminal height.
--
-- @since 0.1
getTerminalHeight :: (Integral a, MonadTerminal m) => m a
getTerminalHeight = height <$> getTerminalSize
