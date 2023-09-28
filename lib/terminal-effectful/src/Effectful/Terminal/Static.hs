{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- ORMOLU_DISABLE -}

-- | Provides a static terminal effect.
--
-- @since 0.1
module Effectful.Terminal.Static
  ( -- * Class
    MonadTerminal (..),

    -- * Effect
    TerminalStatic,
    TermSizeException (..),

    -- ** Handlers
    runTerminalStaticIO,

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
  ( SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    unsafeEff_,
  )
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

-- | Static terminal effect.
--
-- @since 0.1
data TerminalStatic :: Effect

type instance DispatchOf TerminalStatic = Static WithSideEffects

data instance StaticRep TerminalStatic = MkTerminalStatic

-- | Runs an OptparseStatic effect.
--
-- @since 0.1
runTerminalStaticIO :: (IOE :> es) => Eff (TerminalStatic : es) a -> Eff es a
runTerminalStaticIO = evalStaticRep MkTerminalStatic

-- | @since 0.1
instance (TerminalStatic :> es) => MonadTerminal (Eff es) where
  putStr = unsafeEff_ . IO.putStr
  putStrLn = unsafeEff_ . IO.putStrLn
  putBinary = unsafeEff_ . BS.putStr
  getChar = unsafeEff_ IO.getChar
  getLine = unsafeEff_ IO.getLine

#if MIN_VERSION_base(4,15,0)
  getContents' = unsafeEff_ IO.getContents'
#endif

  getTerminalSize =
    unsafeEff_ size >>= \case
      Just h -> pure h
      Nothing -> throwM MkTermSizeException

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
