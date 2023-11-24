{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- ORMOLU_DISABLE -}

-- | Provides a static terminal effect.
--
-- @since 0.1
module Effectful.Terminal.Static
  ( -- * Effect
    TerminalStatic,
    TermSizeException (..),
    putStr,
    putStrLn,
    putBinary,
    getChar,
    getLine,
#if MIN_VERSION_base(4,15,0)
    getContents',
#endif
    getTerminalSize,

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

import Control.Monad.IO.Class (MonadIO (liftIO))
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
    Integral,
    Maybe (Just, Nothing),
    Monad ((>>=)),
    Show (show),
    String,
    ($),
    (.),
    (<$>),
  )

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

-- | Lifted 'IO.putStr'.
--
-- @since 0.1
putStr :: (TerminalStatic :> es) => String -> Eff es ()
putStr = unsafeEff_ . IO.putStr

-- | Lifted 'IO.putStrLn'.
--
-- @since 0.1
putStrLn :: (TerminalStatic :> es) => String -> Eff es ()
putStrLn = unsafeEff_ . IO.putStrLn

-- | Lifted 'BS.putStr'.
--
-- @since 0.1
putBinary :: (TerminalStatic :> es) => ByteString -> Eff es ()
putBinary = unsafeEff_ . BS.putStr

-- | Lifted 'IO.getChar'.
--
-- @since 0.1
getChar :: (TerminalStatic :> es) => Eff es Char
getChar = unsafeEff_ IO.getChar

-- | Lifted 'IO.getLine'.
--
-- @since 0.1
getLine :: (TerminalStatic :> es) => Eff es String
getLine = unsafeEff_ IO.getLine

#if MIN_VERSION_base(4,15,0)

-- | Lifted 'IO.getContents''.
--
-- @since 0.1
getContents' :: (TerminalStatic :> es) => Eff es String
getContents' = unsafeEff_ IO.getContents'

#endif

-- | Retrieves the terminal size.
--
-- @since 0.1
getTerminalSize :: (Integral a, TerminalStatic :> es) => Eff es (Window a)
getTerminalSize =
  unsafeEff_ $
    liftIO size >>= \case
      Just h -> pure h
      Nothing -> throwM MkTermSizeException

-- | @since 0.1
print :: (Show a, TerminalStatic :> es) => a -> Eff es ()
print = putStrLn . show

-- | 'Text' version of 'putStr'.
--
-- @since 0.1
putText :: (TerminalStatic :> es) => Text -> Eff es ()
putText = putStr . T.unpack

-- | 'Text' version of 'putStrLn'.
--
-- @since 0.1
putTextLn :: (TerminalStatic :> es) => Text -> Eff es ()
putTextLn = putStrLn . T.unpack

-- | @since 0.1
getTextLine :: (TerminalStatic :> es) => Eff es Text
getTextLine = T.pack <$> getLine

#if MIN_VERSION_base(4,15,0)

-- | @since 0.1
getTextContents' :: (TerminalStatic :> es) => Eff es Text
getTextContents' = T.pack <$> getContents'

#endif

-- | Retrieves the terminal width.
--
-- @since 0.1
getTerminalWidth :: (Integral a, TerminalStatic :> es) => Eff es a
getTerminalWidth = width <$> getTerminalSize

-- | Retrieves the terminal height.
--
-- @since 0.1
getTerminalHeight :: (Integral a, TerminalStatic :> es) => Eff es a
getTerminalHeight = height <$> getTerminalSize
