-- | Provides 'TermSizeException'.
--
-- @since 0.1
module Effectful.Terminal.TermSizeException
  ( TermSizeException (..),
  )
where

import Control.Exception (Exception (displayException))

-- | Exception when retrieving the terminal size.
--
-- @since 0.1
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
