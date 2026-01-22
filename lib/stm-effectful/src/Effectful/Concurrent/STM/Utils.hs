{-# LANGUAGE MagicHash #-}

module Effectful.Concurrent.STM.Utils
  ( evaluateSTM,
  )
where

import GHC.Conc (STM (STM))
import GHC.Exts (seq#)

-- | Like 'Control.Exception.evaluate', but for 'STM'.
--
-- @since 0.1
evaluateSTM :: a -> STM a
evaluateSTM a = STM $ \s -> seq# a s
