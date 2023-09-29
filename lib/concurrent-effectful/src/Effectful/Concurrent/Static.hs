{-# LANGUAGE UndecidableInstances #-}

-- | Static effect utils for "Control.Concurrent". For the effect itself, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-Concurrent.html.
--
-- @since 0.1
module Effectful.Concurrent.Static
  ( -- * Class
    MonadThread (..),

    -- * Effect
    Concurrent,

    -- * Functions
    microsleep,
    sleep,

    -- * Re-exports
    Natural,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent qualified as CC
import Control.Exception (Exception)
import Data.Foldable (for_)
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as EffCC
import GHC.Natural (Natural)

-- | Represents thread effects.
--
-- @since 0.1
class (Monad m) => MonadThread m where
  -- | Lifted 'CC.threadDelay'.
  --
  -- @since 0.1
  threadDelay :: Int -> m ()

  -- | Lifted 'CC.throwTo'.
  --
  -- @since 0.1
  throwTo :: (Exception e) => ThreadId -> e -> m ()

  -- | Lifted 'CC.getNumCapabilities'.
  --
  -- @since 0.1
  getNumCapabilities :: m Int

  -- | Lifted 'CC.setNumCapabilities'.
  --
  -- @since 0.1
  setNumCapabilities :: Int -> m ()

  -- | Lifted 'CC.threadCapability'.
  --
  -- @since 0.1
  threadCapability :: ThreadId -> m (Int, Bool)

-- | @since 0.1
instance MonadThread IO where
  threadDelay = CC.threadDelay
  throwTo = CC.throwTo
  getNumCapabilities = CC.getNumCapabilities
  setNumCapabilities = CC.setNumCapabilities
  threadCapability = CC.threadCapability

-- | @since 0.1
instance (Concurrent :> es) => MonadThread (Eff es) where
  threadDelay = EffCC.threadDelay
  throwTo = EffCC.throwTo
  getNumCapabilities = EffCC.getNumCapabilities
  setNumCapabilities = EffCC.setNumCapabilities
  threadCapability = EffCC.threadCapability

-- | 'threadDelay' in terms of unbounded 'Natural' rather than 'Int' i.e.
-- runs sleep in the current thread for the specified number of microseconds.
--
-- @since 0.1
microsleep :: (MonadThread m) => Natural -> m ()
microsleep n = for_ (natToInts n) threadDelay

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (MonadThread m) => Natural -> m ()
sleep = microsleep . (* 1_000_000)

natToInts :: Natural -> [Int]
natToInts n
  | n > maxIntAsNat = maxInt : natToInts (n - maxIntAsNat)
  | otherwise = [n2i n]
  where
    maxInt :: Int
    maxInt = maxBound
    maxIntAsNat :: Natural
    maxIntAsNat = i2n maxInt

n2i :: Natural -> Int
n2i = fromIntegral

i2n :: Int -> Natural
i2n = fromIntegral
