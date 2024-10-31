{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- ORMOLU_DISABLE -}

-- | Static effect utils for "Control.Concurrent". For the effect itself, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-Concurrent.html.
--
-- @since 0.1
module Effectful.Concurrent.Static
  ( -- * Effect
    Concurrent,
    labelThread,

#if MIN_VERSION_base(4, 18, 0)
    threadLabel,
#endif

    -- * Functions
    microsleep,
    sleep,

    -- * Re-exports
    Natural,
  )
where

{- ORMOLU_ENABLE -}

import Control.Concurrent (ThreadId)
import Data.Foldable (for_)
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as EffCC
import Effectful.Dispatch.Static (HasCallStack, unsafeEff_)
import GHC.Conc.Sync qualified as Sync
import GHC.Natural (Natural)

-- | Lifted 'Sync.labelThread'.
--
-- @since 0.1
labelThread ::
  (Concurrent :> es, HasCallStack) =>
  ThreadId ->
  String ->
  Eff es ()
labelThread tid = unsafeEff_ . Sync.labelThread tid

#if MIN_VERSION_base(4, 18, 0)

-- | Lifted 'Sync.threadLabel'.
--
-- @since 0.1
threadLabel ::
  (Concurrent :> es, HasCallStack) =>
  ThreadId -> Eff es (Maybe String)
threadLabel = unsafeEff_ . Sync.threadLabel

#endif

-- | 'threadDelay' in terms of unbounded 'Natural' rather than 'Int' i.e.
-- runs sleep in the current thread for the specified number of microseconds.
--
-- @since 0.1
microsleep :: (Concurrent :> es, HasCallStack) => Natural -> Eff es ()
microsleep n = for_ (natToInts n) EffCC.threadDelay

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (Concurrent :> es, HasCallStack) => Natural -> Eff es ()
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
