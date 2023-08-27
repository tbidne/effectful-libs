-- | Static effect utils for "Control.Concurrent". For the effect itself, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-Concurrent.html.
--
-- @since 0.1
module Effectful.Concurrent.Static
  ( -- * Effect
    Concurrent,

    -- * Functions
    microsleep,
    sleep,

    -- * Re-exports
    Natural,
  )
where

import Data.Foldable (for_)
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as CC
import GHC.Natural (Natural)

-- | 'threadDelay' in terms of unbounded 'Natural' rather than 'Int' i.e.
-- runs sleep in the current thread for the specified number of microseconds.
--
-- @since 0.1
microsleep :: (Concurrent :> es) => Natural -> Eff es ()
microsleep n = for_ (natToInts n) CC.threadDelay

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (Concurrent :> es) => Natural -> Eff es ()
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
