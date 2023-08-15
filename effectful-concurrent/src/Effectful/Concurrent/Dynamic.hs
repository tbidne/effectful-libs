-- | Basic thread effects.
--
-- @since 0.1
module Effectful.Concurrent.Dynamic
  ( -- * Effect
    ConcurrentEffect (..),
    threadDelay,
    throwTo,
    getNumCapabilities,
    setNumCapabilities,
    threadCapability,

    -- ** Handlers
    runThreadIO,

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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Natural (Natural)

-- | Effects for general threads.
--
-- @since 0.1
data ConcurrentEffect :: Effect where
  ThreadDelay :: Int -> ConcurrentEffect m ()
  ThrowTo :: (Exception e) => ThreadId -> e -> ConcurrentEffect m ()
  GetNumCapabilities :: ConcurrentEffect m Int
  SetNumCapabilities :: Int -> ConcurrentEffect m ()
  ThreadCapability :: ThreadId -> ConcurrentEffect m (Int, Bool)

-- | @since 0.1
type instance DispatchOf ConcurrentEffect = Dynamic

-- | Runs 'ConcurrentEffect' in 'IO'.
--
-- @since 0.1
runThreadIO ::
  ( IOE :> es
  ) =>
  Eff (ConcurrentEffect : es) a ->
  Eff es a
runThreadIO = interpret $ \_ -> \case
  ThreadDelay n -> liftIO $ CC.threadDelay n
  ThrowTo tid e -> liftIO $ CC.throwTo tid e
  GetNumCapabilities -> liftIO CC.getNumCapabilities
  SetNumCapabilities i -> liftIO $ CC.setNumCapabilities i
  ThreadCapability tid -> liftIO $ CC.threadCapability tid

-- | Lifted 'CC.threadDelay'.
--
-- @since 0.1
threadDelay :: (ConcurrentEffect :> es) => Int -> Eff es ()
threadDelay = send . ThreadDelay

-- | Lifted 'CC.throwTo'.
--
-- @since 0.1
throwTo :: (Exception e, ConcurrentEffect :> es) => ThreadId -> e -> Eff es ()
throwTo tid = send . ThrowTo tid

-- | Lifted 'CC.getNumCapabilities'.
--
-- @since 0.1
getNumCapabilities :: (ConcurrentEffect :> es) => Eff es Int
getNumCapabilities = send GetNumCapabilities

-- | Lifted 'CC.setNumCapabilities'.
--
-- @since 0.1
setNumCapabilities :: (ConcurrentEffect :> es) => Int -> Eff es ()
setNumCapabilities = send . SetNumCapabilities

-- | Lifted 'CC.threadCapability'.
--
-- @since 0.1
threadCapability :: (ConcurrentEffect :> es) => ThreadId -> Eff es (Int, Bool)
threadCapability = send . ThreadCapability

-- | 'threadDelay' in terms of unbounded 'Natural' rather than 'Int' i.e.
-- runs sleep in the current thread for the specified number of microseconds.
--
-- @since 0.1
microsleep :: (ConcurrentEffect :> es) => Natural -> Eff es ()
microsleep n = for_ (natToInts n) threadDelay

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (ConcurrentEffect :> es) => Natural -> Eff es ()
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
