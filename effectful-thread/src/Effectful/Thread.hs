-- | Basic thread effects.
--
-- @since 0.1
module Effectful.Thread
  ( -- * Threads

    -- ** Effect
    EffectThread (..),

    -- ** Handler
    runThreadIO,

    -- ** Functions
    microsleep,
    sleep,

    -- * QSem

    -- ** Effect
    EffectQSem (..),

    -- ** Handler
    runQSemIO,

    -- ** Functions
    newQSem,
    waitQSem,
    signalQSem,
    newQSemN,
    waitQSemN,
    signalQSemN,

    -- * Reexports
    Natural,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.QSem (QSem)
import Control.Concurrent.QSem qualified as QSem
import Control.Concurrent.QSemN (QSemN)
import Control.Concurrent.QSemN qualified as QSemN
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
import Effectful.CallStack
  ( EffectCallStack,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)

-- | Effects for general threads.
--
-- @since 0.1
data EffectThread :: Effect where
  Microsleep :: HasCallStack => Natural -> EffectThread m ()

-- | @since 0.1
type instance DispatchOf EffectThread = Dynamic

-- | Runs 'EffectThread' in 'IO'.
--
-- @since 0.1
runThreadIO ::
  ( EffectCallStack :> es,
    IOE :> es
  ) =>
  Eff (EffectThread : es) a ->
  Eff es a
runThreadIO = interpret $ \_ -> \case
  Microsleep n -> addCallStack $ liftIO $ for_ (natToInts n) threadDelay

-- | @since 0.1
microsleep :: (EffectThread :> es, HasCallStack) => Natural -> Eff es ()
microsleep = send . Microsleep

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (EffectThread :> es, HasCallStack) => Natural -> Eff es ()
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

-- | Effects for semaphores.
--
-- @since 0.1
data EffectQSem :: Effect where
  NewQSem :: HasCallStack => Int -> EffectQSem m QSem
  WaitQSem :: HasCallStack => QSem -> EffectQSem m ()
  SignalQSem :: HasCallStack => QSem -> EffectQSem m ()
  NewQSemN :: HasCallStack => Int -> EffectQSem m QSemN
  WaitQSemN :: HasCallStack => QSemN -> Int -> EffectQSem m ()
  SignalQSemN :: HasCallStack => QSemN -> Int -> EffectQSem m ()

-- | @since 0.1
type instance DispatchOf EffectQSem = Dynamic

-- | Runs 'EffectThread' in 'IO'.
--
-- @since 0.1
runQSemIO ::
  ( EffectCallStack :> es,
    IOE :> es
  ) =>
  Eff (EffectQSem : es) a ->
  Eff es a
runQSemIO = interpret $ \_ -> \case
  NewQSem i -> addCallStack $ liftIO $ QSem.newQSem i
  WaitQSem q -> addCallStack $ liftIO $ QSem.waitQSem q
  SignalQSem q -> addCallStack $ liftIO $ QSem.signalQSem q
  NewQSemN i -> addCallStack $ liftIO $ QSemN.newQSemN i
  WaitQSemN q i -> addCallStack $ liftIO $ QSemN.waitQSemN q i
  SignalQSemN q i -> addCallStack $ liftIO $ QSemN.signalQSemN q i

-- | @since 0.1
newQSem :: (EffectQSem :> es, HasCallStack) => Int -> Eff es QSem
newQSem = send . NewQSem

-- | @since 0.1
waitQSem :: (EffectQSem :> es, HasCallStack) => QSem -> Eff es ()
waitQSem = send . WaitQSem

-- | @since 0.1
signalQSem :: (EffectQSem :> es, HasCallStack) => QSem -> Eff es ()
signalQSem = send . SignalQSem

-- | @since 0.1
newQSemN :: (EffectQSem :> es, HasCallStack) => Int -> Eff es QSemN
newQSemN = send . NewQSemN

-- | @since 0.1
waitQSemN :: (EffectQSem :> es, HasCallStack) => QSemN -> Int -> Eff es ()
waitQSemN q = send . WaitQSemN q

-- | @since 0.1
signalQSemN :: (EffectQSem :> es, HasCallStack) => QSemN -> Int -> Eff es ()
signalQSemN q = send . SignalQSemN q
