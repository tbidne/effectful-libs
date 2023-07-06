-- | Basic thread effects.
--
-- @since 0.1
module Effectful.Thread
  ( -- * Threads

    -- ** Effect
    ThreadEffect (..),

    -- *** Functions
    microsleep,

    -- *** Handlers
    runThreadIO,

    -- ** Misc
    sleep,

    -- * QSem

    -- ** Effect
    QSemEffect (..),

    -- *** Functions
    newQSem,
    waitQSem,
    signalQSem,
    newQSemN,
    waitQSemN,
    signalQSemN,

    -- *** Handlers
    runQSemIO,

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
  ( CallStackEffect,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)

-- | Effects for general threads.
--
-- @since 0.1
data ThreadEffect :: Effect where
  Microsleep :: (HasCallStack) => Natural -> ThreadEffect m ()

-- | @since 0.1
type instance DispatchOf ThreadEffect = Dynamic

-- | Runs 'ThreadEffect' in 'IO'.
--
-- @since 0.1
runThreadIO ::
  ( CallStackEffect :> es,
    IOE :> es
  ) =>
  Eff (ThreadEffect : es) a ->
  Eff es a
runThreadIO = interpret $ \_ -> \case
  Microsleep n -> addCallStack $ liftIO $ for_ (natToInts n) threadDelay

-- | @since 0.1
microsleep :: (HasCallStack, ThreadEffect :> es) => Natural -> Eff es ()
microsleep = send . Microsleep

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (HasCallStack, ThreadEffect :> es) => Natural -> Eff es ()
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
data QSemEffect :: Effect where
  NewQSem :: (HasCallStack) => Int -> QSemEffect m QSem
  WaitQSem :: (HasCallStack) => QSem -> QSemEffect m ()
  SignalQSem :: (HasCallStack) => QSem -> QSemEffect m ()
  NewQSemN :: (HasCallStack) => Int -> QSemEffect m QSemN
  WaitQSemN :: (HasCallStack) => QSemN -> Int -> QSemEffect m ()
  SignalQSemN :: (HasCallStack) => QSemN -> Int -> QSemEffect m ()

-- | @since 0.1
type instance DispatchOf QSemEffect = Dynamic

-- | Runs 'ThreadEffect' in 'IO'.
--
-- @since 0.1
runQSemIO ::
  ( CallStackEffect :> es,
    IOE :> es
  ) =>
  Eff (QSemEffect : es) a ->
  Eff es a
runQSemIO = interpret $ \_ -> \case
  NewQSem i -> addCallStack $ liftIO $ QSem.newQSem i
  WaitQSem q -> addCallStack $ liftIO $ QSem.waitQSem q
  SignalQSem q -> addCallStack $ liftIO $ QSem.signalQSem q
  NewQSemN i -> addCallStack $ liftIO $ QSemN.newQSemN i
  WaitQSemN q i -> addCallStack $ liftIO $ QSemN.waitQSemN q i
  SignalQSemN q i -> addCallStack $ liftIO $ QSemN.signalQSemN q i

-- | @since 0.1
newQSem :: (HasCallStack, QSemEffect :> es) => Int -> Eff es QSem
newQSem = send . NewQSem

-- | @since 0.1
waitQSem :: (HasCallStack, QSemEffect :> es) => QSem -> Eff es ()
waitQSem = send . WaitQSem

-- | @since 0.1
signalQSem :: (HasCallStack, QSemEffect :> es) => QSem -> Eff es ()
signalQSem = send . SignalQSem

-- | @since 0.1
newQSemN :: (HasCallStack, QSemEffect :> es) => Int -> Eff es QSemN
newQSemN = send . NewQSemN

-- | @since 0.1
waitQSemN :: (HasCallStack, QSemEffect :> es) => QSemN -> Int -> Eff es ()
waitQSemN q = send . WaitQSemN q

-- | @since 0.1
signalQSemN :: (HasCallStack, QSemEffect :> es) => QSemN -> Int -> Eff es ()
signalQSemN q = send . SignalQSemN q
