-- | Basic thread effects.
--
-- @since 0.1
module Effectful.Concurrent.Thread
  ( -- * Threads

    -- ** Effect
    ThreadEffect (..),
    threadDelay,
    throwTo,
    getNumCapabilities,
    setNumCapabilities,
    threadCapability,

    -- ** Handlers
    runThreadIO,

    -- ** Functions
    microsleep,
    sleep,

    -- * QSem

    -- ** Effect
    QSemEffect (..),
    newQSem,
    waitQSem,
    signalQSem,

    -- ** Handlers
    runQSemIO,

    -- * QSemN

    -- ** Effect
    QSemNEffect (..),
    newQSemN,
    waitQSemN,
    signalQSemN,

    -- ** Handlers
    runQSemNIO,

    -- * Re-exports
    Natural,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent qualified as CC
import Control.Concurrent.QSem (QSem)
import Control.Concurrent.QSem qualified as QSem
import Control.Concurrent.QSemN (QSemN)
import Control.Concurrent.QSemN qualified as QSemN
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
data ThreadEffect :: Effect where
  ThreadDelay :: Int -> ThreadEffect m ()
  ThrowTo :: (Exception e) => ThreadId -> e -> ThreadEffect m ()
  GetNumCapabilities :: ThreadEffect m Int
  SetNumCapabilities :: Int -> ThreadEffect m ()
  ThreadCapability :: ThreadId -> ThreadEffect m (Int, Bool)

-- | @since 0.1
type instance DispatchOf ThreadEffect = Dynamic

-- | Runs 'ThreadEffect' in 'IO'.
--
-- @since 0.1
runThreadIO ::
  ( IOE :> es
  ) =>
  Eff (ThreadEffect : es) a ->
  Eff es a
runThreadIO = interpret $ \_ -> \case
  ThreadDelay n -> liftIO $ CC.threadDelay n
  ThrowTo tid e -> liftIO $ CC.throwTo tid e
  GetNumCapabilities -> liftIO CC.getNumCapabilities
  SetNumCapabilities i -> liftIO $ CC.setNumCapabilities i
  ThreadCapability tid -> liftIO $ CC.threadCapability tid

-- | @since 0.1
threadDelay :: (ThreadEffect :> es) => Int -> Eff es ()
threadDelay = send . ThreadDelay

-- | @since 0.1
throwTo :: (Exception e, ThreadEffect :> es) => ThreadId -> e -> Eff es ()
throwTo tid = send . ThrowTo tid

-- | @since 0.1
getNumCapabilities :: (ThreadEffect :> es) => Eff es Int
getNumCapabilities = send GetNumCapabilities

-- | @since 0.1
setNumCapabilities :: (ThreadEffect :> es) => Int -> Eff es ()
setNumCapabilities = send . SetNumCapabilities

-- | @since 0.1
threadCapability :: (ThreadEffect :> es) => ThreadId -> Eff es (Int, Bool)
threadCapability = send . ThreadCapability

-- | 'threadDelay' in terms of unbounded 'Natural' rather than 'Int' i.e.
-- runs sleep in the current thread for the specified number of microseconds.
--
-- @since 0.1
microsleep :: (ThreadEffect :> es) => Natural -> Eff es ()
microsleep n = for_ (natToInts n) threadDelay

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (ThreadEffect :> es) => Natural -> Eff es ()
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

-- | Effects for QSem.
--
-- @since 0.1
data QSemEffect :: Effect where
  NewQSem :: Int -> QSemEffect m QSem
  WaitQSem :: QSem -> QSemEffect m ()
  SignalQSem :: QSem -> QSemEffect m ()

-- | @since 0.1
type instance DispatchOf QSemEffect = Dynamic

-- | Runs 'ThreadEffect' in 'IO'.
--
-- @since 0.1
runQSemIO ::
  ( IOE :> es
  ) =>
  Eff (QSemEffect : es) a ->
  Eff es a
runQSemIO = interpret $ \_ -> \case
  NewQSem i -> liftIO $ QSem.newQSem i
  WaitQSem q -> liftIO $ QSem.waitQSem q
  SignalQSem q -> liftIO $ QSem.signalQSem q

-- | @since 0.1
newQSem :: (QSemEffect :> es) => Int -> Eff es QSem
newQSem = send . NewQSem

-- | @since 0.1
waitQSem :: (QSemEffect :> es) => QSem -> Eff es ()
waitQSem = send . WaitQSem

-- | @since 0.1
signalQSem :: (QSemEffect :> es) => QSem -> Eff es ()
signalQSem = send . SignalQSem

-- | Effects for QSemN.
--
-- @since 0.1
data QSemNEffect :: Effect where
  NewQSemN :: Int -> QSemNEffect m QSemN
  WaitQSemN :: QSemN -> Int -> QSemNEffect m ()
  SignalQSemN :: QSemN -> Int -> QSemNEffect m ()

-- | @since 0.1
type instance DispatchOf QSemNEffect = Dynamic

-- | Runs 'ThreadEffect' in 'IO'.
--
-- @since 0.1
runQSemNIO ::
  ( IOE :> es
  ) =>
  Eff (QSemNEffect : es) a ->
  Eff es a
runQSemNIO = interpret $ \_ -> \case
  NewQSemN i -> liftIO $ QSemN.newQSemN i
  WaitQSemN q i -> liftIO $ QSemN.waitQSemN q i
  SignalQSemN q i -> liftIO $ QSemN.signalQSemN q i

-- | @since 0.1
newQSemN :: (QSemNEffect :> es) => Int -> Eff es QSemN
newQSemN = send . NewQSemN

-- | @since 0.1
waitQSemN :: (QSemNEffect :> es) => QSemN -> Int -> Eff es ()
waitQSemN q = send . WaitQSemN q

-- | @since 0.1
signalQSemN :: (QSemNEffect :> es) => QSemN -> Int -> Eff es ()
signalQSemN q = send . SignalQSemN q
