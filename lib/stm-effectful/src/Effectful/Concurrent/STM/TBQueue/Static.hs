-- | 'TBQueue' helpers for static 'Concurrent' effect.
--
-- @since 0.1
module Effectful.Concurrent.STM.TBQueue.Static
  ( newTBQueueA,
    readTBQueueA,
    tryReadTBQueueA,
    writeTBQueueA,
    flushTBQueueA,

    -- * Re-exports
    Concurrent,
    runConcurrent,
    TBQueue,
    Natural,
  )
where

import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Effectful (Eff, type (:>))
import Effectful.Concurrent.STM (Concurrent, atomically, runConcurrent)
import Numeric.Natural (Natural)

-- | @since 0.1
newTBQueueA :: (Concurrent :> es) => Natural -> Eff es (TBQueue a)
newTBQueueA = atomically . TBQueue.newTBQueue

-- | @since 0.1
readTBQueueA :: (Concurrent :> es) => TBQueue a -> Eff es a
readTBQueueA = atomically . TBQueue.readTBQueue

-- | @since 0.1
tryReadTBQueueA :: (Concurrent :> es) => TBQueue a -> Eff es (Maybe a)
tryReadTBQueueA = atomically . TBQueue.tryReadTBQueue

-- | @since 0.1
writeTBQueueA :: (Concurrent :> es) => TBQueue a -> a -> Eff es ()
writeTBQueueA q = atomically . TBQueue.writeTBQueue q

-- | @since 0.1
flushTBQueueA :: (Concurrent :> es) => TBQueue a -> Eff es [a]
flushTBQueueA = atomically . TBQueue.flushTBQueue
