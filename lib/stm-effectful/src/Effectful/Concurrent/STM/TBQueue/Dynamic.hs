-- | 'TBQueue' helpers for dynamic 'STMDynamic' effect.
--
-- @since 0.1
module Effectful.Concurrent.STM.TBQueue.Dynamic
  ( newTBQueueA,
    readTBQueueA,
    tryReadTBQueueA,
    writeTBQueueA,
    flushTBQueueA,

    -- * Re-exports
    STMDynamic,
    runSTMDynamicIO,
    TBQueue,
    Natural,
  )
where

import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Effectful (Eff, type (:>))
import Effectful.Concurrent.STM.Dynamic (STMDynamic, atomically, runSTMDynamicIO)
import Numeric.Natural (Natural)

-- | 'TBQueue.newTBQueue' with 'atomically'.
--
-- @since 0.1
newTBQueueA :: (STMDynamic :> es) => Natural -> Eff es (TBQueue a)
newTBQueueA = atomically . TBQueue.newTBQueue

-- | 'TBQueue.readTBQueue' with 'atomically'.
--
-- @since 0.1
readTBQueueA :: (STMDynamic :> es) => TBQueue a -> Eff es a
readTBQueueA = atomically . TBQueue.readTBQueue

-- | 'TBQueue.tryReadTBQueue' with 'atomically'.
--
-- @since 0.1
tryReadTBQueueA :: (STMDynamic :> es) => TBQueue a -> Eff es (Maybe a)
tryReadTBQueueA = atomically . TBQueue.tryReadTBQueue

-- | 'TBQueue.writeTBQueue' with 'atomically'.
--
-- @since 0.1
writeTBQueueA :: (STMDynamic :> es) => TBQueue a -> a -> Eff es ()
writeTBQueueA q = atomically . TBQueue.writeTBQueue q

-- | 'TBQueue.flushTBQueue' with 'atomically'.
--
-- @since 0.1
flushTBQueueA :: (STMDynamic :> es) => TBQueue a -> Eff es [a]
flushTBQueueA = atomically . TBQueue.flushTBQueue
