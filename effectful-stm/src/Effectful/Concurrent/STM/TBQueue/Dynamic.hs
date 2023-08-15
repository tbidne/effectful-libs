-- | 'TBQueue' helpers for 'STMEffect'.
--
-- @since 0.1
module Effectful.Concurrent.STM.TBQueue.Dynamic
  ( newTBQueueA,
    readTBQueueA,
    tryReadTBQueueA,
    writeTBQueueA,
    flushTBQueueA,

    -- * Re-exports
    STMEffect,
    runSTMIO,
    TBQueue,
    Natural,
  )
where

import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Effectful (Eff, type (:>))
import Effectful.Concurrent.STM.Dynamic (STMEffect, atomically, runSTMIO)
import Numeric.Natural (Natural)

-- | @since 0.1
newTBQueueA :: (STMEffect :> es) => Natural -> Eff es (TBQueue a)
newTBQueueA = atomically . TBQueue.newTBQueue

-- | @since 0.1
readTBQueueA :: (STMEffect :> es) => TBQueue a -> Eff es a
readTBQueueA = atomically . TBQueue.readTBQueue

-- | @since 0.1
tryReadTBQueueA :: (STMEffect :> es) => TBQueue a -> Eff es (Maybe a)
tryReadTBQueueA = atomically . TBQueue.tryReadTBQueue

-- | @since 0.1
writeTBQueueA :: (STMEffect :> es) => TBQueue a -> a -> Eff es ()
writeTBQueueA q = atomically . TBQueue.writeTBQueue q

-- | @since 0.1
flushTBQueueA :: (STMEffect :> es) => TBQueue a -> Eff es [a]
flushTBQueueA = atomically . TBQueue.flushTBQueue
