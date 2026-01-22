-- | 'TBQueue' helpers for static 'Concurrent' effect.
--
-- @since 0.1
module Effectful.Concurrent.STM.TBQueue.Static
  ( -- * TBQueue

    -- ** Strict
    readTBQueue',
    tryReadTBQueue',
    writeTBQueue',
    flushTBQueue',

    -- *** Atomic
    readTBQueueA',
    tryReadTBQueueA',
    writeTBQueueA',
    flushTBQueueA',

    -- ** Lazy
    TBQueue.newTBQueue,
    TBQueue.readTBQueue,
    TBQueue.tryReadTBQueue,
    TBQueue.writeTBQueue,
    TBQueue.flushTBQueue,

    -- *** Atomic
    newTBQueueA,
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
import Control.Monad ((>=>))
import Effectful (Eff, type (:>))
import Effectful.Concurrent.STM (Concurrent, STM, atomically, runConcurrent)
import Effectful.Concurrent.STM.Utils (evaluateSTM)
import Numeric.Natural (Natural)

-- | @since 0.1
newTBQueueA :: (Concurrent :> es) => Natural -> Eff es (TBQueue a)
newTBQueueA = atomically . TBQueue.newTBQueue

-- | @since 0.1
readTBQueue' :: TBQueue a -> STM a
readTBQueue' = TBQueue.readTBQueue >=> evaluateSTM

-- | @since 0.1
readTBQueueA :: (Concurrent :> es) => TBQueue a -> Eff es a
readTBQueueA = atomically . TBQueue.readTBQueue

-- | @since 0.1
readTBQueueA' :: (Concurrent :> es) => TBQueue a -> Eff es a
readTBQueueA' = atomically . readTBQueue'

-- | @since 0.1
tryReadTBQueue' :: TBQueue a -> STM (Maybe a)
tryReadTBQueue' =
  TBQueue.tryReadTBQueue >=> \case
    Nothing -> pure Nothing
    Just x -> Just <$> evaluateSTM x

-- | @since 0.1
tryReadTBQueueA :: (Concurrent :> es) => TBQueue a -> Eff es (Maybe a)
tryReadTBQueueA = atomically . TBQueue.tryReadTBQueue

-- | @since 0.1
tryReadTBQueueA' :: (Concurrent :> es) => TBQueue a -> Eff es (Maybe a)
tryReadTBQueueA' = atomically . tryReadTBQueue'

-- | @since 0.1
writeTBQueue' :: TBQueue a -> a -> STM ()
writeTBQueue' q = evaluateSTM >=> TBQueue.writeTBQueue q

-- | @since 0.1
writeTBQueueA :: (Concurrent :> es) => TBQueue a -> a -> Eff es ()
writeTBQueueA q = atomically . TBQueue.writeTBQueue q

-- | @since 0.1
writeTBQueueA' :: (Concurrent :> es) => TBQueue a -> a -> Eff es ()
writeTBQueueA' q = atomically . writeTBQueue' q

-- | @since 0.1
flushTBQueue' :: TBQueue a -> STM [a]
flushTBQueue' = TBQueue.flushTBQueue >=> evaluateSTM

-- | @since 0.1
flushTBQueueA :: (Concurrent :> es) => TBQueue a -> Eff es [a]
flushTBQueueA = atomically . TBQueue.flushTBQueue

-- | @since 0.1
flushTBQueueA' :: (Concurrent :> es) => TBQueue a -> Eff es [a]
flushTBQueueA' = atomically . flushTBQueue'
