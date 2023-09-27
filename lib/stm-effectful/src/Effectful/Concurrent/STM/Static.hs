{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'MonadSTM' effect along with utils.
--
-- @since 0.1
module Effectful.Concurrent.STM.Static
  ( -- * Class
    MonadSTM (..),

    -- * TBQueue
    TBQueue,
    newTBQueueA,
    readTBQueueA,
    writeTBQueueA,
    flushTBQueueA,

    -- * TVar
    TVar,
    newTVarA,
    readTVarA,
    tryReadTBQueueA,
    writeTVarA,
    modifyTVarA',

    -- * Re-exports
    Concurrent,
    runConcurrent,
    Natural,
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar qualified as TVar
import Effectful (Eff, type (:>))
import Effectful.Concurrent.STM (Concurrent, runConcurrent)
import Effectful.Concurrent.STM qualified as EffSTM
import Numeric.Natural (Natural)

-- | 'STM' effect.
--
-- @since 0.1
class (Monad m) => MonadSTM m where
  -- | Lifted 'CC.atomically'.
  --
  -- @since 0.1
  atomically :: STM a -> m a

-- | @since 0.1
instance MonadSTM IO where
  atomically = STM.atomically

-- | @since 0.1
instance (Concurrent :> es) => MonadSTM (Eff es) where
  atomically = EffSTM.atomically

-- | @since 0.1
newTBQueueA :: (MonadSTM m) => Natural -> m (TBQueue a)
newTBQueueA = atomically . TBQueue.newTBQueue

-- | @since 0.1
readTBQueueA :: (MonadSTM m) => TBQueue a -> m a
readTBQueueA = atomically . TBQueue.readTBQueue

-- | @since 0.1
tryReadTBQueueA :: (MonadSTM m) => TBQueue a -> m (Maybe a)
tryReadTBQueueA = atomically . TBQueue.tryReadTBQueue

-- | @since 0.1
writeTBQueueA :: (MonadSTM m) => TBQueue a -> a -> m ()
writeTBQueueA q = atomically . TBQueue.writeTBQueue q

-- | @since 0.1
flushTBQueueA :: (MonadSTM m) => TBQueue a -> m [a]
flushTBQueueA = atomically . TBQueue.flushTBQueue

-- | @since 0.1
newTVarA :: (MonadSTM m) => a -> m (TVar a)
newTVarA = atomically . TVar.newTVar

-- | @since 0.1
readTVarA :: (MonadSTM m) => TVar a -> m a
readTVarA = atomically . TVar.readTVar

-- | @since 0.1
writeTVarA :: (MonadSTM m) => TVar a -> a -> m ()
writeTVarA var = atomically . TVar.writeTVar var

-- | @since 0.1
modifyTVarA' :: (MonadSTM m) => TVar a -> (a -> a) -> m ()
modifyTVarA' var = atomically . TVar.modifyTVar' var
