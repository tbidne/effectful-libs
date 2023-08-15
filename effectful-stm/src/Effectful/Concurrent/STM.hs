-- | Effect for 'STM'.
--
-- @since 0.1
module Effectful.Concurrent.STM
  ( -- * STM

    -- ** Effect
    STMEffect (..),
    atomically,

    -- ** Handlers
    runSTMIO,

    -- * TVar
    newTVarA,
    readTVarA,
    writeTVarA,
    modifyTVarA',

    -- * TVar
    newTBQueueA,
    readTBQueueA,
    tryReadTBQueueA,
    writeTBQueueA,
    flushTBQueueA,

    -- * Re-exports
    STM,
    TVar,
    TBQueue,
    Natural,
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Monad.IO.Class (MonadIO (liftIO))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import Numeric.Natural (Natural)

-- | Effect for 'STM'.
--
-- @since 0.1
data STMEffect :: Effect where
  Atomically :: STM a -> STMEffect m a

-- | @since 0.1
type instance DispatchOf STMEffect = Dynamic

-- | Runs 'STMEffect' in 'IO'.
--
-- @since 0.1
runSTMIO ::
  ( IOE :> es
  ) =>
  Eff (STMEffect : es) a ->
  Eff es a
runSTMIO = interpret $ \_ -> \case
  Atomically x -> liftIO $ STM.atomically x

-- | Lifted 'STM.atomically'.
--
-- @since 0.1
atomically :: (STMEffect :> es) => STM a -> Eff es a
atomically = send . Atomically

-- | @since 0.1
newTVarA :: (STMEffect :> es) => a -> Eff es (TVar a)
newTVarA = atomically . TVar.newTVar

-- | @since 0.1
readTVarA :: (STMEffect :> es) => TVar a -> Eff es a
readTVarA = atomically . TVar.readTVar

-- | @since 0.1
writeTVarA :: (STMEffect :> es) => TVar a -> a -> Eff es ()
writeTVarA var = atomically . TVar.writeTVar var

-- | @since 0.1
modifyTVarA' :: (STMEffect :> es) => TVar a -> (a -> a) -> Eff es ()
modifyTVarA' var = atomically . TVar.modifyTVar' var

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
