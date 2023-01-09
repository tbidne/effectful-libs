-- | Effect for 'STM'.
--
-- @since 0.1
module Effectful.STM
  ( -- * STM

    -- ** Effect
    EffectSTM (..),

    -- ** Handler
    runSTMIO,

    -- ** Functions
    atomically,

    -- * TVar

    -- ** Effect
    EffectTVar (..),

    -- ** Handler
    runTVarIO,

    -- ** Functions
    newTVar,
    readTVar,
    writeTVar,
    modifyTVar',

    -- * TVar

    -- ** Effect
    EffectTBQueue (..),

    -- ** Handler
    runTBQueueIO,

    -- ** Functions
    newTBQueue,
    readTBQueue,
    tryReadTBQueue,
    writeTBQueue,
    flushTBQueue,

    -- * Reexports
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
import Effectful.CallStack
  ( EffectCallStack,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)

-- | Effect for 'STM'.
--
-- @since 0.1
data EffectSTM :: Effect where
  Atomically :: HasCallStack => STM a -> EffectSTM m a

-- | @since 0.1
type instance DispatchOf EffectSTM = Dynamic

-- | Runs 'EffectSTM' in 'IO'.
--
-- @since 0.1
runSTMIO ::
  ( EffectCallStack :> es,
    IOE :> es
  ) =>
  Eff (EffectSTM : es) a ->
  Eff es a
runSTMIO = interpret $ \_ -> \case
  Atomically x -> addCallStack $ liftIO $ STM.atomically x

-- | @since 0.1
atomically :: (EffectSTM :> es, HasCallStack) => STM a -> Eff es a
atomically stm = send (Atomically stm)

-- | Effect for 'TVar'.
--
-- @since 0.1
data EffectTVar :: Effect where
  NewTVar :: HasCallStack => a -> EffectTVar m (TVar a)
  ReadTVar :: HasCallStack => TVar a -> EffectTVar m a
  WriteTVar :: HasCallStack => TVar a -> a -> EffectTVar m ()
  ModifyTVar' :: HasCallStack => TVar a -> (a -> a) -> EffectTVar m ()

-- | @since 0.1
type instance DispatchOf EffectTVar = Dynamic

-- | Runs 'EffectTVar' in 'IO'.
--
-- @since 0.1
runTVarIO ::
  ( EffectCallStack :> es,
    IOE :> es
  ) =>
  Eff (EffectTVar : es) a ->
  Eff es a
runTVarIO = interpret $ \_ -> \case
  NewTVar x -> addCallStack $ liftIO $ STM.atomically $ TVar.newTVar x
  ReadTVar var -> addCallStack $ liftIO $ STM.atomically $ TVar.readTVar var
  WriteTVar var x -> addCallStack $ liftIO $ STM.atomically $ TVar.writeTVar var x
  ModifyTVar' var f -> addCallStack $ liftIO $ STM.atomically $ TVar.modifyTVar' var f

-- | @since 0.1
newTVar :: (EffectTVar :> es, HasCallStack) => a -> Eff es (TVar a)
newTVar x = send (NewTVar x)

-- | @since 0.1
readTVar :: (EffectTVar :> es, HasCallStack) => TVar a -> Eff es a
readTVar var = send (ReadTVar var)

-- | @since 0.1
writeTVar :: (EffectTVar :> es, HasCallStack) => TVar a -> a -> Eff es ()
writeTVar var x = send (WriteTVar var x)

-- | @since 0.1
modifyTVar' :: (EffectTVar :> es, HasCallStack) => TVar a -> (a -> a) -> Eff es ()
modifyTVar' var f = send (ModifyTVar' var f)

-- | Effect for 'TBQueue'.
--
-- @since 0.1
data EffectTBQueue :: Effect where
  NewTBQueue :: HasCallStack => Natural -> EffectTBQueue m (TBQueue a)
  ReadTBQueue :: HasCallStack => TBQueue a -> EffectTBQueue m a
  TryReadTBQueue :: HasCallStack => TBQueue a -> EffectTBQueue m (Maybe a)
  WriteTBQueue :: HasCallStack => TBQueue a -> a -> EffectTBQueue m ()
  FlushTBQueue :: HasCallStack => TBQueue a -> EffectTBQueue m [a]

-- | @since 0.1
type instance DispatchOf EffectTBQueue = Dynamic

-- | Runs 'EffectTBQueue' in 'IO'.
--
-- @since 0.1
runTBQueueIO ::
  ( EffectCallStack :> es,
    IOE :> es
  ) =>
  Eff (EffectTBQueue : es) a ->
  Eff es a
runTBQueueIO = interpret $ \_ -> \case
  NewTBQueue n -> addCallStack $ liftIO $ STM.atomically $ TBQueue.newTBQueue n
  ReadTBQueue q -> addCallStack $ liftIO $ STM.atomically $ TBQueue.readTBQueue q
  TryReadTBQueue q -> addCallStack $ liftIO $ STM.atomically $ TBQueue.tryReadTBQueue q
  WriteTBQueue q x -> addCallStack $ liftIO $ STM.atomically $ TBQueue.writeTBQueue q x
  FlushTBQueue q -> addCallStack $ liftIO $ STM.atomically $ TBQueue.flushTBQueue q

-- | @since 0.1
newTBQueue :: (EffectTBQueue :> es, HasCallStack) => Natural -> Eff es (TBQueue a)
newTBQueue x = send (NewTBQueue x)

-- | @since 0.1
readTBQueue :: (EffectTBQueue :> es, HasCallStack) => TBQueue a -> Eff es a
readTBQueue q = send (ReadTBQueue q)

-- | @since 0.1
tryReadTBQueue :: (EffectTBQueue :> es, HasCallStack) => TBQueue a -> Eff es (Maybe a)
tryReadTBQueue q = send (TryReadTBQueue q)

-- | @since 0.1
writeTBQueue :: (EffectTBQueue :> es, HasCallStack) => TBQueue a -> a -> Eff es ()
writeTBQueue q x = send (WriteTBQueue q x)

-- | @since 0.1
flushTBQueue :: (EffectTBQueue :> es, HasCallStack) => TBQueue a -> Eff es [a]
flushTBQueue q = send (FlushTBQueue q)
