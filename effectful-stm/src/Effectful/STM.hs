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
    newTVarE,
    readTVarE,
    writeTVarE,
    modifyTVarE',

    -- * TVar

    -- ** Effect
    EffectTBQueue (..),

    -- ** Handler
    runTBQueueIO,

    -- ** Functions
    newTBQueueE,
    readTBQueueE,
    tryReadTBQueueE,
    writeTBQueueE,
    flushTBQueueE,

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
atomically = send . Atomically

-- | Effect for 'TVar'.
--
-- @since 0.1
data EffectTVar :: Effect where
  NewTVarE :: HasCallStack => a -> EffectTVar m (TVar a)
  ReadTVarE :: HasCallStack => TVar a -> EffectTVar m a
  WriteTVarE :: HasCallStack => TVar a -> a -> EffectTVar m ()
  ModifyTVarE' :: HasCallStack => TVar a -> (a -> a) -> EffectTVar m ()

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
  NewTVarE x -> addCallStack $ liftIO $ STM.atomically $ TVar.newTVar x
  ReadTVarE var -> addCallStack $ liftIO $ STM.atomically $ TVar.readTVar var
  WriteTVarE var x -> addCallStack $ liftIO $ STM.atomically $ TVar.writeTVar var x
  ModifyTVarE' var f -> addCallStack $ liftIO $ STM.atomically $ TVar.modifyTVar' var f

-- | @since 0.1
newTVarE :: (EffectTVar :> es, HasCallStack) => a -> Eff es (TVar a)
newTVarE = send . NewTVarE

-- | @since 0.1
readTVarE :: (EffectTVar :> es, HasCallStack) => TVar a -> Eff es a
readTVarE = send . ReadTVarE

-- | @since 0.1
writeTVarE :: (EffectTVar :> es, HasCallStack) => TVar a -> a -> Eff es ()
writeTVarE var = send . WriteTVarE var

-- | @since 0.1
modifyTVarE' :: (EffectTVar :> es, HasCallStack) => TVar a -> (a -> a) -> Eff es ()
modifyTVarE' var = send . ModifyTVarE' var

-- | Effect for 'TBQueue'.
--
-- @since 0.1
data EffectTBQueue :: Effect where
  NewTBQueueE :: HasCallStack => Natural -> EffectTBQueue m (TBQueue a)
  ReadTBQueueE :: HasCallStack => TBQueue a -> EffectTBQueue m a
  TryReadTBQueueE :: HasCallStack => TBQueue a -> EffectTBQueue m (Maybe a)
  WriteTBQueueE :: HasCallStack => TBQueue a -> a -> EffectTBQueue m ()
  FlushTBQueueE :: HasCallStack => TBQueue a -> EffectTBQueue m [a]

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
  NewTBQueueE n -> addCallStack $ liftIO $ STM.atomically $ TBQueue.newTBQueue n
  ReadTBQueueE q -> addCallStack $ liftIO $ STM.atomically $ TBQueue.readTBQueue q
  TryReadTBQueueE q -> addCallStack $ liftIO $ STM.atomically $ TBQueue.tryReadTBQueue q
  WriteTBQueueE q x -> addCallStack $ liftIO $ STM.atomically $ TBQueue.writeTBQueue q x
  FlushTBQueueE q -> addCallStack $ liftIO $ STM.atomically $ TBQueue.flushTBQueue q

-- | @since 0.1
newTBQueueE :: (EffectTBQueue :> es, HasCallStack) => Natural -> Eff es (TBQueue a)
newTBQueueE = send . NewTBQueueE

-- | @since 0.1
readTBQueueE :: (EffectTBQueue :> es, HasCallStack) => TBQueue a -> Eff es a
readTBQueueE = send . ReadTBQueueE

-- | @since 0.1
tryReadTBQueueE :: (EffectTBQueue :> es, HasCallStack) => TBQueue a -> Eff es (Maybe a)
tryReadTBQueueE = send . TryReadTBQueueE

-- | @since 0.1
writeTBQueueE :: (EffectTBQueue :> es, HasCallStack) => TBQueue a -> a -> Eff es ()
writeTBQueueE q = send . WriteTBQueueE q

-- | @since 0.1
flushTBQueueE :: (EffectTBQueue :> es, HasCallStack) => TBQueue a -> Eff es [a]
flushTBQueueE = send . FlushTBQueueE
