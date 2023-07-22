-- | Effect for 'STM'.
--
-- @since 0.1
module Effectful.STM
  ( -- * STM

    -- ** Effect
    STMEffect (..),

    -- *** Functions
    atomically,

    -- *** Handlers
    runSTMIO,

    -- * TVar

    -- ** Effect
    TVarEffect (..),

    -- *** Functions
    newTVarE,
    readTVarE,
    writeTVarE,
    modifyTVarE',

    -- ** Handler
    runTVarIO,

    -- * TVar

    -- ** Effect
    TBQueueEffect (..),

    -- *** Functions
    newTBQueueE,
    readTBQueueE,
    tryReadTBQueueE,
    writeTBQueueE,
    flushTBQueueE,

    -- ** Handler
    runTBQueueIO,

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
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)

-- | Effect for 'STM'.
--
-- @since 0.1
data STMEffect :: Effect where
  Atomically :: (HasCallStack) => STM a -> STMEffect m a

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

-- | @since 0.1
atomically :: (HasCallStack, STMEffect :> es) => STM a -> Eff es a
atomically = send . Atomically

-- | Effect for 'TVar'.
--
-- @since 0.1
data TVarEffect :: Effect where
  NewTVarE :: (HasCallStack) => a -> TVarEffect m (TVar a)
  ReadTVarE :: (HasCallStack) => TVar a -> TVarEffect m a
  WriteTVarE :: (HasCallStack) => TVar a -> a -> TVarEffect m ()
  ModifyTVarE' :: (HasCallStack) => TVar a -> (a -> a) -> TVarEffect m ()

-- | @since 0.1
type instance DispatchOf TVarEffect = Dynamic

-- | Runs 'TVarEffect' in 'IO'.
--
-- @since 0.1
runTVarIO ::
  ( IOE :> es
  ) =>
  Eff (TVarEffect : es) a ->
  Eff es a
runTVarIO = interpret $ \_ -> \case
  NewTVarE x -> liftIO $ STM.atomically $ TVar.newTVar x
  ReadTVarE var -> liftIO $ STM.atomically $ TVar.readTVar var
  WriteTVarE var x -> liftIO $ STM.atomically $ TVar.writeTVar var x
  ModifyTVarE' var f -> liftIO $ STM.atomically $ TVar.modifyTVar' var f

-- | @since 0.1
newTVarE :: (HasCallStack, TVarEffect :> es) => a -> Eff es (TVar a)
newTVarE = send . NewTVarE

-- | @since 0.1
readTVarE :: (HasCallStack, TVarEffect :> es) => TVar a -> Eff es a
readTVarE = send . ReadTVarE

-- | @since 0.1
writeTVarE :: (HasCallStack, TVarEffect :> es) => TVar a -> a -> Eff es ()
writeTVarE var = send . WriteTVarE var

-- | @since 0.1
modifyTVarE' :: (HasCallStack, TVarEffect :> es) => TVar a -> (a -> a) -> Eff es ()
modifyTVarE' var = send . ModifyTVarE' var

-- | Effect for 'TBQueue'.
--
-- @since 0.1
data TBQueueEffect :: Effect where
  NewTBQueueE :: (HasCallStack) => Natural -> TBQueueEffect m (TBQueue a)
  ReadTBQueueE :: (HasCallStack) => TBQueue a -> TBQueueEffect m a
  TryReadTBQueueE :: (HasCallStack) => TBQueue a -> TBQueueEffect m (Maybe a)
  WriteTBQueueE :: (HasCallStack) => TBQueue a -> a -> TBQueueEffect m ()
  FlushTBQueueE :: (HasCallStack) => TBQueue a -> TBQueueEffect m [a]

-- | @since 0.1
type instance DispatchOf TBQueueEffect = Dynamic

-- | Runs 'TBQueueEffect' in 'IO'.
--
-- @since 0.1
runTBQueueIO ::
  ( IOE :> es
  ) =>
  Eff (TBQueueEffect : es) a ->
  Eff es a
runTBQueueIO = interpret $ \_ -> \case
  NewTBQueueE n -> liftIO $ STM.atomically $ TBQueue.newTBQueue n
  ReadTBQueueE q -> liftIO $ STM.atomically $ TBQueue.readTBQueue q
  TryReadTBQueueE q -> liftIO $ STM.atomically $ TBQueue.tryReadTBQueue q
  WriteTBQueueE q x -> liftIO $ STM.atomically $ TBQueue.writeTBQueue q x
  FlushTBQueueE q -> liftIO $ STM.atomically $ TBQueue.flushTBQueue q

-- | @since 0.1
newTBQueueE :: (HasCallStack, TBQueueEffect :> es) => Natural -> Eff es (TBQueue a)
newTBQueueE = send . NewTBQueueE

-- | @since 0.1
readTBQueueE :: (HasCallStack, TBQueueEffect :> es) => TBQueue a -> Eff es a
readTBQueueE = send . ReadTBQueueE

-- | @since 0.1
tryReadTBQueueE :: (HasCallStack, TBQueueEffect :> es) => TBQueue a -> Eff es (Maybe a)
tryReadTBQueueE = send . TryReadTBQueueE

-- | @since 0.1
writeTBQueueE :: (HasCallStack, TBQueueEffect :> es) => TBQueue a -> a -> Eff es ()
writeTBQueueE q = send . WriteTBQueueE q

-- | @since 0.1
flushTBQueueE :: (HasCallStack, TBQueueEffect :> es) => TBQueue a -> Eff es [a]
flushTBQueueE = send . FlushTBQueueE
