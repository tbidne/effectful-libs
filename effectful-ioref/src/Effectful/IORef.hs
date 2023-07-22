-- | Provides an effect for 'IORef'.
--
-- @since 0.1
module Effectful.IORef
  ( -- * Effect
    IORefEffect (..),

    -- ** Functions
    newIORef,
    readIORef,
    writeIORef,
    modifyIORef',
    atomicModifyIORef',

    -- ** Handlers
    runIORefIO,

    -- * Utils
    atomicModifyIORef'_,

    -- * Reexports
    IORef,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Exception
  ( CallStackEffect,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Stack (HasCallStack)

-- | Effect for 'IORef'.
--
-- @since 0.1
data IORefEffect :: Effect where
  NewIORef :: (HasCallStack) => a -> IORefEffect m (IORef a)
  ReadIORef :: (HasCallStack) => IORef a -> IORefEffect m a
  WriteIORef :: (HasCallStack) => IORef a -> a -> IORefEffect m ()
  ModifyIORef' :: (HasCallStack) => IORef a -> (a -> a) -> IORefEffect m ()
  AtomicModifyIORef' :: (HasCallStack) => IORef a -> (a -> (a, b)) -> IORefEffect m b

-- | @since 0.1
type instance DispatchOf IORefEffect = Dynamic

-- | Runs 'IORefEffect' in 'IO'.
--
-- @since 0.1
runIORefIO ::
  ( CallStackEffect :> es,
    IOE :> es
  ) =>
  Eff (IORefEffect : es) a ->
  Eff es a
runIORefIO = interpret $ \_ -> \case
  NewIORef x -> addCallStack $ liftIO $ IORef.newIORef x
  ReadIORef ref -> addCallStack $ liftIO $ IORef.readIORef ref
  WriteIORef ref x -> addCallStack $ liftIO $ IORef.writeIORef ref x
  ModifyIORef' ref f -> addCallStack $ liftIO $ IORef.modifyIORef' ref f
  AtomicModifyIORef' ref f -> addCallStack $ liftIO $ IORef.atomicModifyIORef' ref f

-- | @since 0.1
newIORef :: (HasCallStack, IORefEffect :> es) => a -> Eff es (IORef a)
newIORef = send . NewIORef

-- | @since 0.1
readIORef :: (HasCallStack, IORefEffect :> es) => IORef a -> Eff es a
readIORef = send . ReadIORef

-- | @since 0.1
writeIORef :: (HasCallStack, IORefEffect :> es) => IORef a -> a -> Eff es ()
writeIORef ref = send . WriteIORef ref

-- | @since 0.1
modifyIORef' ::
  ( HasCallStack,
    IORefEffect :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
modifyIORef' ref = send . ModifyIORef' ref

-- | @since 0.1
atomicModifyIORef' ::
  ( HasCallStack,
    IORefEffect :> es
  ) =>
  IORef a ->
  (a -> (a, b)) ->
  Eff es b
atomicModifyIORef' ref = send . AtomicModifyIORef' ref

-- | Variant of 'atomicModifyIORef'' which ignores the return value
atomicModifyIORef'_ ::
  ( HasCallStack,
    IORefEffect :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \a -> (f a, ())
