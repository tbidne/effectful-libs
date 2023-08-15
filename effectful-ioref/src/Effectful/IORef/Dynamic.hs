-- | Provides an effect for 'IORef'.
--
-- @since 0.1
module Effectful.IORef.Dynamic
  ( -- * Effect
    IORefDynamic (..),
    newIORef,
    readIORef,
    writeIORef,
    modifyIORef',
    atomicModifyIORef',

    -- ** Handlers
    runIORefDynamicIO,

    -- * Utils
    atomicModifyIORef'_,

    -- * Re-exports
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
import Effectful.Dispatch.Dynamic (interpret, send)

-- | Effect for 'IORef'.
--
-- @since 0.1
data IORefDynamic :: Effect where
  NewIORef :: a -> IORefDynamic m (IORef a)
  ReadIORef :: IORef a -> IORefDynamic m a
  WriteIORef :: IORef a -> a -> IORefDynamic m ()
  ModifyIORef' :: IORef a -> (a -> a) -> IORefDynamic m ()
  AtomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IORefDynamic m b

-- | @since 0.1
type instance DispatchOf IORefDynamic = Dynamic

-- | Runs 'IORefDynamic' in 'IO'.
--
-- @since 0.1
runIORefDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (IORefDynamic : es) a ->
  Eff es a
runIORefDynamicIO = interpret $ \_ -> \case
  NewIORef x -> liftIO $ IORef.newIORef x
  ReadIORef ref -> liftIO $ IORef.readIORef ref
  WriteIORef ref x -> liftIO $ IORef.writeIORef ref x
  ModifyIORef' ref f -> liftIO $ IORef.modifyIORef' ref f
  AtomicModifyIORef' ref f -> liftIO $ IORef.atomicModifyIORef' ref f

-- | Lifted 'IORef.newIORef'.
--
-- @since 0.1
newIORef :: (IORefDynamic :> es) => a -> Eff es (IORef a)
newIORef = send . NewIORef

-- | Lifted 'IORef.readIORef'.
--
-- @since 0.1
readIORef :: (IORefDynamic :> es) => IORef a -> Eff es a
readIORef = send . ReadIORef

-- | Lifted 'IORef.writeIORef'.
--
-- @since 0.1
writeIORef :: (IORefDynamic :> es) => IORef a -> a -> Eff es ()
writeIORef ref = send . WriteIORef ref

-- | Lifted 'IORef.modifyIORef''.
--
-- @since 0.1
modifyIORef' ::
  ( IORefDynamic :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
modifyIORef' ref = send . ModifyIORef' ref

-- | Lifted 'IORef.atomicModifyIORef''.
--
-- @since 0.1
atomicModifyIORef' ::
  ( IORefDynamic :> es
  ) =>
  IORef a ->
  (a -> (a, b)) ->
  Eff es b
atomicModifyIORef' ref = send . AtomicModifyIORef' ref

-- | Variant of 'atomicModifyIORef'' which ignores the return value.
--
-- @since 0.1
atomicModifyIORef'_ ::
  ( IORefDynamic :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \a -> (f a, ())
