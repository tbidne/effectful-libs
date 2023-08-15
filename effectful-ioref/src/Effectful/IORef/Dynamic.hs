-- | Provides an effect for 'IORef'.
--
-- @since 0.1
module Effectful.IORef.Dynamic
  ( -- * Effect
    IORefEffect (..),
    newIORef,
    readIORef,
    writeIORef,
    modifyIORef',
    atomicModifyIORef',

    -- ** Handlers
    runIORefIO,

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
data IORefEffect :: Effect where
  NewIORef :: a -> IORefEffect m (IORef a)
  ReadIORef :: IORef a -> IORefEffect m a
  WriteIORef :: IORef a -> a -> IORefEffect m ()
  ModifyIORef' :: IORef a -> (a -> a) -> IORefEffect m ()
  AtomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IORefEffect m b

-- | @since 0.1
type instance DispatchOf IORefEffect = Dynamic

-- | Runs 'IORefEffect' in 'IO'.
--
-- @since 0.1
runIORefIO ::
  ( IOE :> es
  ) =>
  Eff (IORefEffect : es) a ->
  Eff es a
runIORefIO = interpret $ \_ -> \case
  NewIORef x -> liftIO $ IORef.newIORef x
  ReadIORef ref -> liftIO $ IORef.readIORef ref
  WriteIORef ref x -> liftIO $ IORef.writeIORef ref x
  ModifyIORef' ref f -> liftIO $ IORef.modifyIORef' ref f
  AtomicModifyIORef' ref f -> liftIO $ IORef.atomicModifyIORef' ref f

-- | Lifted 'IORef.newIORef'.
--
-- @since 0.1
newIORef :: (IORefEffect :> es) => a -> Eff es (IORef a)
newIORef = send . NewIORef

-- | Lifted 'IORef.readIORef'.
--
-- @since 0.1
readIORef :: (IORefEffect :> es) => IORef a -> Eff es a
readIORef = send . ReadIORef

-- | Lifted 'IORef.writeIORef'.
--
-- @since 0.1
writeIORef :: (IORefEffect :> es) => IORef a -> a -> Eff es ()
writeIORef ref = send . WriteIORef ref

-- | Lifted 'IORef.modifyIORef''.
--
-- @since 0.1
modifyIORef' ::
  ( IORefEffect :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
modifyIORef' ref = send . ModifyIORef' ref

-- | Lifted 'IORef.atomicModifyIORef''.
--
-- @since 0.1
atomicModifyIORef' ::
  ( IORefEffect :> es
  ) =>
  IORef a ->
  (a -> (a, b)) ->
  Eff es b
atomicModifyIORef' ref = send . AtomicModifyIORef' ref

-- | Variant of 'atomicModifyIORef'' which ignores the return value.
--
-- @since 0.1
atomicModifyIORef'_ ::
  ( IORefEffect :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \a -> (f a, ())
