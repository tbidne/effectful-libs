-- | Provides an effect for 'IORef'.
--
-- @since 0.1
module Effectful.IORef
  ( -- * Effect
    EffectIORef (..),

    -- * Handler
    runIORefIO,

    -- * Functions
    newIORef,
    readIORef,
    writeIORef,
    modifyIORef',
    atomicModifyIORef',

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
import Effectful.CallStack
  ( ECallStack,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Stack (HasCallStack)

-- | Effect for 'IORef'.
--
-- @since 0.1
data EffectIORef :: Effect where
  NewIORef :: HasCallStack => a -> EffectIORef m (IORef a)
  ReadIORef :: HasCallStack => IORef a -> EffectIORef m a
  WriteIORef :: HasCallStack => IORef a -> a -> EffectIORef m ()
  ModifyIORef' :: HasCallStack => IORef a -> (a -> a) -> EffectIORef m ()
  AtomicModifyIORef' :: HasCallStack => IORef a -> (a -> (a, b)) -> EffectIORef m b

-- | @since 0.1
type instance DispatchOf EffectIORef = Dynamic

-- | Runs 'EffectIORef' in 'IO'.
--
-- @since 0.1
runIORefIO ::
  ( ECallStack :> es,
    IOE :> es
  ) =>
  Eff (EffectIORef : es) a ->
  Eff es a
runIORefIO = interpret $ \_ -> \case
  NewIORef x -> addCallStack $ liftIO $ IORef.newIORef x
  ReadIORef ref -> addCallStack $ liftIO $ IORef.readIORef ref
  WriteIORef ref x -> addCallStack $ liftIO $ IORef.writeIORef ref x
  ModifyIORef' ref f -> addCallStack $ liftIO $ IORef.modifyIORef' ref f
  AtomicModifyIORef' ref f -> addCallStack $ liftIO $ IORef.atomicModifyIORef' ref f

-- | @since 0.1
newIORef :: (HasCallStack, EffectIORef :> es) => a -> Eff es (IORef a)
newIORef x = send (NewIORef x)

-- | @since 0.1
readIORef :: (HasCallStack, EffectIORef :> es) => IORef a -> Eff es a
readIORef ref = send (ReadIORef ref)

-- | @since 0.1
writeIORef :: (HasCallStack, EffectIORef :> es) => IORef a -> a -> Eff es ()
writeIORef ref x = send (WriteIORef ref x)

-- | @since 0.1
modifyIORef' ::
  ( HasCallStack,
    EffectIORef :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
modifyIORef' ref f = send (ModifyIORef' ref f)

-- | @since 0.1
atomicModifyIORef' ::
  ( HasCallStack,
    EffectIORef :> es
  ) =>
  IORef a ->
  (a -> (a, b)) ->
  Eff es b
atomicModifyIORef' ref f = send (AtomicModifyIORef' ref f)

-- | Variant of 'atomicModifyIORef'' which ignores the return value
atomicModifyIORef'_ ::
  ( HasCallStack,
    EffectIORef :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \a -> (f a, ())
