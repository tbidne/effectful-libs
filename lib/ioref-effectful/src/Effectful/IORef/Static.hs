{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for 'IORef'.
--
-- @since 0.1
module Effectful.IORef.Static
  ( -- * Effect
    IORefStatic,
    newIORef,
    readIORef,
    writeIORef,
    modifyIORef',
    atomicModifyIORef',

    -- ** Handlers
    runIORefStaticIO,

    -- * Utils
    atomicModifyIORef'_,

    -- * Re-exports
    IORef,
  )
where

import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    unsafeEff_,
  )

-- | Static effect for 'IORef'.
--
-- @since 0.1
data IORefStatic :: Effect

type instance DispatchOf IORefStatic = Static WithSideEffects

data instance StaticRep IORefStatic = MkIORefStatic

-- | Runs an IORefStatic effect.
--
-- @since 0.1
runIORefStaticIO :: (IOE :> es) => Eff (IORefStatic : es) a -> Eff es a
runIORefStaticIO = evalStaticRep MkIORefStatic

-- | Lifted 'IORef.newIORef'.
--
-- @since 0.1
newIORef :: (IORefStatic :> es) => a -> Eff es (IORef a)
newIORef = unsafeEff_ . IORef.newIORef

-- | Lifted 'IORef.readIORef'.
--
-- @since 0.1
readIORef :: (IORefStatic :> es) => IORef a -> Eff es a
readIORef = unsafeEff_ . IORef.readIORef

-- | Lifted 'IORef.writeIORef'.
--
-- @since 0.1
writeIORef :: (IORefStatic :> es) => IORef a -> a -> Eff es ()
writeIORef ref = unsafeEff_ . IORef.writeIORef ref

-- | Lifted 'IORef.modifyIORef''.
--
-- @since 0.1
modifyIORef' ::
  ( IORefStatic :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
modifyIORef' ref = unsafeEff_ . IORef.modifyIORef' ref

-- | Lifted 'IORef.atomicModifyIORef''.
--
-- @since 0.1
atomicModifyIORef' ::
  ( IORefStatic :> es
  ) =>
  IORef a ->
  (a -> (a, b)) ->
  Eff es b
atomicModifyIORef' ref = unsafeEff_ . IORef.atomicModifyIORef' ref

-- | Variant of 'atomicModifyIORef'' which ignores the return value.
--
-- @since 0.1
atomicModifyIORef'_ ::
  ( IORefStatic :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \a -> (f a, ())
