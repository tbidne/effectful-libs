{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for 'IORef'.
--
-- @since 0.1
module Effectful.IORef.Static
  ( -- * Effect
    IORefE,
    newIORef,
    readIORef,
    writeIORef,
    modifyIORef',
    atomicModifyIORef',

    -- ** Handlers
    runIORef,

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
data IORefE :: Effect

type instance DispatchOf IORefE = Static WithSideEffects

data instance StaticRep IORefE = MkIORefEtatic

-- | Runs an IORefE effect.
--
-- @since 0.1
runIORef :: (IOE :> es) => Eff (IORefE : es) a -> Eff es a
runIORef = evalStaticRep MkIORefEtatic

-- | Lifted 'IORef.newIORef'.
--
-- @since 0.1
newIORef :: (IORefE :> es) => a -> Eff es (IORef a)
newIORef = unsafeEff_ . IORef.newIORef

-- | Lifted 'IORef.readIORef'.
--
-- @since 0.1
readIORef :: (IORefE :> es) => IORef a -> Eff es a
readIORef = unsafeEff_ . IORef.readIORef

-- | Lifted 'IORef.writeIORef'.
--
-- @since 0.1
writeIORef :: (IORefE :> es) => IORef a -> a -> Eff es ()
writeIORef ref = unsafeEff_ . IORef.writeIORef ref

-- | Lifted 'IORef.modifyIORef''.
--
-- @since 0.1
modifyIORef' ::
  ( IORefE :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
modifyIORef' ref = unsafeEff_ . IORef.modifyIORef' ref

-- | Lifted 'IORef.atomicModifyIORef''.
--
-- @since 0.1
atomicModifyIORef' ::
  ( IORefE :> es
  ) =>
  IORef a ->
  (a -> (a, b)) ->
  Eff es b
atomicModifyIORef' ref = unsafeEff_ . IORef.atomicModifyIORef' ref

-- | Variant of 'atomicModifyIORef'' which ignores the return value.
--
-- @since 0.1
atomicModifyIORef'_ ::
  ( IORefE :> es
  ) =>
  IORef a ->
  (a -> a) ->
  Eff es ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \a -> (f a, ())
