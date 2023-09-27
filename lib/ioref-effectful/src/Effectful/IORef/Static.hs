{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for 'IORef'.
--
-- @since 0.1
module Effectful.IORef.Static
  ( -- * Class
    MonadIORef (..),

    -- * Effect
    IORefStatic,

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

-- | 'IORef' effect.
--
-- @since 0.1
class (Monad m) => MonadIORef m where
  -- | Lifted 'IORef.newIORef'.
  --
  -- @since 0.1
  newIORef :: a -> m (IORef a)

  -- | Lifted 'IORef.readIORef'.
  --
  -- @since 0.1
  readIORef :: IORef a -> m a

  -- | Lifted 'IORef.writeIORef'.
  --
  -- @since 0.1
  writeIORef :: IORef a -> a -> m ()

  -- | Lifted 'IORef.atomicWriteIORef'.
  --
  -- @since 0.1
  atomicWriteIORef :: IORef a -> a -> m ()

  -- | Lifted 'IORef.modifyIORef''.
  --
  -- @since 0.1
  modifyIORef' :: IORef a -> (a -> a) -> m ()

  -- | Lifted 'IORef.atomicModifyIORef''.
  --
  -- @since 0.1
  atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> m b

-- | @since 0.1
instance MonadIORef IO where
  newIORef = IORef.newIORef
  readIORef = IORef.readIORef
  writeIORef = IORef.writeIORef
  atomicWriteIORef = IORef.atomicWriteIORef
  modifyIORef' = IORef.modifyIORef'
  atomicModifyIORef' = IORef.atomicModifyIORef'

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

-- | @since 0.1
instance (IORefStatic :> es) => MonadIORef (Eff es) where
  newIORef = unsafeEff_ . IORef.newIORef
  readIORef = unsafeEff_ . IORef.readIORef
  writeIORef ref = unsafeEff_ . IORef.writeIORef ref
  atomicWriteIORef ref = unsafeEff_ . IORef.atomicWriteIORef ref
  modifyIORef' ref = unsafeEff_ . IORef.modifyIORef' ref
  atomicModifyIORef' ref = unsafeEff_ . IORef.atomicModifyIORef' ref

-- | Variant of 'atomicModifyIORef'' which ignores the return value.
--
-- @since 0.1
atomicModifyIORef'_ ::
  ( MonadIORef m
  ) =>
  IORef a ->
  (a -> a) ->
  m ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \a -> (f a, ())
