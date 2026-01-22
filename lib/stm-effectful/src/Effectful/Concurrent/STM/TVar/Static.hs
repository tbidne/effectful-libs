-- | 'TVar' helpers for static 'Concurrent' effect.
--
-- @since 0.1
module Effectful.Concurrent.STM.TVar.Static
  ( -- * TVar

    -- ** Strict
    newTVar',
    readTVar',
    writeTVar',
    TVar.modifyTVar',

    -- *** Atomic
    newTVarA',
    readTVarA',
    writeTVarA',
    modifyTVarA',

    -- ** Lazy
    TVar.newTVar,
    TVar.readTVar,
    TVar.writeTVar,
    TVar.modifyTVar,

    -- *** Atomic
    newTVarA,
    readTVarA,
    writeTVarA,
    modifyTVarA,

    -- * Re-exports
    Concurrent,
    runConcurrent,
    TVar,
  )
where

import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Monad ((>=>))
import Effectful (Eff, type (:>))
import Effectful.Concurrent.STM (Concurrent, STM, atomically, runConcurrent)
import Effectful.Concurrent.STM.Utils (evaluateSTM)

-- | @since 0.1
newTVar' :: a -> STM (TVar a)
newTVar' = evaluateSTM >=> TVar.newTVar

-- | @since 0.1
newTVarA :: (Concurrent :> es) => a -> Eff es (TVar a)
newTVarA = atomically . TVar.newTVar

-- | @since 0.1
newTVarA' :: (Concurrent :> es) => a -> Eff es (TVar a)
newTVarA' = atomically . newTVar'

-- | @since 0.1
readTVar' :: TVar a -> STM a
readTVar' = TVar.readTVar >=> evaluateSTM

-- | @since 0.1
readTVarA :: (Concurrent :> es) => TVar a -> Eff es a
readTVarA = atomically . TVar.readTVar

-- | @since 0.1
readTVarA' :: (Concurrent :> es) => TVar a -> Eff es a
readTVarA' = atomically . readTVar'

-- | @since 0.1
writeTVar' :: TVar a -> a -> STM ()
writeTVar' var = evaluateSTM >=> TVar.writeTVar var

-- | @since 0.1
writeTVarA :: (Concurrent :> es) => TVar a -> a -> Eff es ()
writeTVarA var = atomically . TVar.writeTVar var

-- | @since 0.1
writeTVarA' :: (Concurrent :> es) => TVar a -> a -> Eff es ()
writeTVarA' var = atomically . writeTVar' var

-- | @since 0.1
modifyTVarA' :: (Concurrent :> es) => TVar a -> (a -> a) -> Eff es ()
modifyTVarA' var = atomically . TVar.modifyTVar' var

-- | @since 0.1
modifyTVarA :: (Concurrent :> es) => TVar a -> (a -> a) -> Eff es ()
modifyTVarA var = atomically . TVar.modifyTVar var
