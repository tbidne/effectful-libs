-- | 'TVar' helpers for static 'Concurrent' effect.
--
-- @since 0.1
module Effectful.Concurrent.STM.TVar.Static
  ( -- * TVar
    newTVarA,
    readTVarA,
    writeTVarA,
    modifyTVarA',

    -- * Re-exports
    Concurrent,
    runConcurrent,
    TVar,
  )
where

import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar qualified as TVar
import Effectful (Eff, type (:>))
import Effectful.Concurrent.STM (Concurrent, atomically, runConcurrent)

-- | @since 0.1
newTVarA :: (Concurrent :> es) => a -> Eff es (TVar a)
newTVarA = atomically . TVar.newTVar

-- | @since 0.1
readTVarA :: (Concurrent :> es) => TVar a -> Eff es a
readTVarA = atomically . TVar.readTVar

-- | @since 0.1
writeTVarA :: (Concurrent :> es) => TVar a -> a -> Eff es ()
writeTVarA var = atomically . TVar.writeTVar var

-- | @since 0.1
modifyTVarA' :: (Concurrent :> es) => TVar a -> (a -> a) -> Eff es ()
modifyTVarA' var = atomically . TVar.modifyTVar' var
