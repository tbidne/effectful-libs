-- | 'TVar' helpers for 'STMDynamic'.
--
-- @since 0.1
module Effectful.Concurrent.STM.TVar.Dynamic
  ( -- * TVar
    newTVarA,
    readTVarA,
    writeTVarA,
    modifyTVarA',

    -- * Re-exports
    STMDynamic,
    runSTMDynamicIO,
    TVar,
  )
where

import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar qualified as TVar
import Effectful (Eff, type (:>))
import Effectful.Concurrent.STM.Dynamic (STMDynamic, atomically, runSTMDynamicIO)

-- | @since 0.1
newTVarA :: (STMDynamic :> es) => a -> Eff es (TVar a)
newTVarA = atomically . TVar.newTVar

-- | @since 0.1
readTVarA :: (STMDynamic :> es) => TVar a -> Eff es a
readTVarA = atomically . TVar.readTVar

-- | @since 0.1
writeTVarA :: (STMDynamic :> es) => TVar a -> a -> Eff es ()
writeTVarA var = atomically . TVar.writeTVar var

-- | @since 0.1
modifyTVarA' :: (STMDynamic :> es) => TVar a -> (a -> a) -> Eff es ()
modifyTVarA' var = atomically . TVar.modifyTVar' var
