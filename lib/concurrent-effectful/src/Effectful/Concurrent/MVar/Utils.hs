module Effectful.Concurrent.MVar.Utils
  ( -- * Strict variants
    -- $strict
    newMVar',
    takeMVar',
    putMVar',
    tryTakeMVar',
    tryPutMVar',
    withMVar',
    withMVarMasked',
    modifyMVar',
    modifyMVar_',
    modifyMVarMasked',
    modifyMVarMasked_',
    tryReadMVar',
  )
where

import Control.Concurrent (MVar)
import Control.Monad ((>=>))
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Exception (evaluate)

-- $strict
--
-- These functions are strict variants of the normal functions in the sense
-- that they act is if the MVar held its argument in WHNF e.g.
-- @MVar a = MVar !a@.
--
-- To that end, MVar values are evaluated before writes and after reads.
-- Consequently, @modify*@ functions perform two evaluations.
--
-- Note that these evaluations apply only to the value in the MVar e.g.
-- the @b@ in 'withMVar'' is not given any extra evaluation.

-- | @since 0.1
newMVar' :: (Concurrent :> es) => a -> Eff es (MVar a)
newMVar' = evaluate >=> MVar.newMVar

-- | @since 0.1
takeMVar' :: (Concurrent :> es) => MVar a -> Eff es a
takeMVar' = MVar.takeMVar >=> evaluate

-- | @since 0.1
putMVar' :: (Concurrent :> es) => MVar a -> a -> Eff es ()
putMVar' v = evaluate >=> MVar.putMVar v

-- | @since 0.1
tryTakeMVar' :: (Concurrent :> es) => MVar a -> Eff es (Maybe a)
tryTakeMVar' v =
  MVar.tryTakeMVar v >>= \case
    Nothing -> pure Nothing
    Just x -> Just <$> evaluate x

-- | @since 0.1
tryPutMVar' :: (Concurrent :> es) => MVar a -> a -> Eff es Bool
tryPutMVar' v = evaluate >=> MVar.tryPutMVar v

-- | @since 0.1
withMVar' :: (Concurrent :> es) => MVar a -> (a -> Eff es b) -> Eff es b
withMVar' v f = MVar.withMVar v (evaluate >=> f)

-- | @since 0.1
withMVarMasked' :: (Concurrent :> es) => MVar a -> (a -> Eff es b) -> Eff es b
withMVarMasked' v f = MVar.withMVarMasked v (evaluate >=> f)

-- | @since 0.1
modifyMVar_' :: (Concurrent :> es) => MVar a -> (a -> Eff es a) -> Eff es ()
modifyMVar_' v f = MVar.modifyMVar_ v (evaluate >=> f >=> evaluate)

-- | @since 0.1
modifyMVar' :: (Concurrent :> es) => MVar a -> (a -> Eff es (a, b)) -> Eff es b
modifyMVar' v f = MVar.modifyMVar v $ \x -> do
  (a, b) <- f =<< evaluate x
  (,b) <$> evaluate a

-- | @since 0.1
modifyMVarMasked_' :: (Concurrent :> es) => MVar a -> (a -> Eff es a) -> Eff es ()
modifyMVarMasked_' v f = MVar.modifyMVarMasked_ v (evaluate >=> f >=> evaluate)

-- | @since 0.1
modifyMVarMasked' :: (Concurrent :> es) => MVar a -> (a -> Eff es (a, b)) -> Eff es b
modifyMVarMasked' v f = MVar.modifyMVarMasked v $ \x -> do
  (a, b) <- f =<< evaluate x
  (,b) <$> evaluate a

-- | @since 0.1
tryReadMVar' :: (Concurrent :> es) => MVar a -> Eff es (Maybe a)
tryReadMVar' v =
  MVar.tryReadMVar v >>= \case
    Nothing -> pure Nothing
    Just x -> Just <$> evaluate x
