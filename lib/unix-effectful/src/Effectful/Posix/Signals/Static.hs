{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for "System.Posix.Signals".
--
-- @since 0.1
module Effectful.Posix.Signals.Static
  ( -- * Effect
    PosixSignals,
    raiseSignal,
    signalProcess,
    signalProcessGroup,
    installHandler,
    getSignalMask,
    setSignalMask,
    blockSignals,
    unblockSignals,
    scheduleAlarm,
    getPendingSignals,
    awaitSignal,
    setStoppedChildFlag,
    queryStoppedChildFlag,

    -- ** Handler
    runPosixSignals,

    -- * Posix Handler
    Handler (..),
    Handler.mapHandler,
    Handler.handlerToPosix,
    Handler.handlerFromPosix,

    -- * Re-exports
    Signal,
    SignalSet,
    ProcessID,
    ProcessGroupID,
  )
where

import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( HasCallStack,
    SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    seqUnliftIO,
    unsafeEff,
    unsafeEff_,
  )
import Effectful.Posix.Signals.Handler (Handler)
import Effectful.Posix.Signals.Handler qualified as Handler
import System.Posix.Signals (Signal, SignalSet)
import System.Posix.Signals qualified as Signals
import System.Posix.Types (ProcessGroupID, ProcessID)

-- | Provides a static effect for "System.Posix.Signals".
--
-- @since 0.1
data PosixSignals :: Effect

type instance DispatchOf PosixSignals = Static WithSideEffects

data instance StaticRep PosixSignals = MkPosixSignals

-- | Runs a PosixSignals effect.
--
-- @since 0.1
runPosixSignals ::
  (HasCallStack, IOE :> es) =>
  Eff (PosixSignals : es) a ->
  Eff es a
runPosixSignals = evalStaticRep MkPosixSignals

-- | Lifted 'Signals.raiseSignal'.
--
-- @since 0.1
raiseSignal ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Signal ->
  Eff es ()
raiseSignal = unsafeEff_ . Signals.raiseSignal

-- | Lifted 'Signals.signalProcess'.
--
-- @since 0.1
signalProcess ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Signal ->
  ProcessID ->
  Eff es ()
signalProcess x1 = unsafeEff_ . Signals.signalProcess x1

-- | Lifted 'Signals.signalProcessGroup'.
--
-- @since 0.1
signalProcessGroup ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Signal ->
  ProcessGroupID ->
  Eff es ()
signalProcessGroup x1 = unsafeEff_ . Signals.signalProcessGroup x1

-- | Lifted 'Signals.installHandler'.
--
-- @since 0.1
installHandler ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Signal ->
  Handler (Eff es) ->
  Maybe SignalSet ->
  Eff es (Handler (Eff es))
installHandler s h ms =
  unsafeEff $ \env -> seqUnliftIO env $ \runInIO ->
    fmap (Handler.mapHandler unsafeEff_ . Handler.handlerFromPosix)
      . (\x -> Signals.installHandler s x ms)
      . Handler.handlerToPosix
      . Handler.mapHandler runInIO
      $ h

-- | Lifted 'Signals.getSignalMask'.
--
-- @since 0.1
getSignalMask ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Eff es SignalSet
getSignalMask = unsafeEff_ Signals.getSignalMask

-- | Lifted 'Signals.setSignalMask'.
--
-- @since 0.1
setSignalMask ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  SignalSet ->
  Eff es ()
setSignalMask = unsafeEff_ . Signals.setSignalMask

-- | Lifted 'Signals.blockSignals'.
--
-- @since 0.1
blockSignals ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  SignalSet ->
  Eff es ()
blockSignals = unsafeEff_ . Signals.blockSignals

-- | Lifted 'Signals.unblockSignals'.
--
-- @since 0.1
unblockSignals ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  SignalSet ->
  Eff es ()
unblockSignals = unsafeEff_ . Signals.unblockSignals

-- | Lifted 'Signals.scheduleAlarm'.
--
-- @since 0.1
scheduleAlarm ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Int ->
  Eff es Int
scheduleAlarm = unsafeEff_ . Signals.scheduleAlarm

-- | Lifted 'Signals.getPendingSignals'.
--
-- @since 0.1
getPendingSignals ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Eff es SignalSet
getPendingSignals = unsafeEff_ Signals.getPendingSignals

-- | Lifted 'Signals.awaitSignal'.
--
-- @since 0.1
awaitSignal ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Maybe SignalSet ->
  Eff es ()
awaitSignal = unsafeEff_ . Signals.awaitSignal

-- | Lifted 'Signals.setStoppedChildFlag'.
--
-- @since 0.1
setStoppedChildFlag ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Bool ->
  Eff es Bool
setStoppedChildFlag = unsafeEff_ . Signals.setStoppedChildFlag

-- | Lifted 'Signals.queryStoppedChildFlag'.
--
-- @since 0.1
queryStoppedChildFlag ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Eff es Bool
queryStoppedChildFlag = unsafeEff_ Signals.queryStoppedChildFlag
