-- | Provides a dynamic effect for "System.Posix.Signals".
--
-- @since 0.1
module Effectful.Posix.Signals.Dynamic
  ( -- * Effect
    PosixSignals (..),
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
  )
where

import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    UnliftStrategy (SeqUnlift),
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( HasCallStack,
    localLiftUnlift,
    reinterpret,
    send,
  )
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.Posix.Signals.Handler (Handler)
import Effectful.Posix.Signals.Handler qualified as Handler
import Effectful.Posix.Signals.Static qualified as Static
import System.Posix.Signals (Signal, SignalSet)
import System.Posix.Types (ProcessGroupID, ProcessID)

-- | Dynamic effect for "System.Posix.Files".
--
-- @since 0.1
data PosixSignals :: Effect where
  RaiseSignal :: Signal -> PosixSignals m ()
  SignalProcess :: Signal -> ProcessID -> PosixSignals m ()
  SignalProcessGroup :: Signal -> ProcessGroupID -> PosixSignals m ()
  InstallHandler :: Signal -> Handler m -> Maybe SignalSet -> PosixSignals m (Handler m)
  GetSignalMask :: PosixSignals m SignalSet
  SetSignalMask :: SignalSet -> PosixSignals m ()
  BlockSignals :: SignalSet -> PosixSignals m ()
  UnblockSignals :: SignalSet -> PosixSignals m ()
  ScheduleAlarm :: Int -> PosixSignals m Int
  GetPendingSignals :: PosixSignals m SignalSet
  AwaitSignal :: Maybe SignalSet -> PosixSignals m ()
  SetStoppedChildFlag :: Bool -> PosixSignals m Bool
  QueryStoppedChildFlag :: PosixSignals m Bool

-- | @since 0.1
type instance DispatchOf PosixSignals = Dynamic

-- | @since 0.1
instance ShowEffect PosixSignals where
  showEffectCons = \case
    RaiseSignal {} -> "RaiseSignal"
    SignalProcess {} -> "SignalProcess"
    SignalProcessGroup {} -> "SignalProcessGroup"
    InstallHandler {} -> "InstallHandler"
    GetSignalMask {} -> "GetSignalMask"
    SetSignalMask {} -> "SetSignalMask"
    BlockSignals {} -> "BlockSignals"
    UnblockSignals {} -> "UnblockSignals"
    ScheduleAlarm {} -> "ScheduleAlarm"
    GetPendingSignals {} -> "GetPendingSignals"
    AwaitSignal {} -> "AwaitSignal"
    SetStoppedChildFlag {} -> "SetStoppedChildFlag"
    QueryStoppedChildFlag {} -> "QueryStoppedChildFlag"

-- | Runs 'PosixSignals' in 'IO'.
--
-- @since 0.1
runPosixSignals ::
  forall es a.
  (HasCallStack, IOE :> es) =>
  Eff (PosixSignals : es) a ->
  Eff es a
runPosixSignals = reinterpret Static.runPosixSignals $ \env -> \case
  RaiseSignal x1 -> Static.raiseSignal x1
  SignalProcess x1 x2 -> Static.signalProcess x1 x2
  SignalProcessGroup x1 x2 -> Static.signalProcessGroup x1 x2
  InstallHandler s h ms ->
    localLiftUnlift env SeqUnlift $ \lift unlift ->
      fmap (Handler.mapHandler lift)
        . (\h' -> Static.installHandler s h' ms)
        . Handler.mapHandler unlift
        $ h
  GetSignalMask -> Static.getSignalMask
  SetSignalMask x1 -> Static.setSignalMask x1
  BlockSignals x1 -> Static.blockSignals x1
  UnblockSignals x1 -> Static.unblockSignals x1
  ScheduleAlarm x1 -> Static.scheduleAlarm x1
  GetPendingSignals -> Static.getPendingSignals
  AwaitSignal x1 -> Static.awaitSignal x1
  SetStoppedChildFlag x1 -> Static.setStoppedChildFlag x1
  QueryStoppedChildFlag -> Static.queryStoppedChildFlag

-- | Lifted 'Signals.raiseSignal'.
--
-- @since 0.1
raiseSignal ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Signal ->
  Eff es ()
raiseSignal = send . RaiseSignal

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
signalProcess x1 = send . SignalProcess x1

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
signalProcessGroup x1 = send . SignalProcessGroup x1

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
installHandler x1 x2 = send . InstallHandler x1 x2

-- | Lifted 'Signals.getSignalMask'.
--
-- @since 0.1
getSignalMask ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Eff es SignalSet
getSignalMask = send GetSignalMask

-- | Lifted 'Signals.setSignalMask'.
--
-- @since 0.1
setSignalMask ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  SignalSet ->
  Eff es ()
setSignalMask = send . SetSignalMask

-- | Lifted 'Signals.blockSignals'.
--
-- @since 0.1
blockSignals ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  SignalSet ->
  Eff es ()
blockSignals = send . BlockSignals

-- | Lifted 'Signals.unblockSignals'.
--
-- @since 0.1
unblockSignals ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  SignalSet ->
  Eff es ()
unblockSignals = send . UnblockSignals

-- | Lifted 'Signals.scheduleAlarm'.
--
-- @since 0.1
scheduleAlarm ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Int ->
  Eff es Int
scheduleAlarm = send . ScheduleAlarm

-- | Lifted 'Signals.getPendingSignals'.
--
-- @since 0.1
getPendingSignals ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Eff es SignalSet
getPendingSignals = send GetPendingSignals

-- | Lifted 'Signals.awaitSignal'.
--
-- @since 0.1
awaitSignal ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Maybe SignalSet ->
  Eff es ()
awaitSignal = send . AwaitSignal

-- | Lifted 'Signals.setStoppedChildFlag'.
--
-- @since 0.1
setStoppedChildFlag ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Bool ->
  Eff es Bool
setStoppedChildFlag = send . SetStoppedChildFlag

-- | Lifted 'Signals.queryStoppedChildFlag'.
--
-- @since 0.1
queryStoppedChildFlag ::
  ( HasCallStack,
    PosixSignals :> es
  ) =>
  Eff es Bool
queryStoppedChildFlag = send QueryStoppedChildFlag
