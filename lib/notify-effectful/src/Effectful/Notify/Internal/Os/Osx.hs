{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Notify.Internal.Os.Osx
  ( initNotifyEnv,
    notify,
  )
where

import Effectful.Notify.Internal.Data.Note (Note)
import Effectful.Notify.Internal.Data.NotifyEnv
  ( NotifyEnv (NotifyEnvAppleScript),
  )
import Effectful.Notify.Internal.Data.NotifySystem
  ( NotifySystemOs (NotifySystemOsAppleScript),
  )
import Effectful.Notify.Internal.System.AppleScript qualified as AppleScript
import GHC.Stack.Types (HasCallStack)

initNotifyEnv :: (HasCallStack) => NotifySystemOs -> IO NotifyEnv
initNotifyEnv = \case
  NotifySystemOsAppleScript -> pure NotifyEnvAppleScript

notify :: NotifyEnv -> Note -> IO ()
notify env note = case env of
  NotifyEnvAppleScript -> AppleScript.notify note
