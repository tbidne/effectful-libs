{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- ORMOLU_DISABLE -}

module Effectful.Notify.Internal.Os.Windows
#if __GLASGOW_HASKELL__ >= 981
  {-# WARNING in "x-stub"
    "Notifications are not supported on Windows. These functions are mere stubs that do nothing."
    #-}
#else
  {-# WARNING
    "Notifications are not supported on Windows. These functions are mere stubs that do nothing."
    #-}
#endif
  ( initNotifyEnv,
    notify,
  )
where

{- ORMOLU_ENABLE -}

import Effectful.Notify.Internal.Data.Note (Note)
import Effectful.Notify.Internal.Data.NotifyEnv
  ( NotifyEnv (NotifyEnvWindows),
  )
import Effectful.Notify.Internal.Data.NotifySystem
  ( NotifySystemOs (NotifySystemOsWindows),
  )
import GHC.Stack.Types (HasCallStack)

initNotifyEnv :: (HasCallStack) => NotifySystemOs -> IO NotifyEnv
initNotifyEnv = \case
  NotifySystemOsWindows -> pure NotifyEnvWindows

notify :: NotifyEnv -> Note -> IO ()
notify env _ = case env of
  NotifyEnvWindows -> pure ()
