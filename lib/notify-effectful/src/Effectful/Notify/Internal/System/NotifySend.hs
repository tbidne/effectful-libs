{-# LANGUAGE CPP #-}

module Effectful.Notify.Internal.System.NotifySend
  ( notify,
  )
where

import Control.Exception (throwIO)
import Control.Exception.Utils qualified as Ex.Utils
import Data.Text qualified as T
import Effectful.Notify.Internal.Data.Note (Note)
import Effectful.Notify.Internal.Data.NotifyEnv
  ( NotifyEnv (NotifyEnvNotifySend),
  )
import Effectful.Notify.Internal.Data.NotifyException
  ( NotifyException
      ( MkNotifyException,
        exception,
        fatal,
        note,
        notifyEnv
      ),
  )
import Effectful.Notify.Internal.Data.NotifyTimeout
  ( NotifyTimeout (NotifyTimeoutMillis, NotifyTimeoutNever),
  )
import Effectful.Notify.Internal.Data.NotifyUrgency
  ( NotifyUrgency (NotifyUrgencyCritical, NotifyUrgencyLow, NotifyUrgencyNormal),
  )
import Effectful.Notify.Internal.Utils qualified as Utils
import Optics.Core ((^.))

notify :: Note -> IO ()
notify note =
  sendNote note `Ex.Utils.catchSync` \ex ->
    throwIO $
      MkNotifyException
        { exception = ex,
          fatal = True,
          note,
          notifyEnv = NotifyEnvNotifySend
        }
  where
    sendNote = Utils.runProcessIO . noteToNotifySend

noteToNotifySend :: Note -> String
noteToNotifySend note =
  T.unpack
    . mconcat
    $ [ "notify-send ",
        maybe "" ((" --app-name " <>) . Utils.mkProcessText) (note ^. #title),
        Utils.mkProcessText $ note ^. #summary,
        maybe "" Utils.mkProcessText (note ^. #body),
        maybe "" ((" --urgency " <>) . ulToNS) (note ^. #urgency),
        maybe "" ((" --expire-time " <>) . toToNS) (note ^. #timeout)
      ]
  where
    ulToNS NotifyUrgencyLow = "low"
    ulToNS NotifyUrgencyNormal = "normal"
    ulToNS NotifyUrgencyCritical = "critical"

    toToNS NotifyTimeoutNever = "0"
    toToNS (NotifyTimeoutMillis n) = Utils.showt n
