module Effectful.Notify.System.DBus
  ( -- * Notifications
    I.DBus.initNotifyEnv,
    I.DBus.notify,

    -- * Re-exports

    -- ** Errors
    I.DBus.ClientError,
    I.DBus.clientError,
    I.DBus.clientErrorMessage,
    I.DBus.clientErrorFatal,
  )
where

import Effectful.Notify.Internal.System.DBus qualified as I.DBus
