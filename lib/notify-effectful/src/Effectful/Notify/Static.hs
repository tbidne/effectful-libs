{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides an effect for desktop notifications.
--
-- @since 0.1
module Effectful.Notify.Static
  ( -- * Description
    -- $desc

    -- * Effect
    Notify,
    initNotifyEnv,
    notify,

    -- ** Handlers
    runNotify,

    -- ** Errors
    NotifyInitException (..),
    NotifyException (..),
    catchNonFatalNotify,
    tryNonFatalNotify,
    tryNonFatalNotify_,

    -- * Types

    -- ** System

    -- *** Platform-specific
    NotifySystem.NotifySystemOs (..),
    defaultNotifySystemOs,

    -- *** All platforms
    NotifySystem (..),
    defaultNotifySystem,
    notifySystemToOs,
    notifySystemFromOs,
    Os (..),
    NotifyParseException (..),

    -- ** Env
    NotifyEnv,
    notifyEnvToSystemOs,

    -- ** Notes
    Note,
    mkNote,
    setBody,
    getBody,
    setSummary,
    getSummary,
    setTimeout,
    getTimeout,
    setTitle,
    getTitle,
    setUrgency,
    getUrgency,

    -- ** Timeout
    NotifyTimeout (..),
    _NotifyTimeoutMillis,
    _NotifyTimeoutNever,

    -- ** NotifyUrgency
    NotifyUrgency (..),
    _NotifyUrgencyLow,
    _NotifyUrgencyNormal,
    _NotifyUrgencyCritical,
  )
where

import Control.Monad (void)
import Control.Monad.Catch qualified as C
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
    unsafeEff_,
  )
import Effectful.Notify.Internal.Data.Note
  ( Note,
    getBody,
    getSummary,
    getTimeout,
    getTitle,
    getUrgency,
    mkNote,
    setBody,
    setSummary,
    setTimeout,
    setTitle,
    setUrgency,
  )
import Effectful.Notify.Internal.Data.NotifyEnv (NotifyEnv, notifyEnvToSystemOs)
import Effectful.Notify.Internal.Data.NotifyException
  ( NotifyException (MkNotifyException, exception, fatal, note, notifySystem),
  )
import Effectful.Notify.Internal.Data.NotifyInitException
  ( NotifyInitException (MkNotifyInitException, unNotifyInitException),
  )
import Effectful.Notify.Internal.Data.NotifySystem
  ( NotifyParseException (MkNotifyParseException, os, system),
    NotifySystem
      ( NotifySystemAppleScript,
        NotifySystemDBus,
        NotifySystemNotifySend,
        NotifySystemWindows
      ),
    NotifySystemOs,
    defaultNotifySystem,
    defaultNotifySystemOs,
    notifySystemFromOs,
    notifySystemToOs,
  )
import Effectful.Notify.Internal.Data.NotifySystem qualified as NotifySystem
import Effectful.Notify.Internal.Data.NotifyTimeout
  ( NotifyTimeout (NotifyTimeoutMillis, NotifyTimeoutNever),
    _NotifyTimeoutMillis,
    _NotifyTimeoutNever,
  )
import Effectful.Notify.Internal.Data.NotifyUrgency
  ( NotifyUrgency (NotifyUrgencyCritical, NotifyUrgencyLow, NotifyUrgencyNormal),
    _NotifyUrgencyCritical,
    _NotifyUrgencyLow,
    _NotifyUrgencyNormal,
  )
import Effectful.Notify.Internal.Os (Os (Linux, Osx, Windows))
#if LINUX
import Effectful.Notify.Internal.Os.Linux qualified as Os
#elif OSX
import Effectful.Notify.Internal.Os.Osx qualified as Os
#else
import Effectful.Notify.Internal.Os.Windows qualified as Os
#endif
import Optics.Core ((^.))

-- | Static notify effect.
--
-- @since 0.1
data Notify :: Effect

type instance DispatchOf Notify = Static WithSideEffects

data instance StaticRep Notify = MkNotify

-- | Runs a 'Notify' effect in IO.
--
-- @since 0.1
runNotify ::
  forall es a.
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (Notify : es) a ->
  Eff es a
runNotify = evalStaticRep MkNotify

-- | Initialize the notification environment.
--
-- @since 0.1
initNotifyEnv ::
  forall es.
  ( HasCallStack,
    Notify :> es
  ) =>
  -- | System.
  NotifySystemOs ->
  Eff es NotifyEnv
initNotifyEnv = unsafeEff_ . Os.initNotifyEnv

-- | Send a notification.
--
-- @since 0.1
notify ::
  forall es.
  ( HasCallStack,
    Notify :> es
  ) =>
  -- | Env.
  NotifyEnv ->
  -- | Note.
  Note ->
  Eff es ()
notify env = unsafeEff_ . Os.notify env

-- | Runs 'notify' and runs the handler on any non-fatal 'NotifyException's.
-- Any other exceptions are rethrown.
--
-- @since 0.1
catchNonFatalNotify ::
  forall es.
  ( HasCallStack,
    Notify :> es
  ) =>
  -- | Env.
  NotifyEnv ->
  -- | Note.
  Note ->
  -- | Handler.
  (NotifyException -> Eff es ()) ->
  Eff es ()
catchNonFatalNotify env note handler =
  notify env note `C.catch` \ne ->
    if ne ^. #fatal
      then C.throwM ne
      else handler ne

-- | Like 'notify', but catches non-fatal 'NotifyException'.
--
-- @since 0.1
tryNonFatalNotify ::
  forall es.
  ( HasCallStack,
    Notify :> es
  ) =>
  -- | Env.
  NotifyEnv ->
  -- | Note.
  Note ->
  Eff es (Maybe NotifyException)
tryNonFatalNotify env note =
  C.try (notify env note) >>= \case
    Left ex ->
      if ex ^. #fatal
        then C.throwM ex
        else pure $ Just ex
    Right _ -> pure Nothing

-- | Like 'tryNonFatalNotify', but catches and discards the exception.
--
-- @since 0.1
tryNonFatalNotify_ ::
  forall es.
  ( HasCallStack,
    Notify :> es
  ) =>
  -- | Env.
  NotifyEnv ->
  -- | Note.
  Note ->
  Eff es ()
tryNonFatalNotify_ env = void . tryNonFatalNotify env

-- $desc
--
-- @effects-notify@ is a library for implementing cross-platform desktop
-- notifications. Currently supports:
--
-- - Linux: Supports @dbus@ and @notify-send@. That is, if there is a
--   running dbus notification server, it can be used directly.
--   @notify-send (libnotify)@ can also be used, though it also requires a
--   running dbus server.
--
-- - Osx: Supports built-in @apple-script@ i.e. @osascript@.
--
-- - Windows: Unsupported. Functions are mere stubs that will compile
--   (with a warning) and do nothing at runtime.
--
-- ==== Usage
--
-- General usage involves choosing the notification system and using that
-- to initialize the environment:
--
-- @
-- -- choose system
-- let systemOs :: 'NotifySystemOs'
--     systemOs = ...
--
-- -- prepare environment
-- notifyEnv <- 'initNotifyEnv' systemOs
--
-- -- send notification
-- let note =
--       'mkNote' "A summary"
--         & 'setBody' "Some notification"
--         & 'setTitle' "My Application"
--
-- 'notify' notifyEnv note
-- @
--
-- Choosing the system is more interesting. If we do not care about being
-- cross-platform, we can simply choose a 'NotifySystemOs' directly
-- and pass it to init:
--
-- @
-- -- The NotifySystemOsDBus constructor is only available on linux.
-- let systemOs :: 'NotifySystemOs'
--     systemOs = NotifySystemOsDBus
--
-- notifyEnv <- 'initNotifyEnv' systemOs
-- ...
-- @
--
-- If we need to work on several platforms, we can instead use 'NotifySystem',
-- which includes every possible notification system and is available on all
-- platforms. We then pass it to 'notifySystemToOs', which -- if successful --
-- will return the 'NotifySystemOs' and we can continue as normal.
--
-- @
-- -- All NotifySystem constructors are available on all platforms.
-- let system :: 'NotifySystem'
--     system = ...
--
-- systemOs <- 'notifySystemToOs' system \>\>= \\case
--   Left ex -> throwM ex
--   Right s -> pure s
-- notifyEnv <- 'initNotifyEnv' systemOs
-- ...
-- @
--
-- If a system is chosen that is unavailable on the current platform,
-- a 'NotifyParseException' will be returned.
