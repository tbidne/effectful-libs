-- | Provides static namespaced logging functionality on top of 'Logger'.
--
-- @since 0.1
module Effectful.LoggerNS.Static
  ( -- * Effect
    LoggerNS,
    Namespace (..),
    addNamespace,
    getNamespace,
    localNamespace,

    -- ** Handler
    runLoggerNS,

    -- * Formatting
    LogFormatter (..),
    LoggerNS.Utils.defaultLogFormatter,
    LocStrategy (..),
    formatLog,

    -- * LogStr
    LoggerNS.Utils.logStrToBs,
    LoggerNS.Utils.logStrToText,

    -- * Optics
    LoggerNS.Utils._LocPartial,
    LoggerNS.Utils._LocStable,
    LoggerNS.Utils._LocNone,

    -- * Re-exports
    LogStr,
    Loc,
  )
where

import Data.Sequence ((|>))
import Data.Text (Text)
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    type (:>),
  )
import Effectful.Concurrent.Static (Concurrent)
import Effectful.Dispatch.Static
  ( HasCallStack,
    SideEffects (NoSideEffects),
    StaticRep,
    evalStaticRep,
    getStaticRep,
    localStaticRep,
  )
import Effectful.Logger.Dynamic (LogLevel, LogStr, ToLogStr)
import Effectful.LoggerNS.Utils
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter (locStrategy, newline, threadLabel, timezone),
    Namespace (unNamespace),
  )
import Effectful.LoggerNS.Utils qualified as LoggerNS.Utils
import Effectful.Time.Dynamic (Time)
import Language.Haskell.TH (Loc)
import Optics.Core (over')

-- | Static effect for a namespaced logger.
--
-- @since 0.1
data LoggerNS :: Effect

type instance DispatchOf LoggerNS = Static NoSideEffects

newtype instance StaticRep LoggerNS = MkLoggerNS Namespace

-- | Handler for 'LoggerNS'.
--
-- @since 0.1
runLoggerNS ::
  (HasCallStack) =>
  -- | Initial namespace.
  Namespace ->
  Eff (LoggerNS : es) a ->
  Eff es a
runLoggerNS r = evalStaticRep (MkLoggerNS r)

-- | Retrieves the namespace.
--
-- @since 0.1
getNamespace :: (HasCallStack, LoggerNS :> es) => Eff es Namespace
getNamespace = do
  MkLoggerNS r <- getStaticRep
  pure r

-- | Locally modifies the namespace.
--
-- @since 0.1
localNamespace ::
  ( HasCallStack,
    LoggerNS :> es
  ) =>
  -- | Modifier.
  (Namespace -> Namespace) ->
  Eff es a ->
  Eff es a
localNamespace f = localStaticRep $ \(MkLoggerNS r) -> MkLoggerNS (f r)

-- | Adds to the namespace.
--
-- @since 0.1
addNamespace ::
  ( HasCallStack,
    LoggerNS :> es
  ) =>
  -- | New namespace.
  Text ->
  Eff es a ->
  Eff es a
addNamespace txt = localNamespace (over' #unNamespace (|> txt))

-- | Produces a formatted 'LogStr' in terms of:
--
-- - Static Concurrent.
-- - Static LoggerNS.
-- - Dynamic Time.
--
-- __Example__
--
-- @
-- -- [timestamp][thread_label][namespace][level] msg
-- [2022-02-08 10:20:05][thread-label][one.two][Warn][filename:1:2] msg
-- @
--
-- @since 0.1
formatLog ::
  ( Concurrent :> es,
    HasCallStack,
    LoggerNS :> es,
    Time :> es,
    ToLogStr msg
  ) =>
  -- | Formatter.
  LogFormatter ->
  -- | Log level.
  LogLevel ->
  -- | Message.
  msg ->
  Eff es LogStr
formatLog formatter lvl msg = do
  namespace <- getNamespace
  LoggerNS.Utils.formatLog namespace formatter lvl msg
