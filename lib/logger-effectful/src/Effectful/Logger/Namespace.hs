{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides static namespaced logging functionality on top of
-- 'Effectful.Logger.Dynamic.Logger'.
--
-- @since 0.1
module Effectful.Logger.Namespace
  ( -- * Effect
    Namespace (..),
    addNamespace,

    -- * Constraint aliases
    HasNamespace,
    LoggerNS,

    -- * Formatting
    LogFormatter (..),
    Utils.defaultLogFormatter,
    LocStrategy (..),
    formatLog,

    -- * LogStr
    Utils.logStrToBs,
    Utils.logStrToText,

    -- * Optics

    -- ** LocStrategy
    _LocPartial,
    _LocStable,
    _LocNone,
  )
where

import Data.Kind (Constraint, Type)
import Data.Sequence (Seq ((:|>)))
import Data.Text (Text)
import Effectful (Eff, Effect, type (:>))
import Effectful.Concurrent.Static (Concurrent)
import Effectful.Dispatch.Static (HasCallStack)
import Effectful.Logger.Dynamic (LogLevel, LogStr, Logger, ToLogStr)
import Effectful.Logger.Utils
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter (locStrategy, newline, threadLabel, timezone),
    Namespace (unNamespace),
  )
import Effectful.Logger.Utils qualified as Utils
import Effectful.Reader.Static (Reader, asks, local)
import Effectful.Time.Dynamic (Time)
import Language.Haskell.TH (Loc)
import Optics.Core
  ( A_Getter,
    A_Setter,
    Is,
    LabelOptic',
    NoIx,
    Optic',
    Prism',
    Setter',
    castOptic,
    over',
    prism,
    view,
    (%),
  )

-- | Alias for constraints required to use namespaces.
type HasNamespace :: Type -> Type -> [Effect] -> Constraint
type HasNamespace env k es =
  ( Is k A_Getter,
    Is k A_Setter,
    LabelOptic' "namespace" k env Namespace,
    Reader env :> es
  )

-- | Alias for HasNamespace and Logger constraint.
type LoggerNS :: Type -> Type -> [Effect] -> Constraint
type LoggerNS env k es = (HasNamespace env k es, Logger :> es, Reader env :> es)

-- | Adds to the namespace.
--
-- @since 0.1
addNamespace ::
  forall env a k es.
  ( Is k A_Setter,
    LabelOptic' "namespace" k env Namespace,
    Reader env :> es
  ) =>
  -- | New namespace.
  Text ->
  Eff es a ->
  Eff es a
addNamespace txt = local ((over' (castSet #namespace % #unNamespace) (:|> txt)))
  where
    -- See https://github.com/well-typed/optics/issues/368 for why this is
    -- necessary.
    castSet :: Optic' k NoIx env Namespace -> Setter' env Namespace
    castSet = castOptic @A_Setter @k @_ @env @env @Namespace @Namespace
{-# INLINEABLE addNamespace #-}

-- | Produces a formatted 'LogStr' in terms of:
--
-- - Static Concurrent.
-- - Static Reader.
-- - Dynamic Time.
--
-- __Example__
--
-- @
-- -- [timestamp][thread_label][namespace][code_loc][level] msg
-- [2022-02-08 10:20:05][thread-label][one.two][filename:1:2][Warn] msg
-- @
--
-- @since 0.1
formatLog ::
  forall env msg k es.
  ( Concurrent :> es,
    Is k A_Getter,
    LabelOptic' "namespace" k env Namespace,
    HasCallStack,
    Reader env :> es,
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
  namespace <- asks @env (view #namespace)
  Utils.formatLog (Just namespace) formatter lvl msg

-- | @since 0.1
_LocPartial :: Prism' LocStrategy Loc
_LocPartial =
  prism
    LocPartial
    ( \case
        LocPartial loc -> Right loc
        x -> Left x
    )
{-# INLINE _LocPartial #-}

-- | @since 0.1
_LocStable :: Prism' LocStrategy Loc
_LocStable =
  prism
    LocStable
    ( \case
        LocStable loc -> Right loc
        x -> Left x
    )
{-# INLINE _LocStable #-}

-- | @since 0.1
_LocNone :: Prism' LocStrategy ()
_LocNone =
  prism
    (const LocNone)
    ( \case
        LocNone -> Right ()
        x -> Left x
    )
{-# INLINE _LocNone #-}
