module Effectful.Posix.Signals.Handler
  ( Handler (..),
    mapHandler,
    handlerToPosix,
    handlerFromPosix,

    -- * Concurrency strategy
    persistence,
    limit,
  )
where

import Effectful (Limit (Unlimited), Persistence (Ephemeral))
import System.Posix.Signals (SignalInfo)
import System.Posix.Signals qualified as Signals

-- | @since 0.1
data Handler m
  = Default
  | Ignore
  | Catch (m ())
  | CatchOnce (m ())
  | CatchInfo (SignalInfo -> m ())
  | CatchInfoOnce (SignalInfo -> m ())

-- | @since 0.1
mapHandler :: (forall x. m x -> n x) -> Handler m -> Handler n
mapHandler f = \case
  Default -> Default
  Ignore -> Ignore
  Catch x -> Catch $ f x
  CatchOnce x -> CatchOnce $ f x
  CatchInfo x -> CatchInfo $ f . x
  CatchInfoOnce x -> CatchInfoOnce $ f . x

-- | @since 0.1
handlerToPosix :: Handler IO -> Signals.Handler
handlerToPosix = \case
  Default -> Signals.Default
  Ignore -> Signals.Ignore
  Catch x -> Signals.Catch x
  CatchOnce x -> Signals.CatchOnce x
  CatchInfo x -> Signals.CatchInfo x
  CatchInfoOnce x -> Signals.CatchInfoOnce x

-- | @since 0.1
handlerFromPosix :: Signals.Handler -> Handler IO
handlerFromPosix = \case
  Signals.Default -> Default
  Signals.Ignore -> Ignore
  Signals.Catch x -> Catch x
  Signals.CatchOnce x -> CatchOnce x
  Signals.CatchInfo x -> CatchInfo x
  Signals.CatchInfoOnce x -> CatchInfoOnce x

-- NOTE: [installHandler concurrency]
--
-- We /cannot/ use the sequential unlifting strategy for installHandler,
-- because the handler action might be invoked from a new thread (e.g. Catch).
--
-- A real-life bug was observed when this installHandler used seqUnliftIO,
-- and the action threw an Exception to another thread. I am unsure if the
-- action matters (e.g. trying with 'pure ()' would be interesting).
--
-- Regarding the strategy:
--
-- - Persistence: Persistent/Ephemeral matters when the unlifting function
--   is called multiple times /in the same thread/. Persistent persists
--   state changes, Ephemeral does not.
--
--   In the absence of a compelling example, let's default to Ephemeral.
--
-- - Limit: Anecdotally, usage seems to work with 'Limited 1', but we
--   will allow Unlimited, out of an abundance of caution.

persistence :: Persistence
persistence = Ephemeral

limit :: Limit
limit = Unlimited
