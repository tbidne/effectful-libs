module Effectful.Posix.Signals.Handler
  ( -- * Handler
    Handler (..),
    mapHandler,

    -- ** Posix
    PosixHandler,
    mapHandlerToPosix,
    mapHandlerFromPosix,

    -- * Concurrency strategy
    persistence,
    limit,
  )
where

import Effectful (Limit (Unlimited), Persistence (Ephemeral))
import System.Posix.Signals (SignalInfo)
import System.Posix.Signals qualified as Signals

-- | Alias for unix's 'Signals.Handler'.
--
-- @since 0.1
type PosixHandler = Signals.Handler

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
{-# INLINEABLE mapHandler #-}

-- | @since 0.1
mapHandlerToPosix :: (forall x. m x -> IO x) -> Handler m -> PosixHandler
mapHandlerToPosix f = \case
  Default -> Signals.Default
  Ignore -> Signals.Ignore
  Catch x -> Signals.Catch $ f x
  CatchOnce x -> Signals.CatchOnce $ f x
  CatchInfo x -> Signals.CatchInfo $ f . x
  CatchInfoOnce x -> Signals.CatchInfoOnce $ f . x
{-# INLINEABLE mapHandlerToPosix #-}

-- | @since 0.1
mapHandlerFromPosix :: (forall x. IO x -> n x) -> PosixHandler -> Handler n
mapHandlerFromPosix f = \case
  Signals.Default -> Default
  Signals.Ignore -> Ignore
  Signals.Catch x -> Catch $ f x
  Signals.CatchOnce x -> CatchOnce $ f x
  Signals.CatchInfo x -> CatchInfo $ f . x
  Signals.CatchInfoOnce x -> CatchInfoOnce $ f . x
{-# INLINEABLE mapHandlerFromPosix #-}

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
