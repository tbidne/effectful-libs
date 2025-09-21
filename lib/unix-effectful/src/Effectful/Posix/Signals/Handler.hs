module Effectful.Posix.Signals.Handler
  ( Handler (..),
    mapHandler,
    handlerToPosix,
    handlerFromPosix,
  )
where

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
