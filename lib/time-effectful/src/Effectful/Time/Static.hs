{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a dynamic effect for "Data.Time".
--
-- @since 0.1
module Effectful.Time.Static
  ( -- * Effect
    Time,
    getSystemTime,
    getSystemZonedTime,
    getMonotonicTime,

    -- ** Handlers
    runTime,

    -- * Timing
    withTiming,
    withTiming_,

    -- ** TimeSpec
    TimeSpec (..),

    -- *** Creation
    TimeSpec.fromSeconds,
    TimeSpec.fromNanoSeconds,

    -- *** Elimination
    TimeSpec.toSeconds,
    TimeSpec.toNanoSeconds,

    -- *** Operations
    TimeSpec.diffTimeSpec,
    TimeSpec.normalizeTimeSpec,

    -- * Formatting
    Utils.formatLocalTime,
    Utils.formatZonedTime,

    -- * Parsing
    Utils.parseLocalTime,
    Utils.parseZonedTime,

    -- * Misc
    getSystemTimeString,
    getSystemZonedTimeString,

    -- * Re-exports
    LocalTime (..),
    ZonedTime (..),
  )
where

import Data.Time.LocalTime
  ( LocalTime (LocalTime, localDay, localTimeOfDay),
    ZonedTime (ZonedTime, zonedTimeToLocalTime, zonedTimeZone),
  )
import Data.Time.LocalTime qualified as Local
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    unsafeEff_,
  )
import Effectful.Time.TimeSpec (TimeSpec (nsec, sec))
import Effectful.Time.TimeSpec qualified as TimeSpec
import Effectful.Time.Utils qualified as Utils
import GHC.Clock qualified as C

-- | Static time effect.
--
-- @since 0.1
data Time :: Effect

type instance DispatchOf Time = Static WithSideEffects

data instance StaticRep Time = MkTime

-- | Runs a 'Time' effect in IO.
--
-- @since 0.1
runTime :: (IOE :> es) => Eff (Time : es) a -> Eff es a
runTime = evalStaticRep MkTime

-- | Returns the local system time.
--
-- @since 0.1
getSystemTime :: (Time :> es) => Eff es LocalTime
getSystemTime = Local.zonedTimeToLocalTime <$> getSystemZonedTime

-- | Returns the zoned system time
--
-- @since 0.1
getSystemZonedTime :: (Time :> es) => Eff es ZonedTime
getSystemZonedTime = unsafeEff_ Local.getZonedTime

-- | Returns the zoned system time
--
-- @since 0.1
getMonotonicTime :: (Time :> es) => Eff es Double
getMonotonicTime = unsafeEff_ C.getMonotonicTime

-- | Runs an action, returning the elapsed time.
--
-- @since 0.1
withTiming :: (Time :> es) => Eff es a -> Eff es (TimeSpec, a)
withTiming m = do
  start <- getMonotonicTime
  res <- m
  end <- getMonotonicTime
  pure (TimeSpec.fromSeconds (end - start), res)

-- | 'withTiming' but ignores the result value.
--
-- @since 0.1
withTiming_ :: (Time :> es) => Eff es a -> Eff es TimeSpec
withTiming_ = fmap fst . withTiming

-- | Retrieves the formatted 'LocalTime'.
--
-- @since 0.1
getSystemTimeString :: (Time :> es) => Eff es String
getSystemTimeString = fmap Utils.formatLocalTime getSystemTime

-- | Retrieves the formatted 'ZonedTime'.
--
-- @since 0.1
getSystemZonedTimeString :: (Time :> es) => Eff es String
getSystemZonedTimeString = fmap Utils.formatZonedTime getSystemZonedTime
