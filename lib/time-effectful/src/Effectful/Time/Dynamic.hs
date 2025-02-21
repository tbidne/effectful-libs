-- | Provides a dynamic effect for "Data.Time".
--
-- @since 0.1
module Effectful.Time.Dynamic
  ( -- * Effect
    Time (..),
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
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret_, send)
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.Time.Static qualified as Static
import Effectful.Time.TimeSpec (TimeSpec (nsec, sec))
import Effectful.Time.TimeSpec qualified as TimeSpec
import Effectful.Time.Utils qualified as Utils

-- | Dynamic effect for "Data.Time".
--
-- @since 0.1
data Time :: Effect where
  GetSystemZonedTime :: Time m ZonedTime
  GetMonotonicTime :: Time m Double

-- | @since 0.1
type instance DispatchOf Time = Dynamic

-- | @since 0.1
instance ShowEffect Time where
  showEffectCons = \case
    GetSystemZonedTime -> "GetSystemZonedTime"
    GetMonotonicTime -> "GetMonotonicTime"

-- | Runs 'Time' in 'IO'.
--
-- @since 0.1
runTime :: (HasCallStack, IOE :> es) => Eff (Time : es) a -> Eff es a
runTime = reinterpret_ Static.runTime $ \case
  GetSystemZonedTime -> Static.getSystemZonedTime
  GetMonotonicTime -> Static.getMonotonicTime

-- | Returns the local system time.
--
-- @since 0.1
getSystemTime :: (HasCallStack, Time :> es) => Eff es LocalTime
getSystemTime = Local.zonedTimeToLocalTime <$> getSystemZonedTime

-- | Returns the zoned system time
--
-- @since 0.1
getSystemZonedTime :: (HasCallStack, Time :> es) => Eff es ZonedTime
getSystemZonedTime = send GetSystemZonedTime

-- | Returns the zoned system time
--
-- @since 0.1
getMonotonicTime :: (HasCallStack, Time :> es) => Eff es Double
getMonotonicTime = send GetMonotonicTime

-- | Runs an action, returning the elapsed time.
--
-- @since 0.1
withTiming :: (HasCallStack, Time :> es) => Eff es a -> Eff es (TimeSpec, a)
withTiming m = do
  start <- getMonotonicTime
  res <- m
  end <- getMonotonicTime
  pure (TimeSpec.fromSeconds (end - start), res)

-- | 'withTiming' but ignores the result value.
--
-- @since 0.1
withTiming_ :: (HasCallStack, Time :> es) => Eff es a -> Eff es TimeSpec
withTiming_ = fmap fst . withTiming

-- | Retrieves the formatted 'LocalTime'.
--
-- @since 0.1
getSystemTimeString :: (HasCallStack, Time :> es) => Eff es String
getSystemTimeString = fmap Utils.formatLocalTime getSystemTime

-- | Retrieves the formatted 'ZonedTime'.
--
-- @since 0.1
getSystemZonedTimeString :: (HasCallStack, Time :> es) => Eff es String
getSystemZonedTimeString = fmap Utils.formatZonedTime getSystemZonedTime
