-- | Provides a dynamic effect for "Data.Time".
--
-- @since 0.1
module Effectful.Time.Dynamic
  ( -- * Effect
    Time (..),
    getSystemTime,
    getSystemZonedTime,
    getTimeZone,
    utcToLocalZonedTime,
    loadLocalTZ,
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

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime
  ( LocalTime (LocalTime, localDay, localTimeOfDay),
    TimeZone,
    ZonedTime (ZonedTime, zonedTimeToLocalTime, zonedTimeZone),
  )
import Data.Time.LocalTime qualified as Local
import Data.Time.Zones (TZ)
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
  GetTimeZone :: UTCTime -> Time m TimeZone
  UtcToLocalZonedTime :: UTCTime -> Time m ZonedTime
  LoadLocalTZ :: Time m TZ
  GetMonotonicTime :: Time m Double

-- | @since 0.1
type instance DispatchOf Time = Dynamic

-- | @since 0.1
instance ShowEffect Time where
  showEffectCons = \case
    GetSystemZonedTime -> "GetSystemZonedTime"
    GetTimeZone _ -> "GetTimeZone"
    UtcToLocalZonedTime _ -> "UtcToLocalZonedTime"
    LoadLocalTZ -> "LoadLocalTZ"
    GetMonotonicTime -> "GetMonotonicTime"

-- | Runs 'Time' in 'IO'.
--
-- @since 0.1
runTime :: (HasCallStack, IOE :> es) => Eff (Time : es) a -> Eff es a
runTime = reinterpret_ Static.runTime $ \case
  GetSystemZonedTime -> Static.getSystemZonedTime
  GetTimeZone utc -> Static.getTimeZone utc
  UtcToLocalZonedTime utc -> Static.utcToLocalZonedTime utc
  LoadLocalTZ -> Static.loadLocalTZ
  GetMonotonicTime -> Static.getMonotonicTime

-- | Returns the local system time.
--
-- @since 0.1
getSystemTime :: (HasCallStack, Time :> es) => Eff es LocalTime
getSystemTime = Local.zonedTimeToLocalTime <$> getSystemZonedTime

-- | Returns the zoned system time.
--
-- @since 0.1
getSystemZonedTime :: (HasCallStack, Time :> es) => Eff es ZonedTime
getSystemZonedTime = send GetSystemZonedTime

-- | Lifted 'Local.getTimeZone'.
--
-- @since 0.1
getTimeZone :: (HasCallStack, Time :> es) => UTCTime -> Eff es TimeZone
getTimeZone = send . GetTimeZone

-- | Lifted 'Local.utcToLocalZonedTime'.
--
-- @since 0.1
utcToLocalZonedTime :: (HasCallStack, Time :> es) => UTCTime -> Eff es ZonedTime
utcToLocalZonedTime = send . UtcToLocalZonedTime

-- | Lifted 'TZ.loadLocalTZ'.
--
-- @since 0.1
loadLocalTZ :: (HasCallStack, Time :> es) => Eff es TZ
loadLocalTZ = send LoadLocalTZ

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

-- | Retrieves the formatted 'Data.Time.LocalTime'.
--
-- @since 0.1
getSystemTimeString :: (HasCallStack, Time :> es) => Eff es String
getSystemTimeString = fmap Utils.formatLocalTime getSystemTime

-- | Retrieves the formatted 'Data.Time.ZonedTime'.
--
-- @since 0.1
getSystemZonedTimeString :: (HasCallStack, Time :> es) => Eff es String
getSystemZonedTimeString = fmap Utils.formatZonedTime getSystemZonedTime
