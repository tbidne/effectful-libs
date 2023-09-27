-- | Provides a dynamic effect for "Data.Time".
--
-- @since 0.1
module Effectful.Time.Dynamic
  ( -- * Effect
    TimeDynamic (..),
    getSystemTime,
    getSystemZonedTime,
    getMonotonicTime,

    -- ** Handlers
    runTimeDynamicIO,

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

import Control.Monad.IO.Class (MonadIO (liftIO))
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
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Time.TimeSpec (TimeSpec (nsec, sec))
import Effectful.Time.TimeSpec qualified as TimeSpec
import Effectful.Time.Utils qualified as Utils
import GHC.Clock qualified as C

-- | Dynamic effect for "Data.Time".
--
-- @since 0.1
data TimeDynamic :: Effect where
  GetSystemZonedTime :: TimeDynamic m ZonedTime
  GetMonotonicTime :: TimeDynamic m Double

-- | @since 0.1
type instance DispatchOf TimeDynamic = Dynamic

-- | Runs 'TimeDynamic' in 'IO'.
--
-- @since 0.1
runTimeDynamicIO :: (IOE :> es) => Eff (TimeDynamic : es) a -> Eff es a
runTimeDynamicIO = interpret $ \_ -> \case
  GetSystemZonedTime -> liftIO Local.getZonedTime
  GetMonotonicTime -> liftIO C.getMonotonicTime

-- | Returns the local system time.
--
-- @since 0.1
getSystemTime :: (TimeDynamic :> es) => Eff es LocalTime
getSystemTime = Local.zonedTimeToLocalTime <$> getSystemZonedTime

-- | Returns the zoned system time
--
-- @since 0.1
getSystemZonedTime :: (TimeDynamic :> es) => Eff es ZonedTime
getSystemZonedTime = send GetSystemZonedTime

-- | Returns the zoned system time
--
-- @since 0.1
getMonotonicTime :: (TimeDynamic :> es) => Eff es Double
getMonotonicTime = send GetMonotonicTime

-- | Runs an action, returning the elapsed time.
--
-- @since 0.1
withTiming :: (TimeDynamic :> es) => Eff es a -> Eff es (TimeSpec, a)
withTiming m = do
  start <- getMonotonicTime
  res <- m
  end <- getMonotonicTime
  pure (TimeSpec.fromSeconds (end - start), res)

-- | 'withTiming' but ignores the result value.
--
-- @since 0.1
withTiming_ :: (TimeDynamic :> es) => Eff es a -> Eff es TimeSpec
withTiming_ = fmap fst . withTiming

-- | Retrieves the formatted 'LocalTime'.
--
-- @since 0.1
getSystemTimeString :: (TimeDynamic :> es) => Eff es String
getSystemTimeString = fmap Utils.formatLocalTime getSystemTime

-- | Retrieves the formatted 'ZonedTime'.
--
-- @since 0.1
getSystemZonedTimeString :: (TimeDynamic :> es) => Eff es String
getSystemZonedTimeString = fmap Utils.formatZonedTime getSystemZonedTime
