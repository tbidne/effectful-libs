{-# LANGUAGE UndecidableInstances #-}

-- | Provides a dynamic effect for "Data.Time".
--
-- @since 0.1
module Effectful.Time.Dynamic
  ( -- * Class
    MonadTime (..),

    -- * Effect
    TimeDynamic (..),
    getSystemTime,

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

-- | Time effect.
--
-- @since 0.1
class (Monad m) => MonadTime m where
  -- | Returns the zoned system time.
  --
  -- @since 0.1
  getSystemZonedTime :: m ZonedTime

  -- | Return monotonic time in seconds, since some unspecified starting
  -- point.
  --
  -- @since 0.1
  getMonotonicTime :: m Double

-- | @since 0.1
instance MonadTime IO where
  getSystemZonedTime = Local.getZonedTime
  getMonotonicTime = C.getMonotonicTime

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

-- | @since 0.1
instance (TimeDynamic :> es) => MonadTime (Eff es) where
  getSystemZonedTime = send GetSystemZonedTime
  getMonotonicTime = send GetMonotonicTime

-- | Returns the local system time.
--
-- @since 0.1
getSystemTime :: (MonadTime m) => m LocalTime
getSystemTime = Local.zonedTimeToLocalTime <$> getSystemZonedTime

-- | Runs an action, returning the elapsed time.
--
-- @since 0.1
withTiming :: (MonadTime m) => m a -> m (TimeSpec, a)
withTiming m = do
  start <- getMonotonicTime
  res <- m
  end <- getMonotonicTime
  pure (TimeSpec.fromSeconds (end - start), res)

-- | 'withTiming' but ignores the result value.
--
-- @since 0.1
withTiming_ :: (MonadTime m) => m a -> m TimeSpec
withTiming_ = fmap fst . withTiming

-- | Retrieves the formatted 'LocalTime'.
--
-- @since 0.1
getSystemTimeString :: (MonadTime m) => m String
getSystemTimeString = fmap Utils.formatLocalTime getSystemTime

-- | Retrieves the formatted 'ZonedTime'.
--
-- @since 0.1
getSystemZonedTimeString :: (MonadTime m) => m String
getSystemZonedTimeString = fmap Utils.formatZonedTime getSystemZonedTime
