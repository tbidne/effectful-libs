{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a dynamic effect for "Data.Time".
--
-- @since 0.1
module Effectful.Time.Static
  ( -- * Class
    MonadTime (..),

    -- * Effect
    TimeStatic,
    getSystemTime,

    -- ** Handlers
    runTimeStaticIO,

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

-- | Static time effect.
--
-- @since 0.1
data TimeStatic :: Effect

type instance DispatchOf TimeStatic = Static WithSideEffects

data instance StaticRep TimeStatic = MkTimeStatic

-- | Runs a 'TimeStatic' effect in IO.
--
-- @since 0.1
runTimeStaticIO :: (IOE :> es) => Eff (TimeStatic : es) a -> Eff es a
runTimeStaticIO = evalStaticRep MkTimeStatic

-- | @since 0.1
instance (TimeStatic :> es) => MonadTime (Eff es) where
  getSystemZonedTime = unsafeEff_ Local.getZonedTime
  getMonotonicTime = unsafeEff_ C.getMonotonicTime

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
