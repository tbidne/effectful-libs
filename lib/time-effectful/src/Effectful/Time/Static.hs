{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a dynamic effect for "Data.Time".
--
-- @since 0.1
module Effectful.Time.Static
  ( -- * Effect
    Time,
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
import Data.Time.Zones qualified as TZ
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( HasCallStack,
    SideEffects (WithSideEffects),
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
runTime :: (HasCallStack, IOE :> es) => Eff (Time : es) a -> Eff es a
runTime = evalStaticRep MkTime

-- | Returns the local system time.
--
-- @since 0.1
getSystemTime :: (HasCallStack, Time :> es) => Eff es LocalTime
getSystemTime = Local.zonedTimeToLocalTime <$> getSystemZonedTime

-- | Returns the zoned system time.
--
-- @since 0.1
getSystemZonedTime :: (HasCallStack, Time :> es) => Eff es ZonedTime
getSystemZonedTime = unsafeEff_ Local.getZonedTime

-- | Lifted 'Local.getTimeZone'.
--
-- @since 0.1
getTimeZone :: (HasCallStack, Time :> es) => UTCTime -> Eff es TimeZone
getTimeZone = unsafeEff_ . Local.getTimeZone

-- | Lifted 'Local.utcToLocalZonedTime'.
--
-- @since 0.1
utcToLocalZonedTime :: (HasCallStack, Time :> es) => UTCTime -> Eff es ZonedTime
utcToLocalZonedTime = unsafeEff_ . Local.utcToLocalZonedTime

-- | Lifted 'TZ.loadLocalTZ'.
--
-- @since 0.1
loadLocalTZ :: (HasCallStack, Time :> es) => Eff es TZ
loadLocalTZ = unsafeEff_ TZ.loadLocalTZ

-- | Returns the zoned system time
--
-- @since 0.1
getMonotonicTime :: (HasCallStack, Time :> es) => Eff es Double
getMonotonicTime = unsafeEff_ C.getMonotonicTime

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
