{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Time utils.
--
-- @since 0.1
module Effectful.Time.Utils
  ( formatLocalTime,
    formatZonedTime,
    parseLocalTime,
    parseZonedTime,
  )
where

import Data.Time.Format qualified as Format
import Data.Time.LocalTime (LocalTime, ZonedTime)
import Effectful.Dispatch.Dynamic (HasCallStack)

-- | Formats the 'LocalTime' to @YYYY-MM-DD HH:MM:SS@.
--
-- @since 0.1
formatLocalTime :: LocalTime -> String
formatLocalTime = Format.formatTime Format.defaultTimeLocale localTimeFormat

-- | Formats the 'ZonedTime' to @YYYY-MM-DD HH:MM:SS Z@.
--
-- @since 0.1
formatZonedTime :: ZonedTime -> String
formatZonedTime = Format.formatTime Format.defaultTimeLocale zonedTimeFormat

-- | Parses the 'LocalTime' from @YYYY-MM-DD HH:MM:SS@. If the 'MonadFail'
-- instance throws an 'Control.Exception.Exception' consider
-- 'parseLocalTimeCallStack'.
--
-- @since 0.1
parseLocalTime :: (HasCallStack, MonadFail f) => String -> f LocalTime
parseLocalTime =
  Format.parseTimeM
    True
    Format.defaultTimeLocale
    localTimeFormat

-- | Parses the 'ZonedTime' from @YYYY-MM-DD HH:MM:SS Z@. If the 'MonadFail'
-- instance throws an 'Control.Exception.Exception' consider
-- 'parseZonedTimeCallStack'.
--
-- ==== __Known Timezones__
--
-- * UTC
-- * UT
-- * GMT
-- * EST
-- * EDT
-- * CST
-- * CDT
-- * MST
-- * MDT
-- * PST
-- * PDT
-- * +HHMM (e.g. +1300)
--
-- @since 0.1
parseZonedTime :: (HasCallStack, MonadFail f) => String -> f ZonedTime
parseZonedTime =
  Format.parseTimeM
    True
    Format.defaultTimeLocale
    zonedTimeFormat

localTimeFormat :: String
localTimeFormat = "%0Y-%m-%d %H:%M:%S"

zonedTimeFormat :: String
zonedTimeFormat = "%0Y-%m-%d %H:%M:%S %Z"
