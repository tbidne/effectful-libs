-- | Provides Optparse utils.
--
-- @since 0.1
module Effectful.Optparse.Utils
  ( OsPath,
    osPath,
    validOsPath,
  )
where

import FileSystem.OsPath (OsPath)
import FileSystem.OsPath qualified as OsPath
import Options.Applicative (ReadM)
import Options.Applicative qualified as OA

-- | 'OsPath' 'OA.Option' reader.
--
-- @since 0.1
osPath :: ReadM OsPath
osPath = OA.str >>= OsPath.encodeFail

-- | 'OsPath' 'OA.Option' reader. This includes validation i.e. fails if the
-- path is considered invalid on the given platform.
--
-- @since 0.1
validOsPath :: ReadM OsPath
validOsPath = OA.str >>= OsPath.encodeValidFail
