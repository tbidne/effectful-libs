-- | Provides Optparse utils.
--
-- @since 0.1
module Effectful.Optparse.Utils
  ( OsPath,
    osPath,
    validOsPath,
  )
where

import Control.Exception (Exception (displayException))
import Effectful.FileSystem.Utils (OsPath, encodeFpToOs, encodeFpToValidOs)
import Options.Applicative (ReadM)
import Options.Applicative qualified as OA

-- | 'OsPath' 'OA.Option' reader.
--
-- @since 0.1
osPath :: ReadM OsPath
osPath = do
  pathStr <- OA.str
  case encodeFpToOs pathStr of
    Right p -> pure p
    Left ex -> fail $ "Error encoding string path: " ++ displayException ex

-- | 'OsPath' 'OA.Option' reader. This includes validation i.e. fails if the
-- path is considered invalid on the given platform.
--
-- @since 0.1
validOsPath :: ReadM OsPath
validOsPath = do
  pathStr <- OA.str
  case encodeFpToValidOs pathStr of
    Right p -> pure p
    Left ex -> fail $ "Error encoding string path: " ++ displayException ex
