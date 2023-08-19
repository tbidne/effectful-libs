-- | Provides Optparse utils.
--
-- @since 0.1
module Effectful.Optparse.Utils
  ( OsPath,
    osPath,
  )
where

import Control.Exception (Exception (displayException))
import Effectful.FileSystem.Utils (OsPath, toOsPath)
import Options.Applicative (ReadM)
import Options.Applicative qualified as OA

-- | 'OsPath' 'OA.Option' reader.
--
-- @since 0.1
osPath :: ReadM OsPath
osPath = do
  pathStr <- OA.str
  case toOsPath pathStr of
    Right p -> pure p
    Left ex -> fail $ "Error encoding string path: " ++ displayException ex
