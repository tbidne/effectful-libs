-- | Provides Optparse utils.
--
-- @since 0.1
module Effectful.Optparse.Utils
  ( OsPath,
    osPath,
  )
where

import Control.Exception (Exception (displayException))
import Options.Applicative (ReadM)
import Options.Applicative qualified as OA
import System.IO qualified as IO
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

-- | 'OsPath' 'OA.Option' reader.
--
-- @since 0.1
osPath :: ReadM OsPath
osPath = do
  pathStr <- OA.str
  case OsPath.encodeWith IO.utf16le IO.utf8 pathStr of
    Right p -> pure p
    Left ex -> fail $ "Error encoding string path: " ++ displayException ex
