{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Utils
  ( strToPath,
    pathToStr,
  )
where

-- GHC does not consider these macros as usage, so it will think filepath
-- is an unneeded dependency. We thus add this unused import here so we don't
-- trip that warning.

import Effectful.FileSystem.Path (Path)
import System.FilePath (FilePath)
#if MIN_VERSION_filepath(1,4,100)
import System.OsPath qualified as FP

strToPath :: String -> Path
strToPath = FP.pack . fmap FP.unsafeFromChar

pathToStr :: Path -> String
pathToStr = fmap FP.toChar . FP.unpack
#else

strToPath :: String -> Path
strToPath = id

pathToStr :: Path -> String
pathToStr = id
#endif
