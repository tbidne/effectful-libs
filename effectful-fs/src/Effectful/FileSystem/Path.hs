{-# LANGUAGE CPP #-}

-- | Provides compatibility for filepaths.
--
-- @since 0.1
module Effectful.FileSystem.Path
  ( -- * Path
    Path,
    (</>),
  )
where

import Effectful.FileSystem.Internal (Path, (</>))
