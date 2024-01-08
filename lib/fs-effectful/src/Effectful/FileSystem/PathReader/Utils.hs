-- | PathReader Utils
module Effectful.FileSystem.PathReader.Utils
  ( -- * Types
    PathType (..),
    PC.Utils.displayPathType,

    -- * Optics
    PC.Utils._PathTypeFile,
    PC.Utils._PathTypeDirectory,
    PC.Utils._PathTypeSymbolicLink,
    PC.Utils._PathTypeOther,
  )
where

import Effectful.PosixCompat.Utils
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeOther,
        PathTypeSymbolicLink
      ),
  )
import Effectful.PosixCompat.Utils qualified as PC.Utils
