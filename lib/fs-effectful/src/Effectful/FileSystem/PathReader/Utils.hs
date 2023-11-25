-- | PathReader Utils
module Effectful.FileSystem.PathReader.Utils
  ( -- * Types
    PathType (..),
    displayPathType,

    -- * Optics
    _PathTypeFile,
    _PathTypeDirectory,
    _PathTypeSymbolicLink,
  )
where

import Control.DeepSeq (NFData)
import Data.String (IsString)
import GHC.Generics (Generic)
import Optics.Core (Prism', prism)

-- | Path type.
-- @since 0.1
data PathType
  = -- | @since 0.1
    PathTypeFile
  | -- | @since 0.1
    PathTypeDirectory
  | -- | @since 0.1
    PathTypeSymbolicLink
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_PathTypeFile :: Prism' PathType ()
_PathTypeFile =
  prism
    (const PathTypeFile)
    ( \case
        PathTypeFile -> Right ()
        x -> Left x
    )
{-# INLINE _PathTypeFile #-}

-- | @since 0.1
_PathTypeDirectory :: Prism' PathType ()
_PathTypeDirectory =
  prism
    (const PathTypeDirectory)
    ( \case
        PathTypeDirectory -> Right ()
        x -> Left x
    )
{-# INLINE _PathTypeDirectory #-}

-- | @since 0.1
_PathTypeSymbolicLink :: Prism' PathType ()
_PathTypeSymbolicLink =
  prism
    (const PathTypeSymbolicLink)
    ( \case
        PathTypeSymbolicLink -> Right ()
        x -> Left x
    )
{-# INLINE _PathTypeSymbolicLink #-}

-- | String representation of 'PathType'.
--
-- @since 0.1
displayPathType :: (IsString a) => PathType -> a
displayPathType PathTypeFile = "file"
displayPathType PathTypeDirectory = "directory"
displayPathType PathTypeSymbolicLink = "symlink"
