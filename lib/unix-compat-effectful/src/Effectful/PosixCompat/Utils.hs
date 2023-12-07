{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.PosixCompat.Utils
  ( -- * PathType
    PathType (..),
    displayPathType,

    -- ** Optics
    _PathTypeFile,
    _PathTypeDirectory,
    _PathTypeSymbolicLink,

    -- * Utils
    throwPathIOError,
  )
where

import Control.DeepSeq (NFData)
import Data.String (IsString)
import Effectful.Exception (HasCallStack, MonadThrow, throwM)
import GHC.Generics (Generic)
import GHC.IO.Exception
  ( IOErrorType,
    IOException
      ( IOError,
        ioe_description,
        ioe_errno,
        ioe_filename,
        ioe_handle,
        ioe_location,
        ioe_type
      ),
  )
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

-- | Helper for throwing 'IOException'.
--
-- @since 0.1
throwPathIOError ::
  (HasCallStack, MonadThrow m) =>
  -- | Path upon which the IO operation failed.
  FilePath ->
  -- | String location (e.g. function name).
  String ->
  -- | Type of exception.
  IOErrorType ->
  -- | Description.
  String ->
  m a
throwPathIOError path loc ty desc =
  throwM $
    IOError
      { ioe_handle = Nothing,
        ioe_type = ty,
        ioe_location = loc,
        ioe_description = desc,
        ioe_errno = Nothing,
        ioe_filename = Just path
      }
{-# INLINEABLE throwPathIOError #-}
