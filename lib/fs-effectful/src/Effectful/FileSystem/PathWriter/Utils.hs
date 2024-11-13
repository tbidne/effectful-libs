{-# LANGUAGE UndecidableInstances #-}

-- | Provides utilities used by path writing.
--
-- @since 0.1
module Effectful.FileSystem.PathWriter.Utils
  ( -- * Config
    CopyDirConfig (..),
    defaultCopyDirConfig,
    Overwrite (..),
    TargetName (..),

    -- * Optics
    _OverwriteNone,
    _OverwriteDirectories,
    _OverwriteAll,
    _TargetNameSrc,
    _TargetNameLiteral,
    _TargetNameDest,
  )
where

import Control.DeepSeq (NFData)
import FileSystem.OsPath (OsPath)
import GHC.Generics (Generic)
import Optics.Core
  ( A_Lens,
    LabelOptic (labelOptic),
    Prism',
    lensVL,
    prism,
  )

-- | Determines file/directory overwrite behavior.
--
-- @since 0.1
data Overwrite
  = -- | No overwriting allowed.
    --
    -- @since 0.1
    OverwriteNone
  | -- | Allow overwriting directories.
    --
    -- @since 0.1
    OverwriteDirectories
  | -- | Allow overwriting the target directory and all subpaths.
    --
    -- @since 0.1
    OverwriteAll
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_OverwriteNone :: Prism' Overwrite ()
_OverwriteNone =
  prism
    (\() -> OverwriteNone)
    ( \x -> case x of
        OverwriteNone -> Right ()
        _ -> Left x
    )
{-# INLINE _OverwriteNone #-}

-- | @since 0.1
_OverwriteDirectories :: Prism' Overwrite ()
_OverwriteDirectories =
  prism
    (\() -> OverwriteDirectories)
    ( \x -> case x of
        OverwriteDirectories -> Right ()
        _ -> Left x
    )
{-# INLINE _OverwriteDirectories #-}

-- | @since 0.1
_OverwriteAll :: Prism' Overwrite ()
_OverwriteAll =
  prism
    (\() -> OverwriteAll)
    ( \x -> case x of
        OverwriteAll -> Right ()
        _ -> Left x
    )
{-# INLINE _OverwriteAll #-}

-- | Determines how to name the target.
--
-- @since 0.1
data TargetName
  = -- | Uses the src dir as the dest name i.e. @dest/\<src\>@.
    --
    -- @since 0.1
    TargetNameSrc
  | -- | Uses the given literal as the dest name i.e. @dest/\<targetName\>@.
    --
    -- @since 0.1
    TargetNameLiteral !OsPath
  | -- | Uses dest itself as the target i.e. @dest/@ (top-level copy).
    --
    -- @since 0.1
    TargetNameDest
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_TargetNameSrc :: Prism' TargetName ()
_TargetNameSrc =
  prism
    (\() -> TargetNameSrc)
    ( \x -> case x of
        TargetNameSrc -> Right ()
        _ -> Left x
    )
{-# INLINE _TargetNameSrc #-}

-- | @since 0.1
_TargetNameLiteral :: Prism' TargetName OsPath
_TargetNameLiteral =
  prism
    TargetNameLiteral
    ( \x -> case x of
        TargetNameLiteral p -> Right p
        _ -> Left x
    )
{-# INLINE _TargetNameLiteral #-}

-- | @since 0.1
_TargetNameDest :: Prism' TargetName ()
_TargetNameDest =
  prism
    (\() -> TargetNameDest)
    ( \x -> case x of
        TargetNameDest -> Right ()
        _ -> Left x
    )
{-# INLINE _TargetNameDest #-}

-- | Directory copying config.
--
-- @since 0.1
data CopyDirConfig = MkCopyDirConfig
  { -- | Overwrite behavior.
    --
    -- @since 0.1
    overwrite :: !Overwrite,
    -- | TargetName behavior.
    --
    -- @since 0.1
    targetName :: !TargetName
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Overwrite, b ~ Overwrite) =>
  LabelOptic "overwrite" k CopyDirConfig CopyDirConfig a b
  where
  labelOptic = lensVL $ \f (MkCopyDirConfig a1 a2) ->
    fmap (\b -> MkCopyDirConfig b a2) (f a1)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ TargetName, b ~ TargetName) =>
  LabelOptic "targetName" k CopyDirConfig CopyDirConfig a b
  where
  labelOptic = lensVL $ \f (MkCopyDirConfig a1 a2) ->
    fmap (\b -> MkCopyDirConfig a1 b) (f a2)
  {-# INLINE labelOptic #-}

-- | Default config for copying directories.
--
-- >>> defaultCopyDirConfig
-- MkCopyDirConfig {overwrite = OverwriteNone, destName = TargetNameSrc}
--
-- @since 0.1
defaultCopyDirConfig :: CopyDirConfig
defaultCopyDirConfig = MkCopyDirConfig OverwriteNone TargetNameSrc
