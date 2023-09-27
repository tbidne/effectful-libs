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

    -- * Exceptions
    PathExistsException (..),
    PathDoesNotExistException (..),

    -- * Optics
    _OverwriteNone,
    _OverwriteDirectories,
    _OverwriteAll,
    _TargetNameSrc,
    _TargetNameLiteral,
    _TargetNameDest,

    -- * Copying Utils
    Handle (..),
    copyDirectoryRecursiveConfig,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (unless, when)
import Data.Foldable (for_, traverse_)
import Data.IORef (IORef)
import Effectful.Exception
  ( Exception (displayException),
    MonadMask,
    mask_,
    onException,
    throwM,
  )
import Effectful.FileSystem.Utils (OsPath, (</>))
import GHC.Generics (Generic)
import Optics.Core
  ( A_Lens,
    LabelOptic (labelOptic),
    Prism',
    lensVL,
    prism,
    (^.),
  )
import System.OsPath qualified as FP

-- | Exception for trying to create a path that already exists.
--
-- @since 0.1
newtype PathExistsException = MkPathExistsException OsPath
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception PathExistsException where
  displayException (MkPathExistsException path) =
    "Path already exists: " <> pathToStr path

-- | Exception for when a path does not exist.
--
-- @since 0.1
newtype PathDoesNotExistException = MkPathDoesNotExistException OsPath
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception PathDoesNotExistException where
  displayException (MkPathDoesNotExistException path) =
    "Path does not exist: " <> pathToStr path

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
  labelOptic = lensVL $ \f (MkCopyDirConfig _overwrite _targetName) ->
    fmap (`MkCopyDirConfig` _targetName) (f _overwrite)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ TargetName, b ~ TargetName) =>
  LabelOptic "targetName" k CopyDirConfig CopyDirConfig a b
  where
  labelOptic = lensVL $ \f (MkCopyDirConfig _overwrite _targetName) ->
    fmap (MkCopyDirConfig _overwrite) (f _targetName)
  {-# INLINE labelOptic #-}

-- | Default config for copying directories.
--
-- >>> defaultCopyDirConfig
-- MkCopyDirConfig {overwrite = OverwriteNone, destName = TargetNameSrc}
--
-- @since 0.1
defaultCopyDirConfig :: CopyDirConfig
defaultCopyDirConfig = MkCopyDirConfig OverwriteNone TargetNameSrc

-- | Internal handler for path writing effects. Used to implement recursive
-- directory copying for both static and dynamic effects.
--
-- @since 0.1
data Handle m = MkHandle
  { newIORef :: forall a. a -> m (IORef a),
    readIORef :: forall a. IORef a -> m a,
    modifyIORef' :: forall a. IORef a -> (a -> a) -> m (),
    doesDirectoryExist :: OsPath -> m Bool,
    doesFileExist :: OsPath -> m Bool,
    listDirectoryRecursive :: OsPath -> m ([OsPath], [OsPath]),
    createDirectory :: OsPath -> m (),
    createDirectoryIfMissing :: Bool -> OsPath -> m (),
    copyFileWithMetadata :: OsPath -> OsPath -> m (),
    removeFile :: OsPath -> m (),
    removeDirectory :: OsPath -> m (),
    removeDirectoryRecursive :: OsPath -> m ()
  }

-- | Internal copying function in terms of explicit handler.
--
-- @since 0.1
copyDirectoryRecursiveConfig ::
  (MonadMask m) =>
  -- | Handler
  Handle m ->
  -- | Config
  CopyDirConfig ->
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  m ()
copyDirectoryRecursiveConfig
  handlers@MkHandle {doesDirectoryExist}
  config
  src
  destRoot = do
    destExists <- doesDirectoryExist destRoot

    unless destExists $
      throwM $
        MkPathDoesNotExistException destRoot

    -- NOTE: Use the given name if it exists. Otherwise use the source folder's
    -- name.
    let dest = case config ^. #targetName of
          -- Use source directory's name
          TargetNameSrc ->
            destRoot </> FP.takeBaseName (FP.dropTrailingPathSeparator src)
          -- Use the give name
          TargetNameLiteral p -> destRoot </> p
          -- Use dest itself (i.e. top-level copy)
          TargetNameDest -> destRoot

    case config ^. #overwrite of
      OverwriteNone -> copyDirectoryNoOverwrite handlers src dest
      OverwriteDirectories -> copyDirectoryOverwrite handlers False src dest
      OverwriteAll -> copyDirectoryOverwrite handlers True src dest
{-# INLINEABLE copyDirectoryRecursiveConfig #-}

copyDirectoryOverwrite ::
  (MonadMask m) =>
  Handle m ->
  -- | Overwrite files
  Bool ->
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  m ()
copyDirectoryOverwrite
  MkHandle
    { newIORef,
      readIORef,
      modifyIORef',
      doesDirectoryExist,
      doesFileExist,
      listDirectoryRecursive,
      createDirectory,
      createDirectoryIfMissing,
      copyFileWithMetadata,
      removeFile,
      removeDirectory,
      removeDirectoryRecursive
    }
  overwriteFiles
  src
  dest = do
    -- NOTE: The logic here merits explanation. The idea is if we encounter
    -- any errors while copying, we want to "roll back" any successful copies
    -- i.e. copying should try to be atomic.
    --
    -- In copyDirectory this is simple; we can assume the dest/\<src\> does not
    -- exist (otherwise throwing an exception), so the logic is:
    --
    -- 1. Copying: use (createDirectoryIfMissing True) to create the
    --   necessary parent dirs automatically.
    -- 2. Cleanup: If anything goes wrong, delete the entire dest/\<src\>.
    --
    -- For copyDirectoryOverwrite, however, the dest/\<src\> might already exist,
    -- making our job harder. In particular:
    --
    -- 1. Copying:
    --      - Create the parent directories sequentially. We store the
    --        created paths in an IORef.
    --      - Copy the files over, saving the copied paths to another IORef.
    -- 2. Cleanup:
    --      - If anything goes wrong, we cannot simply delete the dest/\<src\>
    --        because it might have already existed. We iterate through our
    --        IORefs, deleting the paths.

    copiedFilesRef <- newIORef []
    createdDirsRef <- newIORef []

    destExists <- doesDirectoryExist dest

    let checkOverwrites =
          if not overwriteFiles
            then \f -> do
              exists <- doesFileExist f
              when exists $
                throwM $
                  MkPathExistsException f
            else const (pure ())

        copyFiles = do
          (subFiles, subDirs) <- listDirectoryRecursive src

          -- create dest if it does not exist
          unless destExists $ createDirectory dest

          -- create the parent directories
          for_ subDirs $ \d -> do
            let d' = dest </> d
            dExists <- doesDirectoryExist d'
            unless dExists $ do
              createDirectoryIfMissing False d'
              modifyIORef' createdDirsRef (d' :)

          -- copy files
          for_ subFiles $ \f -> do
            let f' = dest </> f
            checkOverwrites f'
            copyFileWithMetadata (src </> f) f'
            modifyIORef' copiedFilesRef (f' :)

        cleanup =
          if destExists
            then do
              -- manually delete files and dirs
              readIORef copiedFilesRef >>= traverse_ removeFile
              readIORef createdDirsRef >>= traverse_ removeDirectory
            else removeDirectoryRecursive dest

    copyFiles `onException` mask_ cleanup
{-# INLINEABLE copyDirectoryOverwrite #-}

copyDirectoryNoOverwrite ::
  (MonadMask m) =>
  Handle m ->
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  m ()
copyDirectoryNoOverwrite
  MkHandle
    { doesDirectoryExist,
      listDirectoryRecursive,
      createDirectory,
      createDirectoryIfMissing,
      copyFileWithMetadata,
      removeDirectoryRecursive
    }
  src
  dest = do
    destExists <- doesDirectoryExist dest
    when destExists $ throwM (MkPathExistsException dest)

    let copyFiles = do
          (subFiles, subDirs) <- listDirectoryRecursive src
          createDirectory dest

          -- create intermediate dirs if they do not exist
          traverse_ (createDirectoryIfMissing True . (dest </>)) subDirs

          -- copy files
          for_ subFiles $ \f -> copyFileWithMetadata (src </> f) (dest </> f)

        -- delete directory
        cleanup = removeDirectoryRecursive dest

    copyFiles `onException` mask_ cleanup
{-# INLINEABLE copyDirectoryNoOverwrite #-}

pathToStr :: OsPath -> String
pathToStr = fmap FP.toChar . FP.unpack
