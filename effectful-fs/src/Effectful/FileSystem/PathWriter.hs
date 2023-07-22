{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides an effect for writing paths.
--
-- @since 0.1
module Effectful.FileSystem.PathWriter
  ( -- * Effect
    PathWriterEffect (..),
    Path,
    createDirectory,
    createDirectoryIfMissing,
    removeDirectory,
    removeDirectoryRecursive,
    removePathForcibly,
    renameDirectory,
    setCurrentDirectory,
    withCurrentDirectory,
    removeFile,
    renameFile,
    renamePath,
    copyFile,
    copyFileWithMetadata,
    createFileLink,
    createDirectoryLink,
    removeDirectoryLink,
    setPermissions,
    copyPermissions,
    setAccessTime,
    setModificationTime,

    -- ** Handlers
    runPathWriterIO,

    -- * Copying

    -- ** Config
    CopyDirConfig (..),
    Overwrite (..),
    TargetName (..),
    defaultCopyDirConfig,

    -- ** Functions
    copyDirectoryRecursive,
    copyDirectoryRecursiveConfig,

    -- ** Optics
    _OverwriteNone,
    _OverwriteDirectories,
    _OverwriteAll,
    _TargetNameSrc,
    _TargetNameLiteral,
    _TargetNameDest,

    -- * Removing
    removeFileIfExists,
    removeDirectoryIfExists,
    removeDirectoryRecursiveIfExists,
    removePathForciblyIfExists,

    -- * Exceptions
    PathExistsException (..),
    PathDoesNotExistException (..),

    -- * Re-exports
    Permissions (..),
    UTCTime (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_, traverse_)
import Data.Time (UTCTime (..))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.Exception
  ( Exception (displayException),
    mask_,
    onException,
    throwM,
  )
import Effectful.FileSystem.Path (Path, (</>))
import Effectful.FileSystem.PathReader
  ( PathReaderEffect,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    listDirectoryRecursive,
  )
import Effectful.IORef (IORefEffect, modifyIORef', newIORef, readIORef)
import GHC.Generics (Generic)
import Optics.Core
  ( A_Lens,
    LabelOptic (labelOptic),
    Prism',
    lensVL,
    prism,
    (^.),
  )
import System.Directory (Permissions (..))
#if MIN_VERSION_filepath(1,4,100) && MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath qualified as Dir
import System.OsPath qualified as FP
#else
import System.Directory qualified as Dir
import System.FilePath qualified as FP
#endif

-- | Effect for writing paths.
--
-- @since 0.1
data PathWriterEffect :: Effect where
  CreateDirectory :: Path -> PathWriterEffect m ()
  CreateDirectoryIfMissing :: Bool -> Path -> PathWriterEffect m ()
  RemoveDirectory :: Path -> PathWriterEffect m ()
  RemoveDirectoryRecursive :: Path -> PathWriterEffect m ()
  RemovePathForcibly :: Path -> PathWriterEffect m ()
  RenameDirectory :: Path -> Path -> PathWriterEffect m ()
  SetCurrentDirectory :: Path -> PathWriterEffect m ()
  WithCurrentDirectory :: Path -> m a -> PathWriterEffect m a
  RemoveFile :: Path -> PathWriterEffect m ()
  RenameFile :: Path -> Path -> PathWriterEffect m ()
  RenamePath :: Path -> Path -> PathWriterEffect m ()
  CopyFile :: Path -> Path -> PathWriterEffect m ()
  CopyFileWithMetadata :: Path -> Path -> PathWriterEffect m ()
  CreateFileLink :: Path -> Path -> PathWriterEffect m ()
  CreateDirectoryLink :: Path -> Path -> PathWriterEffect m ()
  RemoveDirectoryLink :: Path -> PathWriterEffect m ()
  SetPermissions :: Path -> Permissions -> PathWriterEffect m ()
  CopyPermissions :: Path -> Path -> PathWriterEffect m ()
  SetAccessTime :: Path -> UTCTime -> PathWriterEffect m ()
  SetModificationTime :: Path -> UTCTime -> PathWriterEffect m ()

-- | @since 0.1
type instance DispatchOf PathWriterEffect = Dynamic

-- | Runs 'PathWriterEffect' in 'IO'.
--
-- @since 0.1
runPathWriterIO ::
  ( IOE :> es
  ) =>
  Eff (PathWriterEffect : es) a ->
  Eff es a
runPathWriterIO = interpret $ \env -> \case
  CreateDirectory p -> liftIO $ Dir.createDirectory p
  CreateDirectoryIfMissing b p -> liftIO $ Dir.createDirectoryIfMissing b p
  RemoveDirectory p -> liftIO $ Dir.removeDirectory p
  RemoveDirectoryRecursive p -> liftIO $ Dir.removeDirectoryRecursive p
  RemovePathForcibly p -> liftIO $ Dir.removePathForcibly p
  RenameDirectory p p' -> liftIO $ Dir.renameDirectory p p'
  SetCurrentDirectory p -> liftIO $ Dir.setCurrentDirectory p
  WithCurrentDirectory p m -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Dir.withCurrentDirectory p (runInIO m)
  RemoveFile p -> liftIO $ Dir.removeFile p
  RenameFile p p' -> liftIO $ Dir.renameFile p p'
  RenamePath p p' -> liftIO $ Dir.renamePath p p'
  CopyFile p p' -> liftIO $ Dir.copyFile p p'
  CopyFileWithMetadata p p' -> liftIO $ Dir.copyFileWithMetadata p p'
  CreateFileLink p p' -> liftIO $ Dir.createFileLink p p'
  CreateDirectoryLink p p' -> liftIO $ Dir.createDirectoryLink p p'
  RemoveDirectoryLink p -> liftIO $ Dir.removeDirectoryLink p
  SetPermissions p ps -> liftIO $ Dir.setPermissions p ps
  CopyPermissions p ps -> liftIO $ Dir.copyPermissions p ps
  SetAccessTime p t -> liftIO $ Dir.setAccessTime p t
  SetModificationTime p t -> liftIO $ Dir.setModificationTime p t

-- | @since 0.1
createDirectory ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
createDirectory = send . CreateDirectory

-- | @since 0.1
createDirectoryIfMissing ::
  ( PathWriterEffect :> es
  ) =>
  Bool ->
  Path ->
  Eff es ()
createDirectoryIfMissing b = send . CreateDirectoryIfMissing b

-- | @since 0.1
removeDirectory ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeDirectory = send . RemoveDirectory

-- | @since 0.1
removeDirectoryRecursive ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeDirectoryRecursive = send . RemoveDirectoryRecursive

-- | @since 0.1
removePathForcibly ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removePathForcibly = send . RemovePathForcibly

-- | @since 0.1
renameDirectory ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
renameDirectory p = send . RenameDirectory p

-- | @since 0.1
setCurrentDirectory ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
setCurrentDirectory = send . SetCurrentDirectory

-- | @since 0.1
withCurrentDirectory ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Eff es a ->
  Eff es a
withCurrentDirectory p = send . WithCurrentDirectory p

-- | @since 0.1
removeFile ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeFile = send . RemoveFile

-- | @since 0.1
renameFile ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
renameFile p = send . RenameFile p

-- | @since 0.1
renamePath ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
renamePath p = send . RenamePath p

-- | @since 0.1
copyFile ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
copyFile p = send . CopyFile p

-- | @since 0.1
copyFileWithMetadata ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
copyFileWithMetadata p = send . CopyFileWithMetadata p

-- | @since 0.1
createFileLink ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
createFileLink p = send . CreateFileLink p

-- | @since 0.1
createDirectoryLink ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
createDirectoryLink p = send . CreateDirectoryLink p

-- | @since 0.1
removeDirectoryLink ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeDirectoryLink = send . RemoveDirectoryLink

-- | @since 0.1
setPermissions ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Permissions ->
  Eff es ()
setPermissions p = send . SetPermissions p

-- | @since 0.1
copyPermissions ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
copyPermissions p = send . CopyPermissions p

-- | @since 0.1
setAccessTime ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  UTCTime ->
  Eff es ()
setAccessTime p = send . SetAccessTime p

-- | @since 0.1
setModificationTime ::
  ( PathWriterEffect :> es
  ) =>
  Path ->
  UTCTime ->
  Eff es ()
setModificationTime p = send . SetModificationTime p

-- | Calls 'removeFile' if 'doesFileExist' is 'True'.
--
-- @since 0.1
removeFileIfExists ::
  ( PathReaderEffect :> es,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeFileIfExists = removeIfExists doesFileExist removeFile

-- | Calls 'removeDirectory' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryIfExists ::
  ( PathReaderEffect :> es,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeDirectoryIfExists = removeIfExists doesDirectoryExist removeDirectory

-- | Calls 'removeDirectoryRecursive' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryRecursiveIfExists ::
  ( PathReaderEffect :> es,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeDirectoryRecursiveIfExists =
  removeIfExists doesDirectoryExist removeDirectoryRecursive

-- | Calls 'removePathForcibly' if 'doesPathExist' is 'True'.
--
-- @since 0.1
removePathForciblyIfExists ::
  ( PathReaderEffect :> es,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removePathForciblyIfExists =
  removeIfExists doesPathExist removePathForcibly

removeIfExists :: (Monad m) => (t -> m Bool) -> (t -> m ()) -> t -> m ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)

-- | Exception for trying to create a path that already exists.
--
-- @since 0.1
newtype PathExistsException = MkPathExistsException Path
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
newtype PathDoesNotExistException = MkPathDoesNotExistException Path
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
    TargetNameLiteral !Path
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
_TargetNameLiteral :: Prism' TargetName Path
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

-- | 'copyDirectoryRecursiveConfig' with 'defaultCopyDirConfig'.
--
-- @since 0.1
copyDirectoryRecursive ::
  ( IORefEffect :> es,
    PathReaderEffect :> es,
    PathWriterEffect :> es
  ) =>
  -- | Source
  Path ->
  -- | Destination
  Path ->
  Eff es ()
copyDirectoryRecursive = copyDirectoryRecursiveConfig defaultCopyDirConfig

-- | @copyDirectoryRecursiveConfig cfg src dest@ copies the @src@ and its
-- contents into @dest@ e.g.
--
-- @
-- copyDirectoryRecursiveConfig cfg "path\/to\/foo" "path\/to\/bar"
-- @
--
-- will create @path\/to\/bar\/foo@, @path\/to\/bar\/\<target\>@, or copy
-- @foo@'s contents directly into @bar@, depending on the value of
-- 'targetName'.
--
-- The atomicity semantics are as follows:
--
-- * 'OverwriteNone': If an error is encountered, we roll back the successful
--    writes by deleting the entire @dest\/\<target\>@.
-- * 'OverwriteDirectories': If an error is encountered, we attempt to delete
--   all successfully written paths/directories. Because these deletes are
--   performed sequentially, we cannot guarantee all are removed before the
--   process is interrupted.
-- * 'OverwriteAll': Same as 'OverwriteDirectories', except paths that were
--   overwritten are not restored. That is, if a path @dest\/\<src\>\/p@ is
--   overwritten and an error later encountered, @p@ is not restored.
--
-- __Throws:__
--
-- * 'PathDoesNotExistException': if @dest@ does not exist.
-- * 'PathExistsException':
--
--     * 'OverwriteNone' and @dest/\<src\>@ exists.
--     * 'OverwriteDirectories' and some @dest/\<target\>\/p@ would be
--        overwritten.
--
-- @since 0.1
copyDirectoryRecursiveConfig ::
  ( IORefEffect :> es,
    PathReaderEffect :> es,
    PathWriterEffect :> es
  ) =>
  -- | Config
  CopyDirConfig ->
  -- | Source
  Path ->
  -- | Destination
  Path ->
  Eff es ()
copyDirectoryRecursiveConfig config src destRoot = do
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
    OverwriteNone -> copyDirectoryNoOverwrite src dest
    OverwriteDirectories -> copyDirectoryOverwrite False src dest
    OverwriteAll -> copyDirectoryOverwrite True src dest

copyDirectoryOverwrite ::
  ( IORefEffect :> es,
    PathReaderEffect :> es,
    PathWriterEffect :> es
  ) =>
  -- | Overwrite files
  Bool ->
  -- | Source
  Path ->
  -- | Destination
  Path ->
  Eff es ()
copyDirectoryOverwrite overwriteFiles src dest = do
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

copyDirectoryNoOverwrite ::
  ( PathReaderEffect :> es,
    PathWriterEffect :> es
  ) =>
  -- | Source
  Path ->
  -- | Destination
  Path ->
  Eff es ()
copyDirectoryNoOverwrite src dest = do
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

pathToStr :: Path -> String
#if MIN_VERSION_filepath(1,4,100) && MIN_VERSION_directory(1,3,8)
pathToStr = fmap FP.toChar . FP.unpack
#else
pathToStr = id
#endif
