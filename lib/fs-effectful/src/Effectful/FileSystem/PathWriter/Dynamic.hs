{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides a dynamic effect for the writable portion of "System.Directory"'s
-- interface.
--
-- @since 0.1
module Effectful.FileSystem.PathWriter.Dynamic
  ( -- * Effect
    PathWriterDynamic (..),
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
    runPathWriterDynamicIO,

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
    OsPath,
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
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReaderDynamic,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    listDirectoryRecursive,
  )
import Effectful.FileSystem.Utils (OsPath, (</>))
import Effectful.IORef.Dynamic (IORefDynamic, modifyIORef', newIORef, readIORef)
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
import System.Directory.OsPath qualified as Dir
import System.OsPath qualified as FP

-- | Effect for writing paths.
--
-- @since 0.1
data PathWriterDynamic :: Effect where
  CreateDirectory :: OsPath -> PathWriterDynamic m ()
  CreateDirectoryIfMissing :: Bool -> OsPath -> PathWriterDynamic m ()
  RemoveDirectory :: OsPath -> PathWriterDynamic m ()
  RemoveDirectoryRecursive :: OsPath -> PathWriterDynamic m ()
  RemovePathForcibly :: OsPath -> PathWriterDynamic m ()
  RenameDirectory :: OsPath -> OsPath -> PathWriterDynamic m ()
  SetCurrentDirectory :: OsPath -> PathWriterDynamic m ()
  WithCurrentDirectory :: OsPath -> m a -> PathWriterDynamic m a
  RemoveFile :: OsPath -> PathWriterDynamic m ()
  RenameFile :: OsPath -> OsPath -> PathWriterDynamic m ()
  RenamePath :: OsPath -> OsPath -> PathWriterDynamic m ()
  CopyFile :: OsPath -> OsPath -> PathWriterDynamic m ()
  CopyFileWithMetadata :: OsPath -> OsPath -> PathWriterDynamic m ()
  CreateFileLink :: OsPath -> OsPath -> PathWriterDynamic m ()
  CreateDirectoryLink :: OsPath -> OsPath -> PathWriterDynamic m ()
  RemoveDirectoryLink :: OsPath -> PathWriterDynamic m ()
  SetPermissions :: OsPath -> Permissions -> PathWriterDynamic m ()
  CopyPermissions :: OsPath -> OsPath -> PathWriterDynamic m ()
  SetAccessTime :: OsPath -> UTCTime -> PathWriterDynamic m ()
  SetModificationTime :: OsPath -> UTCTime -> PathWriterDynamic m ()

-- | @since 0.1
type instance DispatchOf PathWriterDynamic = Dynamic

-- | Runs 'PathWriterDynamic' in 'IO'.
--
-- @since 0.1
runPathWriterDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (PathWriterDynamic : es) a ->
  Eff es a
runPathWriterDynamicIO = interpret $ \env -> \case
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

-- | Lifted 'Dir.createDirectory'.
--
-- @since 0.1
createDirectory ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
createDirectory = send . CreateDirectory

-- | Lifted 'Dir.createDirectoryIfMissing'.
--
-- @since 0.1
createDirectoryIfMissing ::
  ( PathWriterDynamic :> es
  ) =>
  Bool ->
  OsPath ->
  Eff es ()
createDirectoryIfMissing b = send . CreateDirectoryIfMissing b

-- | Lifted 'Dir.removeDirectory'.
--
-- @since 0.1
removeDirectory ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
removeDirectory = send . RemoveDirectory

-- | Lifted 'Dir.removeDirectoryRecursive'.
--
-- @since 0.1
removeDirectoryRecursive ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
removeDirectoryRecursive = send . RemoveDirectoryRecursive

-- | Lifted 'Dir.removePathForcibly'.
--
-- @since 0.1
removePathForcibly ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
removePathForcibly = send . RemovePathForcibly

-- | Lifted 'Dir.renameDirectory'.
--
-- @since 0.1
renameDirectory ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
renameDirectory p = send . RenameDirectory p

-- | Lifted 'Dir.setCurrentDirectory'.
--
-- @since 0.1
setCurrentDirectory ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
setCurrentDirectory = send . SetCurrentDirectory

-- | Lifted 'Dir.withCurrentDirectory'.
--
-- @since 0.1
withCurrentDirectory ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es a ->
  Eff es a
withCurrentDirectory p = send . WithCurrentDirectory p

-- | Lifted 'Dir.removeFile'.
--
-- @since 0.1
removeFile ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
removeFile = send . RemoveFile

-- | Lifted 'Dir.renameFile'.
--
-- @since 0.1
renameFile ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
renameFile p = send . RenameFile p

-- | Lifted 'Dir.renamePath'.
--
-- @since 0.1
renamePath ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
renamePath p = send . RenamePath p

-- | Lifted 'Dir.copyFile'.
--
-- @since 0.1
copyFile ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyFile p = send . CopyFile p

-- | Lifted 'Dir.copyFileWithMetadata'.
--
-- @since 0.1
copyFileWithMetadata ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyFileWithMetadata p = send . CopyFileWithMetadata p

-- | Lifted 'Dir.createFileLink'.
--
-- @since 0.1
createFileLink ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
createFileLink p = send . CreateFileLink p

-- | Lifted 'Dir.createDirectoryLink'.
--
-- @since 0.1
createDirectoryLink ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
createDirectoryLink p = send . CreateDirectoryLink p

-- | Lifted 'Dir.removeDirectoryLink'.
--
-- @since 0.1
removeDirectoryLink ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
removeDirectoryLink = send . RemoveDirectoryLink

-- | Lifted 'Dir.setPermissions'.
--
-- @since 0.1
setPermissions ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  Permissions ->
  Eff es ()
setPermissions p = send . SetPermissions p

-- | Lifted 'Dir.copyPermissions'.
--
-- @since 0.1
copyPermissions ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyPermissions p = send . CopyPermissions p

-- | Lifted 'Dir.setAccessTime'.
--
-- @since 0.1
setAccessTime ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  UTCTime ->
  Eff es ()
setAccessTime p = send . SetAccessTime p

-- | Lifted 'Dir.setModificationTime'.
--
-- @since 0.1
setModificationTime ::
  ( PathWriterDynamic :> es
  ) =>
  OsPath ->
  UTCTime ->
  Eff es ()
setModificationTime p = send . SetModificationTime p

-- | Calls 'removeFile' if 'doesFileExist' is 'True'.
--
-- @since 0.1
removeFileIfExists ::
  ( PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
removeFileIfExists = removeIfExists doesFileExist removeFile

-- | Calls 'removeDirectory' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryIfExists ::
  ( PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
removeDirectoryIfExists = removeIfExists doesDirectoryExist removeDirectory

-- | Calls 'removeDirectoryRecursive' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryRecursiveIfExists ::
  ( PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
removeDirectoryRecursiveIfExists =
  removeIfExists doesDirectoryExist removeDirectoryRecursive

-- | Calls 'removePathForcibly' if 'doesPathExist' is 'True'.
--
-- @since 0.1
removePathForciblyIfExists ::
  ( PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
removePathForciblyIfExists =
  removeIfExists doesPathExist removePathForcibly

removeIfExists :: (Monad m) => (t -> m Bool) -> (t -> m ()) -> t -> m ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)

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

-- | 'copyDirectoryRecursiveConfig' with 'defaultCopyDirConfig'.
--
-- @since 0.1
copyDirectoryRecursive ::
  ( IORefDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
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
  ( IORefDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  -- | Config
  CopyDirConfig ->
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
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
  ( IORefDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  -- | Overwrite files
  Bool ->
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
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
  ( PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
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

pathToStr :: OsPath -> String
pathToStr = fmap FP.toChar . FP.unpack
