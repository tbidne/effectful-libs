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
    Utils.defaultCopyDirConfig,

    -- ** Functions
    copyDirectoryRecursive,
    copyDirectoryRecursiveConfig,

    -- ** Optics
    Utils._OverwriteNone,
    Utils._OverwriteDirectories,
    Utils._OverwriteAll,
    Utils._TargetNameSrc,
    Utils._TargetNameLiteral,
    Utils._TargetNameDest,

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

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (modifyIORef', newIORef, readIORef)
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
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReaderDynamic,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    listDirectoryRecursive,
  )
import Effectful.FileSystem.PathWriter.Utils
  ( CopyDirConfig (..),
    Handle (MkHandle),
    Overwrite (..),
    PathDoesNotExistException (..),
    PathExistsException (..),
    TargetName (..),
  )
import Effectful.FileSystem.PathWriter.Utils qualified as Utils
import Effectful.FileSystem.Utils (OsPath)
import System.Directory (Permissions (..))
import System.Directory.OsPath qualified as Dir

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
  (PathWriterDynamic :> es) =>
  OsPath ->
  Eff es ()
createDirectory = send . CreateDirectory

-- | Lifted 'Dir.createDirectoryIfMissing'.
--
-- @since 0.1
createDirectoryIfMissing ::
  (PathWriterDynamic :> es) =>
  Bool ->
  OsPath ->
  Eff es ()
createDirectoryIfMissing b = send . CreateDirectoryIfMissing b

-- | Lifted 'Dir.removeDirectory'.
--
-- @since 0.1
removeDirectory ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  Eff es ()
removeDirectory = send . RemoveDirectory

-- | Lifted 'Dir.removeDirectoryRecursive'.
--
-- @since 0.1
removeDirectoryRecursive ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  Eff es ()
removeDirectoryRecursive = send . RemoveDirectoryRecursive

-- | Lifted 'Dir.removePathForcibly'.
--
-- @since 0.1
removePathForcibly ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  Eff es ()
removePathForcibly = send . RemovePathForcibly

-- | Lifted 'Dir.renameDirectory'.
--
-- @since 0.1
renameDirectory ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
renameDirectory p = send . RenameDirectory p

-- | Lifted 'Dir.setCurrentDirectory'.
--
-- @since 0.1
setCurrentDirectory ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  Eff es ()
setCurrentDirectory = send . SetCurrentDirectory

-- | Lifted 'Dir.withCurrentDirectory'.
--
-- @since 0.1
withCurrentDirectory ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  Eff es a ->
  Eff es a
withCurrentDirectory p = send . WithCurrentDirectory p

-- | Lifted 'Dir.removeFile'.
--
-- @since 0.1
removeFile ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  Eff es ()
removeFile = send . RemoveFile

-- | Lifted 'Dir.renameFile'.
--
-- @since 0.1
renameFile ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
renameFile p = send . RenameFile p

-- | Lifted 'Dir.renamePath'.
--
-- @since 0.1
renamePath ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
renamePath p = send . RenamePath p

-- | Lifted 'Dir.copyFile'.
--
-- @since 0.1
copyFile ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyFile p = send . CopyFile p

-- | Lifted 'Dir.copyFileWithMetadata'.
--
-- @since 0.1
copyFileWithMetadata ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyFileWithMetadata p = send . CopyFileWithMetadata p

-- | Lifted 'Dir.createFileLink'.
--
-- @since 0.1
createFileLink ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
createFileLink p = send . CreateFileLink p

-- | Lifted 'Dir.createDirectoryLink'.
--
-- @since 0.1
createDirectoryLink ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
createDirectoryLink p = send . CreateDirectoryLink p

-- | Lifted 'Dir.removeDirectoryLink'.
--
-- @since 0.1
removeDirectoryLink ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  Eff es ()
removeDirectoryLink = send . RemoveDirectoryLink

-- | Lifted 'Dir.setPermissions'.
--
-- @since 0.1
setPermissions ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  Permissions ->
  Eff es ()
setPermissions p = send . SetPermissions p

-- | Lifted 'Dir.copyPermissions'.
--
-- @since 0.1
copyPermissions ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyPermissions p = send . CopyPermissions p

-- | Lifted 'Dir.setAccessTime'.
--
-- @since 0.1
setAccessTime ::
  (PathWriterDynamic :> es) =>
  OsPath ->
  UTCTime ->
  Eff es ()
setAccessTime p = send . SetAccessTime p

-- | Lifted 'Dir.setModificationTime'.
--
-- @since 0.1
setModificationTime ::
  (PathWriterDynamic :> es) =>
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

-- | 'copyDirectoryRecursiveConfig' with 'Utils.defaultCopyDirConfig'.
--
-- @since 0.1
copyDirectoryRecursive ::
  ( PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  Eff es ()
copyDirectoryRecursive =
  copyDirectoryRecursiveConfig Utils.defaultCopyDirConfig

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
  forall es.
  ( PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  -- | Config
  CopyDirConfig ->
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  Eff es ()
copyDirectoryRecursiveConfig = Utils.copyDirectoryRecursiveConfig handle
  where
    handle :: Handle es
    handle =
      MkHandle
        { Utils.newIORef = unsafeEff_ . newIORef,
          Utils.readIORef = unsafeEff_ . readIORef,
          Utils.modifyIORef' = \r -> unsafeEff_ . modifyIORef' r,
          Utils.doesDirectoryExist = doesDirectoryExist,
          Utils.doesFileExist = doesFileExist,
          Utils.listDirectoryRecursive = listDirectoryRecursive,
          Utils.createDirectory = createDirectory,
          Utils.createDirectoryIfMissing = createDirectoryIfMissing,
          Utils.copyFileWithMetadata = copyFileWithMetadata,
          Utils.removeFile = removeFile,
          Utils.removeDirectory = removeDirectory,
          Utils.removeDirectoryRecursive = removeDirectoryRecursive
        }
