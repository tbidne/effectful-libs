{-# LANGUAGE CPP #-}

-- | Provides an effect for writing paths.
--
-- @since 0.1
module Effectful.FileSystem.PathWriter
  ( -- * Effect
    PathWriterEffect (..),
    Path,

    -- ** Functions
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

    -- * Misc
    removeFileIfExists,
    removeDirectoryIfExists,
    removeDirectoryRecursiveIfExists,
    removePathForciblyIfExists,

    -- * Reexports
    Permissions (..),
    UTCTime (..),
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
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
import Effectful.FileSystem.Path (Path)
import Effectful.FileSystem.PathReader
  ( PathReaderEffect,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
  )
import System.Directory (Permissions (..))
#if MIN_VERSION_filepath(1,4,100) && MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath qualified as Dir
#else
import System.Directory qualified as Dir
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
