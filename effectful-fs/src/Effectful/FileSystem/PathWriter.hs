{-# LANGUAGE CPP #-}

-- | Provides an effect for writing paths.
--
-- @since 0.1
module Effectful.FileSystem.PathWriter
  ( -- * Effect
    PathWriterEffect (..),
    Path,

    -- * Handler
    runPathWriterIO,

    -- * Functions
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
import Effectful.CallStack
  ( CallStackEffect,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.FileSystem.Path (Path)
import Effectful.FileSystem.PathReader
  ( PathReaderEffect,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
  )
import GHC.Stack (HasCallStack)
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
  CreateDirectory :: HasCallStack => Path -> PathWriterEffect m ()
  CreateDirectoryIfMissing :: HasCallStack => Bool -> Path -> PathWriterEffect m ()
  RemoveDirectory :: HasCallStack => Path -> PathWriterEffect m ()
  RemoveDirectoryRecursive :: HasCallStack => Path -> PathWriterEffect m ()
  RemovePathForcibly :: HasCallStack => Path -> PathWriterEffect m ()
  RenameDirectory :: HasCallStack => Path -> Path -> PathWriterEffect m ()
  SetCurrentDirectory :: HasCallStack => Path -> PathWriterEffect m ()
  WithCurrentDirectory :: HasCallStack => Path -> m a -> PathWriterEffect m a
  RemoveFile :: HasCallStack => Path -> PathWriterEffect m ()
  RenameFile :: HasCallStack => Path -> Path -> PathWriterEffect m ()
  RenamePath :: HasCallStack => Path -> Path -> PathWriterEffect m ()
  CopyFile :: HasCallStack => Path -> Path -> PathWriterEffect m ()
  CopyFileWithMetadata :: HasCallStack => Path -> Path -> PathWriterEffect m ()
  CreateFileLink :: HasCallStack => Path -> Path -> PathWriterEffect m ()
  CreateDirectoryLink :: HasCallStack => Path -> Path -> PathWriterEffect m ()
  RemoveDirectoryLink :: HasCallStack => Path -> PathWriterEffect m ()
  SetPermissions :: HasCallStack => Path -> Permissions -> PathWriterEffect m ()
  CopyPermissions :: HasCallStack => Path -> Path -> PathWriterEffect m ()
  SetAccessTime :: HasCallStack => Path -> UTCTime -> PathWriterEffect m ()
  SetModificationTime :: HasCallStack => Path -> UTCTime -> PathWriterEffect m ()

-- | @since 0.1
type instance DispatchOf PathWriterEffect = Dynamic

-- | Runs 'PathWriterEffect' in 'IO'.
--
-- @since 0.1
runPathWriterIO ::
  ( CallStackEffect :> es,
    IOE :> es
  ) =>
  Eff (PathWriterEffect : es) a ->
  Eff es a
runPathWriterIO = interpret $ \env -> \case
  CreateDirectory p -> addCallStack $ liftIO $ Dir.createDirectory p
  CreateDirectoryIfMissing b p -> addCallStack $ liftIO $ Dir.createDirectoryIfMissing b p
  RemoveDirectory p -> addCallStack $ liftIO $ Dir.removeDirectory p
  RemoveDirectoryRecursive p -> addCallStack $ liftIO $ Dir.removeDirectoryRecursive p
  RemovePathForcibly p -> addCallStack $ liftIO $ Dir.removePathForcibly p
  RenameDirectory p p' -> addCallStack $ liftIO $ Dir.renameDirectory p p'
  SetCurrentDirectory p -> addCallStack $ liftIO $ Dir.setCurrentDirectory p
  WithCurrentDirectory p m -> addCallStack $ localSeqUnliftIO env $ \runInIO ->
    liftIO $ Dir.withCurrentDirectory p (runInIO m)
  RemoveFile p -> addCallStack $ liftIO $ Dir.removeFile p
  RenameFile p p' -> addCallStack $ liftIO $ Dir.renameFile p p'
  RenamePath p p' -> addCallStack $ liftIO $ Dir.renamePath p p'
  CopyFile p p' -> addCallStack $ liftIO $ Dir.copyFile p p'
  CopyFileWithMetadata p p' -> addCallStack $ liftIO $ Dir.copyFileWithMetadata p p'
  CreateFileLink p p' -> addCallStack $ liftIO $ Dir.createFileLink p p'
  CreateDirectoryLink p p' -> addCallStack $ liftIO $ Dir.createDirectoryLink p p'
  RemoveDirectoryLink p -> addCallStack $ liftIO $ Dir.removeDirectoryLink p
  SetPermissions p ps -> addCallStack $ liftIO $ Dir.setPermissions p ps
  CopyPermissions p ps -> addCallStack $ liftIO $ Dir.copyPermissions p ps
  SetAccessTime p t -> addCallStack $ liftIO $ Dir.setAccessTime p t
  SetModificationTime p t -> addCallStack $ liftIO $ Dir.setModificationTime p t

-- | @since 0.1
createDirectory ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
createDirectory = send . CreateDirectory

-- | @since 0.1
createDirectoryIfMissing ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Bool ->
  Path ->
  Eff es ()
createDirectoryIfMissing b = send . CreateDirectoryIfMissing b

-- | @since 0.1
removeDirectory ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeDirectory = send . RemoveDirectory

-- | @since 0.1
removeDirectoryRecursive ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeDirectoryRecursive = send . RemoveDirectoryRecursive

-- | @since 0.1
removePathForcibly ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removePathForcibly = send . RemovePathForcibly

-- | @since 0.1
renameDirectory ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
renameDirectory p = send . RenameDirectory p

-- | @since 0.1
setCurrentDirectory ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
setCurrentDirectory = send . SetCurrentDirectory

-- | @since 0.1
withCurrentDirectory ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es a ->
  Eff es a
withCurrentDirectory p = send . WithCurrentDirectory p

-- | @since 0.1
removeFile ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeFile = send . RemoveFile

-- | @since 0.1
renameFile ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
renameFile p = send . RenameFile p

-- | @since 0.1
renamePath ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
renamePath p = send . RenamePath p

-- | @since 0.1
copyFile ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
copyFile p = send . CopyFile p

-- | @since 0.1
copyFileWithMetadata ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
copyFileWithMetadata p = send . CopyFileWithMetadata p

-- | @since 0.1
createFileLink ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
createFileLink p = send . CreateFileLink p

-- | @since 0.1
createDirectoryLink ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
createDirectoryLink p = send . CreateDirectoryLink p

-- | @since 0.1
removeDirectoryLink ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeDirectoryLink = send . RemoveDirectoryLink

-- | @since 0.1
setPermissions ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Permissions ->
  Eff es ()
setPermissions p = send . SetPermissions p

-- | @since 0.1
copyPermissions ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  Path ->
  Eff es ()
copyPermissions p = send . CopyPermissions p

-- | @since 0.1
setAccessTime ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  UTCTime ->
  Eff es ()
setAccessTime p = send . SetAccessTime p

-- | @since 0.1
setModificationTime ::
  ( HasCallStack,
    PathWriterEffect :> es
  ) =>
  Path ->
  UTCTime ->
  Eff es ()
setModificationTime p = send . SetModificationTime p

-- | Calls 'removeFile' if 'doesFileExist' is 'True'.
--
-- @since 0.1
removeFileIfExists ::
  ( HasCallStack,
    PathReaderEffect :> es,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeFileIfExists = removeIfExists doesFileExist removeFile

-- | Calls 'removeDirectory' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryIfExists ::
  ( HasCallStack,
    PathReaderEffect :> es,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removeDirectoryIfExists = removeIfExists doesDirectoryExist removeDirectory

-- | Calls 'removeDirectoryRecursive' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryRecursiveIfExists ::
  ( HasCallStack,
    PathReaderEffect :> es,
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
  ( HasCallStack,
    PathReaderEffect :> es,
    PathWriterEffect :> es
  ) =>
  Path ->
  Eff es ()
removePathForciblyIfExists =
  removeIfExists doesPathExist removePathForcibly

removeIfExists :: Monad m => (t -> m Bool) -> (t -> m ()) -> t -> m ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)
