{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides an effect for writing paths.
--
-- @since 0.1
module Effectful.FileSystem.PathWriter
  ( -- * Effect
    PathWriter (..),
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
    UnliftStrategy (SeqUnlift),
    type (:>),
  )
import Effectful.CallStack
  ( ECallStack,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO)
import Effectful.FileSystem.Path (Path)
import Effectful.FileSystem.PathReader
  ( PathReader,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
  )
import Effectful.TH (makeEffect_)
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
data PathWriter :: Effect where
  CreateDirectory :: HasCallStack => Path -> PathWriter m ()
  CreateDirectoryIfMissing :: HasCallStack => Bool -> Path -> PathWriter m ()
  RemoveDirectory :: HasCallStack => Path -> PathWriter m ()
  RemoveDirectoryRecursive :: HasCallStack => Path -> PathWriter m ()
  RemovePathForcibly :: HasCallStack => Path -> PathWriter m ()
  RenameDirectory :: HasCallStack => Path -> Path -> PathWriter m ()
  SetCurrentDirectory :: HasCallStack => Path -> PathWriter m ()
  WithCurrentDirectory :: HasCallStack => Path -> m a -> PathWriter m a
  RemoveFile :: HasCallStack => Path -> PathWriter m ()
  RenameFile :: HasCallStack => Path -> Path -> PathWriter m ()
  RenamePath :: HasCallStack => Path -> Path -> PathWriter m ()
  CopyFile :: HasCallStack => Path -> Path -> PathWriter m ()
  CopyFileWithMetadata :: HasCallStack => Path -> Path -> PathWriter m ()
  CreateFileLink :: HasCallStack => Path -> Path -> PathWriter m ()
  CreateDirectoryLink :: HasCallStack => Path -> Path -> PathWriter m ()
  RemoveDirectoryLink :: HasCallStack => Path -> PathWriter m ()
  SetPermissions :: HasCallStack => Path -> Permissions -> PathWriter m ()
  CopyPermissions :: HasCallStack => Path -> Path -> PathWriter m ()
  SetAccessTime :: HasCallStack => Path -> UTCTime -> PathWriter m ()
  SetModificationTime :: HasCallStack => Path -> UTCTime -> PathWriter m ()

-- | @since 0.1
type instance DispatchOf PathWriter = Dynamic

-- | Runs 'PathWriter' in 'IO'.
--
-- @since 0.1
runPathWriterIO ::
  ( ECallStack :> es,
    IOE :> es
  ) =>
  Eff (PathWriter : es) a ->
  Eff es a
runPathWriterIO = interpret $ \env -> \case
  CreateDirectory p -> addCallStack $ liftIO $ Dir.createDirectory p
  CreateDirectoryIfMissing b p -> addCallStack $ liftIO $ Dir.createDirectoryIfMissing b p
  RemoveDirectory p -> addCallStack $ liftIO $ Dir.removeDirectory p
  RemoveDirectoryRecursive p -> addCallStack $ liftIO $ Dir.removeDirectoryRecursive p
  RemovePathForcibly p -> addCallStack $ liftIO $ Dir.removePathForcibly p
  RenameDirectory p p' -> addCallStack $ liftIO $ Dir.renameDirectory p p'
  SetCurrentDirectory p -> addCallStack $ liftIO $ Dir.setCurrentDirectory p
  WithCurrentDirectory p m -> addCallStack $ localUnliftIO env SeqUnlift $ \runInIO ->
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

makeEffect_ ''PathWriter

-- | @since 0.1
createDirectory :: (HasCallStack, PathWriter :> es) => Path -> Eff es ()

-- | @since 0.1
createDirectoryIfMissing :: (HasCallStack, PathWriter :> es) => Bool -> Path -> Eff es ()

-- | @since 0.1
removeDirectory :: (HasCallStack, PathWriter :> es) => Path -> Eff es ()

-- | @since 0.1
removeDirectoryRecursive :: (HasCallStack, PathWriter :> es) => Path -> Eff es ()

-- | @since 0.1
removePathForcibly :: (HasCallStack, PathWriter :> es) => Path -> Eff es ()

-- | @since 0.1
renameDirectory :: (HasCallStack, PathWriter :> es) => Path -> Path -> Eff es ()

-- | @since 0.1
setCurrentDirectory :: (HasCallStack, PathWriter :> es) => Path -> Eff es ()

-- | @since 0.1
withCurrentDirectory :: (HasCallStack, PathWriter :> es) => Path -> Eff es a -> Eff es a

-- | @since 0.1
removeFile :: (HasCallStack, PathWriter :> es) => Path -> Eff es ()

-- | @since 0.1
renameFile :: (HasCallStack, PathWriter :> es) => Path -> Path -> Eff es ()

-- | @since 0.1
renamePath :: (HasCallStack, PathWriter :> es) => Path -> Path -> Eff es ()

-- | @since 0.1
copyFile :: (HasCallStack, PathWriter :> es) => Path -> Path -> Eff es ()

-- | @since 0.1
copyFileWithMetadata :: (HasCallStack, PathWriter :> es) => Path -> Path -> Eff es ()

-- | @since 0.1
createFileLink :: (HasCallStack, PathWriter :> es) => Path -> Path -> Eff es ()

-- | @since 0.1
createDirectoryLink :: (HasCallStack, PathWriter :> es) => Path -> Path -> Eff es ()

-- | @since 0.1
removeDirectoryLink :: (HasCallStack, PathWriter :> es) => Path -> Eff es ()

-- | @since 0.1
setPermissions :: (HasCallStack, PathWriter :> es) => Path -> Permissions -> Eff es ()

-- | @since 0.1
copyPermissions :: (HasCallStack, PathWriter :> es) => Path -> Path -> Eff es ()

-- | @since 0.1
setAccessTime :: (HasCallStack, PathWriter :> es) => Path -> UTCTime -> Eff es ()

-- | @since 0.1
setModificationTime :: (HasCallStack, PathWriter :> es) => Path -> UTCTime -> Eff es ()

{-}
-- | Represents file-system writer effects.
--
-- @since 0.1
class Monad m => PathWriter m where
  -- | @since 0.1
  createDirectory :: HasCallStack => Path -> m ()

  -- | @since 0.1
  createDirectoryIfMissing :: HasCallStack => Bool -> Path -> m ()

  -- | @since 0.1
  removeDirectory :: HasCallStack => Path -> m ()

  -- | @since 0.1
  removeDirectoryRecursive :: HasCallStack => Path -> m ()

  -- | @since 0.1
  removePathForcibly :: HasCallStack => Path -> m ()

  -- | @since 0.1
  renameDirectory :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  setCurrentDirectory :: HasCallStack => Path -> m ()

  -- | @since 0.1
  withCurrentDirectory :: HasCallStack => Path -> m a -> m a

  -- | @since 0.1
  removeFile :: HasCallStack => Path -> m ()

  -- | @since 0.1
  renameFile :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  renamePath :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  copyFile :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  copyFileWithMetadata :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  createFileLink :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  createDirectoryLink :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  removeDirectoryLink :: HasCallStack => Path -> m ()

  -- | @since 0.1
  setPermissions :: HasCallStack => Path -> Permissions -> m ()

  -- | @since 0.1
  copyPermissions :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  setAccessTime :: HasCallStack => Path -> UTCTime -> m ()

  -- | @since 0.1
  setModificationTime :: HasCallStack => Path -> UTCTime -> m ()

-- | @since 0.1
instance PathWriter IO where
  createDirectory = addCallStack . Dir.createDirectory
  createDirectoryIfMissing b = addCallStack . Dir.createDirectoryIfMissing b
  removeDirectory = addCallStack . Dir.removeDirectory
  removeDirectoryRecursive = addCallStack . Dir.removeDirectoryRecursive
  removePathForcibly = addCallStack . Dir.removePathForcibly
  renameDirectory p = addCallStack . Dir.renameDirectory p
  setCurrentDirectory = addCallStack . Dir.setCurrentDirectory
  withCurrentDirectory p = addCallStack . Dir.withCurrentDirectory p
  removeFile = addCallStack . Dir.removeFile
  renameFile p = addCallStack . Dir.renameFile p
  renamePath p = addCallStack . Dir.renamePath p
  copyFile p = addCallStack . Dir.copyFile p
  copyFileWithMetadata p = addCallStack . Dir.copyFileWithMetadata p
  createFileLink p = addCallStack . Dir.createFileLink p
  createDirectoryLink p = addCallStack . Dir.createDirectoryLink p
  removeDirectoryLink = addCallStack . Dir.removeDirectoryLink
  setPermissions p = addCallStack . Dir.setPermissions p
  copyPermissions p = addCallStack . Dir.copyPermissions p
  setAccessTime p = addCallStack . Dir.setAccessTime p
  setModificationTime p = addCallStack . Dir.setModificationTime p

-- | @since 0.1
instance PathWriter m => PathWriter (ReaderT env m) where
  createDirectory = lift . createDirectory
  createDirectoryIfMissing b = lift . createDirectoryIfMissing b
  removeDirectory = lift . removeDirectory
  removeDirectoryRecursive = lift . removeDirectoryRecursive
  removePathForcibly = lift . removePathForcibly
  renameDirectory p = lift . renameDirectory p
  setCurrentDirectory = lift . setCurrentDirectory
  withCurrentDirectory p action =
    ask >>= lift . \e -> withCurrentDirectory p (runReaderT action e)
  removeFile = lift . removeFile
  renameFile p = lift . renameFile p
  renamePath p = lift . renamePath p
  copyFile p = lift . copyFile p
  copyFileWithMetadata p = lift . copyFileWithMetadata p
  createFileLink p = lift . createFileLink p
  createDirectoryLink p = lift . createDirectoryLink p
  removeDirectoryLink = lift . removeDirectoryLink
  setPermissions p = lift . setPermissions p
  copyPermissions p = lift . copyPermissions p
  setAccessTime p = lift . setAccessTime p
  setModificationTime p = lift . setModificationTime p-}

-- | Calls 'removeFile' if 'doesFileExist' is 'True'.
--
-- @since 0.1
removeFileIfExists ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  Path ->
  Eff es ()
removeFileIfExists = removeIfExists doesFileExist removeFile

-- | Calls 'removeDirectory' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryIfExists ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  Path ->
  Eff es ()
removeDirectoryIfExists = removeIfExists doesDirectoryExist removeDirectory

-- | Calls 'removeDirectoryRecursive' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryRecursiveIfExists ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
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
    PathReader :> es,
    PathWriter :> es
  ) =>
  Path ->
  Eff es ()
removePathForciblyIfExists =
  removeIfExists doesPathExist removePathForcibly

removeIfExists :: Monad m => (t -> m Bool) -> (t -> m ()) -> t -> m ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)
