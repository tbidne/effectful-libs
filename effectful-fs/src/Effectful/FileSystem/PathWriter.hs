{-# LANGUAGE CPP #-}

-- | Provides an effect for writing paths.
--
-- @since 0.1
module Effectful.FileSystem.PathWriter
  ( -- * Effect
    EffectPathWriter (..),
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
  ( EffectCallStack,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO, send)
import Effectful.FileSystem.Path (Path)
import Effectful.FileSystem.PathReader
  ( EffectPathReader,
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
data EffectPathWriter :: Effect where
  CreateDirectory :: HasCallStack => Path -> EffectPathWriter m ()
  CreateDirectoryIfMissing :: HasCallStack => Bool -> Path -> EffectPathWriter m ()
  RemoveDirectory :: HasCallStack => Path -> EffectPathWriter m ()
  RemoveDirectoryRecursive :: HasCallStack => Path -> EffectPathWriter m ()
  RemovePathForcibly :: HasCallStack => Path -> EffectPathWriter m ()
  RenameDirectory :: HasCallStack => Path -> Path -> EffectPathWriter m ()
  SetCurrentDirectory :: HasCallStack => Path -> EffectPathWriter m ()
  WithCurrentDirectory :: HasCallStack => Path -> m a -> EffectPathWriter m a
  RemoveFile :: HasCallStack => Path -> EffectPathWriter m ()
  RenameFile :: HasCallStack => Path -> Path -> EffectPathWriter m ()
  RenamePath :: HasCallStack => Path -> Path -> EffectPathWriter m ()
  CopyFile :: HasCallStack => Path -> Path -> EffectPathWriter m ()
  CopyFileWithMetadata :: HasCallStack => Path -> Path -> EffectPathWriter m ()
  CreateFileLink :: HasCallStack => Path -> Path -> EffectPathWriter m ()
  CreateDirectoryLink :: HasCallStack => Path -> Path -> EffectPathWriter m ()
  RemoveDirectoryLink :: HasCallStack => Path -> EffectPathWriter m ()
  SetPermissions :: HasCallStack => Path -> Permissions -> EffectPathWriter m ()
  CopyPermissions :: HasCallStack => Path -> Path -> EffectPathWriter m ()
  SetAccessTime :: HasCallStack => Path -> UTCTime -> EffectPathWriter m ()
  SetModificationTime :: HasCallStack => Path -> UTCTime -> EffectPathWriter m ()

-- | @since 0.1
type instance DispatchOf EffectPathWriter = Dynamic

-- | Runs 'PathWriter' in 'IO'.
--
-- @since 0.1
runPathWriterIO ::
  ( EffectCallStack :> es,
    IOE :> es
  ) =>
  Eff (EffectPathWriter : es) a ->
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

-- | @since 0.1
createDirectory ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es ()
createDirectory = send . CreateDirectory

-- | @since 0.1
createDirectoryIfMissing ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Bool ->
  Path ->
  Eff es ()
createDirectoryIfMissing b = send . CreateDirectoryIfMissing b

-- | @since 0.1
removeDirectory ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es ()
removeDirectory = send . RemoveDirectory

-- | @since 0.1
removeDirectoryRecursive ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es ()
removeDirectoryRecursive = send . RemoveDirectoryRecursive

-- | @since 0.1
removePathForcibly ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es ()
removePathForcibly = send . RemovePathForcibly

-- | @since 0.1
renameDirectory ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Path ->
  Eff es ()
renameDirectory p = send . RenameDirectory p

-- | @since 0.1
setCurrentDirectory ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es ()
setCurrentDirectory = send . SetCurrentDirectory

-- | @since 0.1
withCurrentDirectory ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es a ->
  Eff es a
withCurrentDirectory p = send . WithCurrentDirectory p

-- | @since 0.1
removeFile ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es ()
removeFile = send . RemoveFile

-- | @since 0.1
renameFile ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Path ->
  Eff es ()
renameFile p = send . RenameFile p

-- | @since 0.1
renamePath ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Path ->
  Eff es ()
renamePath p = send . RenamePath p

-- | @since 0.1
copyFile ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Path ->
  Eff es ()
copyFile p = send . CopyFile p

-- | @since 0.1
copyFileWithMetadata ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Path ->
  Eff es ()
copyFileWithMetadata p = send . CopyFileWithMetadata p

-- | @since 0.1
createFileLink ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Path ->
  Eff es ()
createFileLink p = send . CreateFileLink p

-- | @since 0.1
createDirectoryLink ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Path ->
  Eff es ()
createDirectoryLink p = send . CreateDirectoryLink p

-- | @since 0.1
removeDirectoryLink ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es ()
removeDirectoryLink = send . RemoveDirectoryLink

-- | @since 0.1
setPermissions ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Permissions ->
  Eff es ()
setPermissions p = send . SetPermissions p

-- | @since 0.1
copyPermissions ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Path ->
  Eff es ()
copyPermissions p = send . CopyPermissions p

-- | @since 0.1
setAccessTime ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  UTCTime ->
  Eff es ()
setAccessTime p = send . SetAccessTime p

-- | @since 0.1
setModificationTime ::
  ( EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  UTCTime ->
  Eff es ()
setModificationTime p = send . SetModificationTime p

-- | Calls 'removeFile' if 'doesFileExist' is 'True'.
--
-- @since 0.1
removeFileIfExists ::
  ( EffectPathReader :> es,
    EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es ()
removeFileIfExists = removeIfExists doesFileExist removeFile

-- | Calls 'removeDirectory' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryIfExists ::
  ( EffectPathReader :> es,
    EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es ()
removeDirectoryIfExists = removeIfExists doesDirectoryExist removeDirectory

-- | Calls 'removeDirectoryRecursive' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryRecursiveIfExists ::
  ( EffectPathReader :> es,
    EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es ()
removeDirectoryRecursiveIfExists =
  removeIfExists doesDirectoryExist removeDirectoryRecursive

-- | Calls 'removePathForcibly' if 'doesPathExist' is 'True'.
--
-- @since 0.1
removePathForciblyIfExists ::
  ( EffectPathReader :> es,
    EffectPathWriter :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es ()
removePathForciblyIfExists =
  removeIfExists doesPathExist removePathForcibly

removeIfExists :: Monad m => (t -> m Bool) -> (t -> m ()) -> t -> m ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)
