{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for the writable portion of "System.Directory"'s
-- interface. For the static interface of the entire "System.Directory"
-- interface, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-FileSystem.html.
--
-- @since 0.1
module Effectful.FileSystem.PathWriter.Static
  ( -- * Effect
    PathWriterStatic,
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
    runPathWriterStaticIO,

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
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time (UTCTime (..))
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    seqUnliftIO,
    unsafeEff,
    unsafeEff_,
  )
import Effectful.FileSystem.PathReader.Static
  ( PathReaderStatic,
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

-- | Static effect for writing paths.
--
-- @since 0.1
data PathWriterStatic :: Effect

type instance DispatchOf PathWriterStatic = Static WithSideEffects

data instance StaticRep PathWriterStatic = MkPathWriterStatic

-- | Runs an 'PathWriterStatic' effect in IO.
--
-- @since 0.1
runPathWriterStaticIO :: (IOE :> es) => Eff (PathWriterStatic : es) a -> Eff es a
runPathWriterStaticIO = evalStaticRep MkPathWriterStatic

-- | Lifted 'Dir.createDirectory'.
--
-- @since 0.1
createDirectory ::
  (PathWriterStatic :> es) =>
  OsPath ->
  Eff es ()
createDirectory = unsafeEff_ . Dir.createDirectory

-- | Lifted 'Dir.createDirectoryIfMissing'.
--
-- @since 0.1
createDirectoryIfMissing ::
  (PathWriterStatic :> es) =>
  Bool ->
  OsPath ->
  Eff es ()
createDirectoryIfMissing b = unsafeEff_ . Dir.createDirectoryIfMissing b

-- | Lifted 'Dir.removeDirectory'.
--
-- @since 0.1
removeDirectory ::
  (PathWriterStatic :> es) =>
  OsPath ->
  Eff es ()
removeDirectory = unsafeEff_ . Dir.removeDirectory

-- | Lifted 'Dir.removeDirectoryRecursive'.
--
-- @since 0.1
removeDirectoryRecursive ::
  (PathWriterStatic :> es) =>
  OsPath ->
  Eff es ()
removeDirectoryRecursive = unsafeEff_ . Dir.removeDirectoryRecursive

-- | Lifted 'Dir.removePathForcibly'.
--
-- @since 0.1
removePathForcibly ::
  (PathWriterStatic :> es) =>
  OsPath ->
  Eff es ()
removePathForcibly = unsafeEff_ . Dir.removePathForcibly

-- | Lifted 'Dir.renameDirectory'.
--
-- @since 0.1
renameDirectory ::
  (PathWriterStatic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
renameDirectory p = unsafeEff_ . Dir.renameDirectory p

-- | Lifted 'Dir.setCurrentDirectory'.
--
-- @since 0.1
setCurrentDirectory ::
  (PathWriterStatic :> es) =>
  OsPath ->
  Eff es ()
setCurrentDirectory = unsafeEff_ . Dir.setCurrentDirectory

-- | Lifted 'Dir.withCurrentDirectory'.
--
-- @since 0.1
withCurrentDirectory ::
  (PathWriterStatic :> es) =>
  OsPath ->
  Eff es a ->
  Eff es a
withCurrentDirectory p m =
  unsafeEff $ \env -> seqUnliftIO env $
    \runInIO -> Dir.withCurrentDirectory p (runInIO m)

-- | Lifted 'Dir.removeFile'.
--
-- @since 0.1
removeFile ::
  (PathWriterStatic :> es) =>
  OsPath ->
  Eff es ()
removeFile = unsafeEff_ . Dir.removeFile

-- | Lifted 'Dir.renameFile'.
--
-- @since 0.1
renameFile ::
  (PathWriterStatic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
renameFile p = unsafeEff_ . Dir.renameFile p

-- | Lifted 'Dir.renamePath'.
--
-- @since 0.1
renamePath ::
  (PathWriterStatic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
renamePath p = unsafeEff_ . Dir.renamePath p

-- | Lifted 'Dir.copyFile'.
--
-- @since 0.1
copyFile ::
  (PathWriterStatic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyFile p = unsafeEff_ . Dir.copyFile p

-- | Lifted 'Dir.copyFileWithMetadata'.
--
-- @since 0.1
copyFileWithMetadata ::
  (PathWriterStatic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyFileWithMetadata p = unsafeEff_ . Dir.copyFileWithMetadata p

-- | Lifted 'Dir.createFileLink'.
--
-- @since 0.1
createFileLink ::
  (PathWriterStatic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
createFileLink p = unsafeEff_ . Dir.createFileLink p

-- | Lifted 'Dir.createDirectoryLink'.
--
-- @since 0.1
createDirectoryLink ::
  (PathWriterStatic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
createDirectoryLink p = unsafeEff_ . Dir.createDirectoryLink p

-- | Lifted 'Dir.removeDirectoryLink'.
--
-- @since 0.1
removeDirectoryLink ::
  (PathWriterStatic :> es) =>
  OsPath ->
  Eff es ()
removeDirectoryLink = unsafeEff_ . Dir.removeDirectoryLink

-- | Lifted 'Dir.setPermissions'.
--
-- @since 0.1
setPermissions ::
  (PathWriterStatic :> es) =>
  OsPath ->
  Permissions ->
  Eff es ()
setPermissions p = unsafeEff_ . Dir.setPermissions p

-- | Lifted 'Dir.copyPermissions'.
--
-- @since 0.1
copyPermissions ::
  (PathWriterStatic :> es) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyPermissions p = unsafeEff_ . Dir.copyPermissions p

-- | Lifted 'Dir.setAccessTime'.
--
-- @since 0.1
setAccessTime ::
  (PathWriterStatic :> es) =>
  OsPath ->
  UTCTime ->
  Eff es ()
setAccessTime p = unsafeEff_ . Dir.setAccessTime p

-- | Lifted 'Dir.setModificationTime'.
--
-- @since 0.1
setModificationTime ::
  (PathWriterStatic :> es) =>
  OsPath ->
  UTCTime ->
  Eff es ()
setModificationTime p = unsafeEff_ . Dir.setModificationTime p

-- | Calls 'removeFile' if 'doesFileExist' is 'True'.
--
-- @since 0.1
removeFileIfExists ::
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
  ) =>
  OsPath ->
  Eff es ()
removeFileIfExists = removeIfExists doesFileExist removeFile

-- | Calls 'removeDirectory' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryIfExists ::
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
  ) =>
  OsPath ->
  Eff es ()
removeDirectoryIfExists = removeIfExists doesDirectoryExist removeDirectory

-- | Calls 'removeDirectoryRecursive' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryRecursiveIfExists ::
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
  ) =>
  OsPath ->
  Eff es ()
removeDirectoryRecursiveIfExists =
  removeIfExists doesDirectoryExist removeDirectoryRecursive

-- | Calls 'removePathForcibly' if 'doesPathExist' is 'True'.
--
-- @since 0.1
removePathForciblyIfExists ::
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
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
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
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
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
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
