{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for the writable portion of "System.Directory"'s
-- interface. For the static interface of the entire "System.Directory"
-- interface, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-FileSystem.html.
--
-- @since 0.1
module Effectful.FileSystem.PathWriter.Static
  ( -- * Class
    MonadPathWriter (..),

    -- * Effect
    PathWriterStatic,

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
    Permissions,
    UTCTime (..),
  )
where

import Control.Monad (when)
import Data.Time (UTCTime (UTCTime, utctDay, utctDayTime))
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
import Effectful.Exception (MonadMask)
import Effectful.FileSystem.PathReader.Static
  ( MonadPathReader,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    listDirectoryRecursive,
  )
import Effectful.FileSystem.PathWriter.Utils
  ( CopyDirConfig (MkCopyDirConfig, overwrite, targetName),
    Handle (MkHandle),
    Overwrite (OverwriteAll, OverwriteDirectories, OverwriteNone),
    PathDoesNotExistException (MkPathDoesNotExistException),
    PathExistsException (MkPathExistsException),
    TargetName (TargetNameDest, TargetNameLiteral, TargetNameSrc),
  )
import Effectful.FileSystem.PathWriter.Utils qualified as Utils
import Effectful.FileSystem.Utils (OsPath)
import Effectful.IORef.Static (MonadIORef (modifyIORef', newIORef, readIORef))
import System.Directory (Permissions)
import System.Directory.OsPath qualified as Dir

-- | Represents file-system writer effects.
--
-- @since 0.1
class (Monad m) => MonadPathWriter m where
  -- | Lifted 'Dir.createDirectory'.
  --
  -- @since 0.1
  createDirectory :: OsPath -> m ()

  -- | Lifted 'Dir.createDirectoryIfMissing'.
  --
  -- @since 0.1
  createDirectoryIfMissing ::
    -- | Create its parents too?
    Bool ->
    -- | The path to the directory you want to make
    OsPath ->
    m ()

  -- | Lifted 'Dir.removeDirectory'.
  --
  -- @since 0.1
  removeDirectory :: OsPath -> m ()

  -- | Lifted 'Dir.removeDirectoryRecursive'.
  --
  -- @since 0.1
  removeDirectoryRecursive :: OsPath -> m ()

  -- | Lifted 'Dir.removePathForcibly'.
  --
  -- @since 0.1
  removePathForcibly :: OsPath -> m ()

  -- | Lifted 'Dir.renameDirectory'.
  --
  -- @since 0.1
  renameDirectory :: OsPath -> OsPath -> m ()

  -- | Lifted 'Dir.setCurrentDirectory'.
  --
  -- @since 0.1
  setCurrentDirectory :: OsPath -> m ()

  -- | Lifted 'Dir.withCurrentDirectory'.
  --
  -- @since 0.1
  withCurrentDirectory :: OsPath -> m a -> m a

  -- | Lifted 'Dir.removeFile'.
  --
  -- @since 0.1
  removeFile :: OsPath -> m ()

  -- | Lifted 'Dir.renameFile'.
  --
  -- @since 0.1
  renameFile :: OsPath -> OsPath -> m ()

  -- | Lifted 'Dir.renamePath'.
  --
  -- @since 0.1
  renamePath ::
    -- | Old path
    OsPath ->
    -- | New path
    OsPath ->
    m ()

  -- | Lifted 'Dir.copyFile'.
  --
  -- @since 0.1
  copyFile ::
    -- | Source filename
    OsPath ->
    -- | Destination filename
    OsPath ->
    m ()

  -- | Lifted 'Dir.copyFileWithMetadata'.
  --
  -- @since 0.1
  copyFileWithMetadata ::
    -- | Source file
    OsPath ->
    -- | Destination file
    OsPath ->
    m ()

  -- | Lifted 'Dir.createFileLink'.
  --
  -- @since 0.1
  createFileLink ::
    -- | path to the target file
    OsPath ->
    -- | path of the link to be created
    OsPath ->
    m ()

  -- | Lifted 'Dir.createDirectoryLink'.
  --
  -- @since 0.1
  createDirectoryLink ::
    -- | path to the target directory
    OsPath ->
    -- | path of the link to be created
    OsPath ->
    m ()

  -- | Lifted 'Dir.removeDirectoryLink'.
  --
  -- @since 0.1
  removeDirectoryLink :: OsPath -> m ()

  -- | Lifted 'Dir.setPermissions'.
  --
  -- @since 0.1
  setPermissions :: OsPath -> Permissions -> m ()

  -- | Lifted 'Dir.copyPermissions'.
  --
  -- @since 0.1
  copyPermissions :: OsPath -> OsPath -> m ()

  -- | Lifted 'Dir.setAccessTime'.
  --
  -- @since 0.1
  setAccessTime :: OsPath -> UTCTime -> m ()

  -- | Lifted 'Dir.setModificationTime'.
  --
  -- @since 0.1
  setModificationTime :: OsPath -> UTCTime -> m ()

-- | @since 0.1
instance MonadPathWriter IO where
  createDirectory = Dir.createDirectory
  createDirectoryIfMissing = Dir.createDirectoryIfMissing
  removeDirectory = Dir.removeDirectory
  removeDirectoryRecursive = Dir.removeDirectoryRecursive
  removePathForcibly = Dir.removePathForcibly
  renameDirectory = Dir.renameDirectory
  setCurrentDirectory = Dir.setCurrentDirectory
  withCurrentDirectory = Dir.withCurrentDirectory
  removeFile = Dir.removeFile
  renameFile = Dir.renameFile
  renamePath = Dir.renamePath
  copyFile = Dir.copyFile
  copyFileWithMetadata = Dir.copyFileWithMetadata
  createFileLink = Dir.createFileLink
  createDirectoryLink = Dir.createDirectoryLink
  removeDirectoryLink = Dir.removeDirectoryLink
  setPermissions = Dir.setPermissions
  copyPermissions = Dir.copyPermissions
  setAccessTime = Dir.setAccessTime
  setModificationTime = Dir.setModificationTime

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

-- | @since 0.1
instance (PathWriterStatic :> es) => MonadPathWriter (Eff es) where
  createDirectory = unsafeEff_ . Dir.createDirectory
  createDirectoryIfMissing b = unsafeEff_ . Dir.createDirectoryIfMissing b
  removeDirectory = unsafeEff_ . Dir.removeDirectory
  removeDirectoryRecursive = unsafeEff_ . Dir.removeDirectoryRecursive
  removePathForcibly = unsafeEff_ . Dir.removePathForcibly
  renameDirectory p = unsafeEff_ . Dir.renameDirectory p
  setCurrentDirectory = unsafeEff_ . Dir.setCurrentDirectory
  withCurrentDirectory p m =
    unsafeEff $ \env -> seqUnliftIO env $
      \runInIO -> Dir.withCurrentDirectory p (runInIO m)
  removeFile = unsafeEff_ . Dir.removeFile
  renameFile p = unsafeEff_ . Dir.renameFile p
  renamePath p = unsafeEff_ . Dir.renamePath p
  copyFile p = unsafeEff_ . Dir.copyFile p
  copyFileWithMetadata p = unsafeEff_ . Dir.copyFileWithMetadata p
  createFileLink p = unsafeEff_ . Dir.createFileLink p
  createDirectoryLink p = unsafeEff_ . Dir.createDirectoryLink p
  removeDirectoryLink = unsafeEff_ . Dir.removeDirectoryLink
  setPermissions p = unsafeEff_ . Dir.setPermissions p
  copyPermissions p = unsafeEff_ . Dir.copyPermissions p
  setAccessTime p = unsafeEff_ . Dir.setAccessTime p
  setModificationTime p = unsafeEff_ . Dir.setModificationTime p

-- | Calls 'removeFile' if 'doesFileExist' is 'True'.
--
-- @since 0.1
removeFileIfExists ::
  ( MonadPathReader m,
    MonadPathWriter m
  ) =>
  OsPath ->
  m ()
removeFileIfExists = removeIfExists doesFileExist removeFile

-- | Calls 'removeDirectory' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryIfExists ::
  ( MonadPathReader m,
    MonadPathWriter m
  ) =>
  OsPath ->
  m ()
removeDirectoryIfExists = removeIfExists doesDirectoryExist removeDirectory

-- | Calls 'removeDirectoryRecursive' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryRecursiveIfExists ::
  ( MonadPathReader m,
    MonadPathWriter m
  ) =>
  OsPath ->
  m ()
removeDirectoryRecursiveIfExists =
  removeIfExists doesDirectoryExist removeDirectoryRecursive

-- | Calls 'removePathForcibly' if 'doesPathExist' is 'True'.
--
-- @since 0.1
removePathForciblyIfExists ::
  ( MonadPathReader m,
    MonadPathWriter m
  ) =>
  OsPath ->
  m ()
removePathForciblyIfExists =
  removeIfExists doesPathExist removePathForcibly

removeIfExists :: (Monad m) => (t -> m Bool) -> (t -> m ()) -> t -> m ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)

-- | 'copyDirectoryRecursiveConfig' with 'Utils.defaultCopyDirConfig'.
--
-- @since 0.1
copyDirectoryRecursive ::
  forall m.
  ( MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadMask m
  ) =>
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  m ()
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
  forall m.
  ( MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadMask m
  ) =>
  -- | Config
  CopyDirConfig ->
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  m ()
copyDirectoryRecursiveConfig = Utils.copyDirectoryRecursiveConfig handle
  where
    handle :: Handle m
    handle =
      MkHandle
        { Utils.newIORef = newIORef,
          Utils.readIORef = readIORef,
          Utils.modifyIORef' = modifyIORef',
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
