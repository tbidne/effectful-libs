{-# LANGUAGE UndecidableInstances #-}

-- | Provides a dynamic effect for the writable portion of "System.Directory"'s
-- interface.
--
-- @since 0.1
module Effectful.FileSystem.PathWriter.Dynamic
  ( -- * Effect
    PathWriter (..),
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
    runPathWriter,

    -- * Copying

    -- ** Config
    CopyDirConfig (..),
    Overwrite (..),
    TargetName (..),
    Utils.defaultCopyDirConfig,

    -- ** Functions
    copyDirectoryRecursive,
    copyDirectoryRecursiveConfig,
    copySymbolicLink,

    -- ** Optics
    Utils._OverwriteNone,
    Utils._OverwriteDirectories,
    Utils._OverwriteAll,
    Utils._TargetNameSrc,
    Utils._TargetNameLiteral,
    Utils._TargetNameDest,

    -- * Removing
    -- $if-exists
    removeFileIfExists,
    removeFileIfExists_,
    removeDirectoryIfExists,
    removeDirectoryIfExists_,
    removeDirectoryRecursiveIfExists,
    removeDirectoryRecursiveIfExists_,
    removePathForciblyIfExists,
    removePathForciblyIfExists_,

    -- ** Symbolic Links
    removeSymbolicLink,
    removeSymbolicLinkIfExists,
    removeSymbolicLinkIfExists_,

    -- * Re-exports
    OsPath,
    IOException,
    Permissions,
    UTCTime (..),
  )
where

import Control.Exception (IOException)
import Control.Exception.Utils (onSyncException)
import Control.Monad (unless, void, when)
import Data.Foldable (for_, traverse_)
import Data.Functor (($>))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time (UTCTime (UTCTime, utctDay, utctDayTime))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (localSeqUnlift, reinterpret, send)
import Effectful.Dispatch.Static (HasCallStack, unsafeEff_)
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.Exception (mask_)
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReader,
    PathType (PathTypeDirectory, PathTypeSymbolicLink),
  )
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as Static
import Effectful.FileSystem.PathWriter.Utils
  ( CopyDirConfig (MkCopyDirConfig, overwrite, targetName),
    Overwrite (OverwriteAll, OverwriteDirectories, OverwriteNone),
    TargetName (TargetNameDest, TargetNameLiteral, TargetNameSrc),
  )
import Effectful.FileSystem.PathWriter.Utils qualified as Utils
import FileSystem.IO qualified as FS.IO
import FileSystem.OsPath (OsPath, (</>))
import Optics.Core ((^.))
import System.Directory (Permissions)
import System.IO.Error qualified as Error
import System.OsPath qualified as FP

-- | Effect for writing paths.
--
-- @since 0.1
data PathWriter :: Effect where
  CreateDirectory :: OsPath -> PathWriter m ()
  CreateDirectoryIfMissing :: Bool -> OsPath -> PathWriter m ()
  RemoveDirectory :: OsPath -> PathWriter m ()
  RemoveDirectoryRecursive :: OsPath -> PathWriter m ()
  RemovePathForcibly :: OsPath -> PathWriter m ()
  RenameDirectory :: OsPath -> OsPath -> PathWriter m ()
  SetCurrentDirectory :: OsPath -> PathWriter m ()
  WithCurrentDirectory :: OsPath -> m a -> PathWriter m a
  RemoveFile :: OsPath -> PathWriter m ()
  RenameFile :: OsPath -> OsPath -> PathWriter m ()
  RenamePath :: OsPath -> OsPath -> PathWriter m ()
  CopyFile :: OsPath -> OsPath -> PathWriter m ()
  CopyFileWithMetadata :: OsPath -> OsPath -> PathWriter m ()
  CreateFileLink :: OsPath -> OsPath -> PathWriter m ()
  CreateDirectoryLink :: OsPath -> OsPath -> PathWriter m ()
  RemoveDirectoryLink :: OsPath -> PathWriter m ()
  SetPermissions :: OsPath -> Permissions -> PathWriter m ()
  CopyPermissions :: OsPath -> OsPath -> PathWriter m ()
  SetAccessTime :: OsPath -> UTCTime -> PathWriter m ()
  SetModificationTime :: OsPath -> UTCTime -> PathWriter m ()

-- | @since 0.1
type instance DispatchOf PathWriter = Dynamic

-- | @since 0.1
instance ShowEffect PathWriter where
  showEffectCons = \case
    CreateDirectory _ -> "CreateDirectory"
    CreateDirectoryIfMissing _ _ -> "CreateDirectoryIfMissing"
    RemoveDirectory _ -> "RemoveDirectory"
    RemoveDirectoryRecursive _ -> "RemoveDirectoryRecursive"
    RemovePathForcibly _ -> "RemovePathForcibly"
    RenameDirectory _ _ -> "RenameDirectory"
    SetCurrentDirectory _ -> "SetCurrentDirectory"
    WithCurrentDirectory _ _ -> "WithCurrentDirectory"
    RemoveFile _ -> "RemoveFile"
    RenameFile _ _ -> "RenameFile"
    RenamePath _ _ -> "RenamePath"
    CopyFile _ _ -> "CopyFile"
    CopyFileWithMetadata _ _ -> "CopyFileWithMetadata"
    CreateFileLink _ _ -> "CreateFileLink"
    CreateDirectoryLink _ _ -> "CreateDirectoryLink"
    RemoveDirectoryLink _ -> "RemoveDirectoryLink"
    SetPermissions _ _ -> "SetPermissions"
    CopyPermissions _ _ -> "CopyPermissions"
    SetAccessTime _ _ -> "SetAccessTime"
    SetModificationTime _ _ -> "SetModificationTime"

-- | Runs 'PathWriter' in 'IO'.
--
-- @since 0.1
runPathWriter ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (PathWriter : es) a ->
  Eff es a
runPathWriter = reinterpret Static.runPathWriter $ \env -> \case
  CreateDirectory p -> Static.createDirectory p
  CreateDirectoryIfMissing b p -> Static.createDirectoryIfMissing b p
  RemoveDirectory p -> Static.removeDirectory p
  RemoveDirectoryRecursive p -> Static.removeDirectoryRecursive p
  RemovePathForcibly p -> Static.removePathForcibly p
  RenameDirectory p p' -> Static.renameDirectory p p'
  SetCurrentDirectory p -> Static.setCurrentDirectory p
  WithCurrentDirectory p m -> localSeqUnlift env $ \runInStatic ->
    Static.withCurrentDirectory p (runInStatic m)
  RemoveFile p -> Static.removeFile p
  RenameFile p p' -> Static.renameFile p p'
  RenamePath p p' -> Static.renamePath p p'
  CopyFile p p' -> Static.copyFile p p'
  CopyFileWithMetadata p p' -> Static.copyFileWithMetadata p p'
  CreateFileLink p p' -> Static.createFileLink p p'
  CreateDirectoryLink p p' -> Static.createDirectoryLink p p'
  RemoveDirectoryLink p -> Static.removeDirectoryLink p
  SetPermissions p ps -> Static.setPermissions p ps
  CopyPermissions p ps -> Static.copyPermissions p ps
  SetAccessTime p t -> Static.setAccessTime p t
  SetModificationTime p t -> Static.setModificationTime p t

-- | Lifted 'Dir.createDirectory'.
--
-- @since 0.1
createDirectory ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  Eff es ()
createDirectory = send . CreateDirectory

-- | Lifted 'Dir.createDirectoryIfMissing'.
--
-- @since 0.1
createDirectoryIfMissing ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  Bool ->
  OsPath ->
  Eff es ()
createDirectoryIfMissing b = send . CreateDirectoryIfMissing b

-- | Lifted 'Dir.removeDirectory'.
--
-- @since 0.1
removeDirectory ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  Eff es ()
removeDirectory = send . RemoveDirectory

-- | Lifted 'Dir.removeDirectoryRecursive'.
--
-- @since 0.1
removeDirectoryRecursive ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  Eff es ()
removeDirectoryRecursive = send . RemoveDirectoryRecursive

-- | Lifted 'Dir.removePathForcibly'.
--
-- @since 0.1
removePathForcibly ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  Eff es ()
removePathForcibly = send . RemovePathForcibly

-- | Lifted 'Dir.renameDirectory'.
--
-- @since 0.1
renameDirectory ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
renameDirectory p = send . RenameDirectory p

-- | Lifted 'Dir.setCurrentDirectory'.
--
-- @since 0.1
setCurrentDirectory ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  Eff es ()
setCurrentDirectory = send . SetCurrentDirectory

-- | Lifted 'Dir.withCurrentDirectory'.
--
-- @since 0.1
withCurrentDirectory ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  Eff es a ->
  Eff es a
withCurrentDirectory p = send . WithCurrentDirectory p

-- | Lifted 'Dir.removeFile'.
--
-- @since 0.1
removeFile ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  Eff es ()
removeFile = send . RemoveFile

-- | Lifted 'Dir.renameFile'.
--
-- @since 0.1
renameFile ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
renameFile p = send . RenameFile p

-- | Lifted 'Dir.renamePath'.
--
-- @since 0.1
renamePath ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
renamePath p = send . RenamePath p

-- | Lifted 'Dir.copyFile'.
--
-- @since 0.1
copyFile ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyFile p = send . CopyFile p

-- | Lifted 'Dir.copyFileWithMetadata'.
--
-- @since 0.1
copyFileWithMetadata ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyFileWithMetadata p = send . CopyFileWithMetadata p

-- | Lifted 'Dir.createFileLink'.
--
-- @since 0.1
createFileLink ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
createFileLink p = send . CreateFileLink p

-- | Lifted 'Dir.createDirectoryLink'.
--
-- @since 0.1
createDirectoryLink ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
createDirectoryLink p = send . CreateDirectoryLink p

-- | Lifted 'Dir.removeDirectoryLink'.
--
-- @since 0.1
removeDirectoryLink ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  Eff es ()
removeDirectoryLink = send . RemoveDirectoryLink

-- | Lifted 'Dir.setPermissions'.
--
-- @since 0.1
setPermissions ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  Permissions ->
  Eff es ()
setPermissions p = send . SetPermissions p

-- | Lifted 'Dir.copyPermissions'.
--
-- @since 0.1
copyPermissions ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  OsPath ->
  Eff es ()
copyPermissions p = send . CopyPermissions p

-- | Lifted 'Dir.setAccessTime'.
--
-- @since 0.1
setAccessTime ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  UTCTime ->
  Eff es ()
setAccessTime p = send . SetAccessTime p

-- | Lifted 'Dir.setModificationTime'.
--
-- @since 0.1
setModificationTime ::
  ( HasCallStack,
    PathWriter :> es
  ) =>
  OsPath ->
  UTCTime ->
  Eff es ()
setModificationTime p = send . SetModificationTime p

-- | Variant of 'removeFileIfExists' that ignores the return value.
--
-- @since 0.1
removeFileIfExists_ ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | .
  OsPath ->
  Eff es ()
removeFileIfExists_ = void . removeFileIfExists

-- | Calls 'removeFile' if 'doesFileExist' is 'True'.
--
-- @since 0.1
removeFileIfExists ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | .
  OsPath ->
  Eff es Bool
removeFileIfExists = removeIfExists PR.doesFileExist removeFile

-- | Variant of 'removeDirectoryIfExists' that ignores the return value.
--
-- @since 0.1
removeDirectoryIfExists_ ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | .
  OsPath ->
  Eff es ()
removeDirectoryIfExists_ = void . removeDirectoryIfExists

-- | Calls 'removeDirectory' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryIfExists ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | .
  OsPath ->
  Eff es Bool
removeDirectoryIfExists = removeIfExists PR.doesDirectoryExist removeDirectory

-- | Variant of 'removeDirectoryRecursiveIfExists' that ignores the return value.
--
-- @since 0.1
removeDirectoryRecursiveIfExists_ ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | .
  OsPath ->
  Eff es ()
removeDirectoryRecursiveIfExists_ = void . removeDirectoryRecursiveIfExists

-- | Calls 'removeDirectoryRecursive' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryRecursiveIfExists ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | .
  OsPath ->
  Eff es Bool
removeDirectoryRecursiveIfExists =
  removeIfExists PR.doesDirectoryExist removeDirectoryRecursive

-- | Variant of 'removePathForciblyIfExists' that ignores the return value.
--
-- @since 0.1
removePathForciblyIfExists_ ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | .
  OsPath ->
  Eff es ()
removePathForciblyIfExists_ = void . removePathForciblyIfExists

-- | Calls 'removePathForcibly' if 'doesPathExist' is 'True'.
--
-- @since 0.1
removePathForciblyIfExists ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | .
  OsPath ->
  Eff es Bool
removePathForciblyIfExists =
  removeIfExists PR.doesPathExist removePathForcibly

-- | Variant of 'removeSymbolicLinkIfExists' that ignores the return value.
--
-- @since 0.1
removeSymbolicLinkIfExists_ ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | .
  OsPath ->
  Eff es ()
removeSymbolicLinkIfExists_ = void . removeSymbolicLinkIfExists

-- | Calls 'removeSymbolicLink' if 'doesSymbolicLinkExist' is 'True'.
--
-- @since 0.1
removeSymbolicLinkIfExists ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | .
  OsPath ->
  Eff es Bool
removeSymbolicLinkIfExists =
  removeIfExists PR.doesSymbolicLinkExist removeSymbolicLink

removeIfExists ::
  (OsPath -> Eff es Bool) ->
  (OsPath -> Eff es ()) ->
  OsPath ->
  Eff es Bool
removeIfExists existsFn deleteFn f = do
  exists <- existsFn f
  if exists
    then deleteFn f $> True
    else pure False

-- | 'copyDirectoryRecursiveConfig' with 'defaultCopyDirConfig'.
--
-- @since 0.1
copyDirectoryRecursive ::
  forall es.
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  Eff es ()
copyDirectoryRecursive = copyDirectoryRecursiveConfig Utils.defaultCopyDirConfig

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
-- * 'PathNotFoundException': if @dest@ does not exist.
-- * 'PathFoundException':
--
--     * 'OverwriteNone' and @dest/\<src\>@ exists.
--     * 'OverwriteDirectories' and some @dest/\<target\>\/p@ would be
--        overwritten.
--
-- @since 0.1
copyDirectoryRecursiveConfig ::
  forall es.
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | Config
  CopyDirConfig ->
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  Eff es ()
copyDirectoryRecursiveConfig config src destRoot = do
  PR.throwIfWrongPathType "copyDirectoryRecursiveConfig" PathTypeDirectory src
  PR.throwIfWrongPathType "copyDirectoryRecursiveConfig" PathTypeDirectory destRoot

  let dest = case config ^. #targetName of
        -- Use source directory's name
        TargetNameSrc ->
          let -- Previously we used takeBaseName, but this caused a bug
              -- where e.g. dir-1.0.0 -> dir-1.0 (i.e. the last dot was treated
              -- as an extension, that takeBaseName removes).
              --
              -- splitFileName seems to do what we want e.g.
              --
              -- (/path/to/, dir-1.0.0) === splitFileName /path/to/dir-1.0.0
              --
              -- Note that dropTrailingPathSeparator needs to be used first
              -- to ensure correctness.
              --
              -- This also caused a bug where hidden directories were copied
              -- incorrectly.
              (_, name) = FP.splitFileName (FP.dropTrailingPathSeparator src)
           in destRoot </> name
        -- Use the give name
        TargetNameLiteral p -> destRoot </> p
        -- Use dest itself (i.e. top-level copy)
        TargetNameDest -> destRoot

  case config ^. #overwrite of
    OverwriteNone -> copyDirectoryNoOverwrite src dest
    OverwriteDirectories -> copyDirectoryOverwrite False src dest
    OverwriteAll -> copyDirectoryOverwrite True src dest

copyDirectoryOverwrite ::
  forall es.
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
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

  copiedFilesRef <- unsafeEff_ $ newIORef []
  createdDirsRef <- unsafeEff_ $ newIORef []
  copiedSymlinksRef <- unsafeEff_ $ newIORef []

  destExists <- PR.doesDirectoryExist dest

  let checkFileOverwrites =
        if not overwriteFiles
          then \f -> do
            exists <- PR.doesFileExist f
            when exists $
              FS.IO.throwPathIOError
                f
                "copyDirectoryOverwrite"
                Error.alreadyExistsErrorType
                "Attempted file overwrite when CopyDirConfig.overwriteFiles is false"
          else const (pure ())

      checkSymlinkOverwrites =
        if not overwriteFiles
          then \f -> do
            exists <- PR.doesSymbolicLinkExist f
            when exists $
              FS.IO.throwPathIOError
                f
                "copyDirectoryOverwrite"
                Error.alreadyExistsErrorType
                "Attempted symlink overwrite when CopyDirConfig.overwriteFiles is false"
          else const (pure ())

      copyFiles = do
        (subFiles, subDirs, symlinks) <- PR.listDirectoryRecursiveSymbolicLink src

        -- Create dest if it does not exist. Do not need to save dir
        -- in createdDirsRef IORef as it will be correctly deleted by
        -- removeDirectoryRecursive if necessary.
        unless destExists $ createDirectory dest

        -- create the parent directories
        for_ subDirs $ \d -> do
          let d' = dest </> d
          dExists <- PR.doesDirectoryExist d'
          unless dExists $ do
            createDirectoryIfMissing False d'
            unsafeEff_ $ modifyIORef' createdDirsRef (d' :)

        -- copy files
        for_ subFiles $ \f -> do
          let f' = dest </> f
          checkFileOverwrites f'
          copyFileWithMetadata (src </> f) f'
          unsafeEff_ $ modifyIORef' copiedFilesRef (f' :)

        -- copy symlinks
        for_ symlinks $ \s -> do
          let s' = dest </> s
          checkSymlinkOverwrites s'
          copySymbolicLink (src </> s) s'
          unsafeEff_ $ modifyIORef' copiedSymlinksRef (s' :)

      cleanup =
        if destExists
          then do
            -- manually delete files and dirs
            unsafeEff_ (readIORef copiedFilesRef) >>= traverse_ removeFile
            unsafeEff_ (readIORef createdDirsRef) >>= traverse_ removeDirectory
            unsafeEff_ (readIORef copiedSymlinksRef) >>= traverse_ removeSymbolicLink
          else removeDirectoryRecursive dest

  copyFiles `onSyncException` mask_ cleanup

copyDirectoryNoOverwrite ::
  forall es.
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  Eff es ()
copyDirectoryNoOverwrite src dest = do
  destExists <- PR.doesDirectoryExist dest
  when destExists $
    FS.IO.throwPathIOError
      dest
      "copyDirectoryNoOverwrite"
      Error.alreadyExistsErrorType
      "Attempted directory overwrite when CopyDirConfig.overwrite is OverwriteNone"

  let copyFiles = do
        (subFiles, subDirs, symlinks) <- PR.listDirectoryRecursiveSymbolicLink src
        createDirectory dest

        -- create intermediate dirs if they do not exist
        traverse_ (createDirectoryIfMissing True . (dest </>)) subDirs

        -- copy files
        for_ subFiles $ \f -> copyFileWithMetadata (src </> f) (dest </> f)

        -- copy symlinks
        for_ symlinks $ \s -> copySymbolicLink (src </> s) (dest </> s)

      -- delete directory
      cleanup = removeDirectoryRecursive dest

  copyFiles `onSyncException` mask_ cleanup

-- | Removes a symbolic link. On Windows, attempts to distinguish
-- file and directory links (Posix makes no distinction).
--
-- @since 0.1
removeSymbolicLink ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  OsPath ->
  Eff es ()
removeSymbolicLink p = do
  PR.throwIfWrongPathType "removeSymbolicLink" PathTypeSymbolicLink p

  PR.pathIsSymbolicDirectoryLink p >>= \case
    True -> removeDirectoryLink p
    False -> removeFile p

-- | Copies the symbolic link /without/ traversing the link i.e. copy the
-- link itself. Does not throw an exception if the target does exist.
-- Throws an @IOException@ if the path is not a symbolic link.
--
-- __Windows:__ We have to distinguish between file and directory links
-- (Posix makes no such distinction). If the target does not exist or is
-- not considered a directory (e.g. it could also be a link), we fall back
-- to creating a file link.
--
-- @since 0.1
copySymbolicLink ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | Source
  OsPath ->
  -- | Dest
  OsPath ->
  Eff es ()
copySymbolicLink src dest = do
  PR.throwIfWrongPathType "copySymbolicLink" PathTypeSymbolicLink src

  target <- PR.getSymbolicLinkTarget src

  -- NOTE: The distinction between a directory vs. file link does not exist
  -- for Posix, so this logic is for Windows. We test if the target exists
  -- and is a directory, in which case we use createDirectoryLink. If the
  -- target is a file, symlink itself, or does not exist, we fall back to
  -- createFileLink.
  PR.pathIsSymbolicDirectoryLink src >>= \case
    True -> createDirectoryLink target dest
    False -> createFileLink target dest

-- $if-exists
-- The @removeXIfExists@ functions should be understood as helper combinators
-- for the obvious @doesXExist -> removeX@ pattern. They should __not__ be
-- understood as a total "delete arbitrary path if it exists" pattern.
--
-- For instance, 'doesDirectoryExist' will return true if the /target/ of a
-- symbolic link is a directory, yet 'removeDirectory' will throw an exception.
-- Thus these functions should only be used when the type (file, dir, symlink)
-- of a (possibly non-extant) path is __known__.
