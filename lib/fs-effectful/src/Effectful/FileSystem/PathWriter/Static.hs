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
    removeDirectoryIfExists,
    removeDirectoryRecursiveIfExists,
    removePathForciblyIfExists,

    -- ** Symbolic Links
    removeSymbolicLink,
    removeSymbolicLinkIfExists,

    -- * Re-exports
    OsPath,
    Permissions,
    UTCTime (..),
  )
where

import Control.Monad (unless, when)
import Data.Foldable (for_, traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef)
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
import Effectful.Exception (mask_, onException)
import Effectful.FileSystem.PathReader.Static
  ( PathReaderStatic,
    PathType (PathTypeDirectory, PathTypeSymbolicLink),
  )
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Utils
  ( CopyDirConfig (MkCopyDirConfig, overwrite, targetName),
    Overwrite (OverwriteAll, OverwriteDirectories, OverwriteNone),
    TargetName (TargetNameDest, TargetNameLiteral, TargetNameSrc),
  )
import Effectful.FileSystem.PathWriter.Utils qualified as Utils
import Effectful.FileSystem.Utils (OsPath, (</>))
import Effectful.FileSystem.Utils qualified as FS.Utils
import Optics.Core ((^.))
import System.Directory (Permissions)
import System.Directory.OsPath qualified as Dir
import System.IO.Error qualified as Error
import System.OsPath qualified as FP

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
removeFileIfExists = removeIfExists PR.doesFileExist removeFile

-- | Calls 'removeDirectory' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryIfExists ::
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
  ) =>
  OsPath ->
  Eff es ()
removeDirectoryIfExists = removeIfExists PR.doesDirectoryExist removeDirectory

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
  removeIfExists PR.doesDirectoryExist removeDirectoryRecursive

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
  removeIfExists PR.doesPathExist removePathForcibly

-- | Calls 'removeSymbolicLink' if 'doesSymbolicLinkExist' is 'True'.
--
-- @since 0.1
removeSymbolicLinkIfExists ::
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
  ) =>
  OsPath ->
  Eff es ()
removeSymbolicLinkIfExists =
  removeIfExists PR.doesSymbolicLinkExist removeSymbolicLink

removeIfExists :: (OsPath -> Eff es Bool) -> (OsPath -> Eff es ()) -> OsPath -> Eff es ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)

-- | 'copyDirectoryRecursiveConfig' with 'defaultCopyDirConfig'.
--
-- @since 0.1
copyDirectoryRecursive ::
  forall es.
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
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
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
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
              FS.Utils.throwIOError
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
              FS.Utils.throwIOError
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

  copyFiles `onException` mask_ cleanup

copyDirectoryNoOverwrite ::
  forall es.
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
  ) =>
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  Eff es ()
copyDirectoryNoOverwrite src dest = do
  destExists <- PR.doesDirectoryExist dest
  when destExists $
    FS.Utils.throwIOError
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

  copyFiles `onException` mask_ cleanup

-- | Removes a symbolic link. On Windows, attempts to distinguish
-- file and directory links (Posix makes no distinction).
--
-- @since 0.1
removeSymbolicLink ::
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
  ) =>
  OsPath ->
  Eff es ()
removeSymbolicLink p = do
  PR.throwIfWrongPathType "removeSymbolicLink" PathTypeSymbolicLink p

  PR.pathIsSymbolicDirectoryLink p >>= \case
    True -> removeDirectoryLink p
    False -> removeFile p
{-# INLINEABLE removeSymbolicLink #-}

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
  ( PathReaderStatic :> es,
    PathWriterStatic :> es
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
{-# INLINEABLE copySymbolicLink #-}

-- $if-exists
-- The @removeXIfExists@ functions should be understood as helper combinators
-- for the obvious @doesXExist -> removeX@ pattern. They should __not__ be
-- understood as a total "delete arbitrary path if it exists" pattern.
--
-- For instance, 'doesDirectoryExist' will return true if the /target/ of a
-- symbolic link is a directory, yet 'removeDirectory' will throw an exception.
-- Thus these functions should only be used when the type (file, dir, symlink)
-- of a (possibly non-extant) path is __known__.
