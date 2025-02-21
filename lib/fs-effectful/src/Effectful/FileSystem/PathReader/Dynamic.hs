-- | Provides a dynamic effect for the readable portion of "System.Directory"'s
-- interface.
--
-- @since 0.1
module Effectful.FileSystem.PathReader.Dynamic
  ( -- * Effect
    PathReader (..),

    -- ** Functions
    listDirectory,
    getDirectoryContents,
    getCurrentDirectory,
    getHomeDirectory,
    getXdgDirectory,
    getXdgDirectoryList,
    getAppUserDataDirectory,
    getUserDocumentsDirectory,
    getTemporaryDirectory,
    getFileSize,
    canonicalizePath,
    makeAbsolute,
    makeRelativeToCurrentDirectory,
    doesPathExist,
    doesFileExist,
    doesDirectoryExist,
    findExecutable,
    findExecutables,
    findExecutablesInDirectories,
    findFileWith,
    findFilesWith,
    pathIsSymbolicLink,
    getSymbolicLinkTarget,
    getPermissions,
    getAccessTime,
    getModificationTime,

    -- ** Handlers
    runPathReader,

    -- * Functions
    findFile,
    findFiles,

    -- ** XDG Utils
    getXdgData,
    getXdgConfig,
    getXdgCache,
    getXdgState,

    -- * Path Types
    PathType (..),

    -- ** Functions
    PathType.displayPathType,
    getPathType,
    isPathType,
    throwIfWrongPathType,

    -- * Misc
    listDirectoryRecursive,
    listDirectoryRecursiveSymbolicLink,
    doesSymbolicLinkExist,
    pathIsSymbolicDirectoryLink,
    pathIsSymbolicFileLink,

    -- * Re-exports
    OsPath,
    Permissions,
    UTCTime (..),
    XdgDirectory (..),
    XdgDirectoryList (..),
  )
where

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time (UTCTime (UTCTime, utctDay, utctDayTime))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (HasCallStack, localSeqUnlift, reinterpret, send)
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.Exception (catchIO)
import Effectful.FileSystem.PathReader.Static qualified as Static
import FileSystem.IO qualified as IO
import FileSystem.OsPath (OsPath, (</>))
import FileSystem.PathType
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeOther,
        PathTypeSymbolicLink
      ),
  )
import FileSystem.PathType qualified as PathType
import GHC.IO.Exception (IOErrorType (InappropriateType))
import System.Directory
  ( Permissions,
    XdgDirectory (XdgCache, XdgConfig, XdgData, XdgState),
    XdgDirectoryList (XdgConfigDirs, XdgDataDirs),
  )
import System.Directory.OsPath qualified as Dir
import System.IO.Error qualified as IO.Error

-- | Dynamic effect for reading paths.
--
-- @since 0.1
data PathReader :: Effect where
  ListDirectory :: OsPath -> PathReader m [OsPath]
  GetDirectoryContents :: OsPath -> PathReader m [OsPath]
  GetCurrentDirectory :: PathReader m OsPath
  GetHomeDirectory :: PathReader m OsPath
  GetXdgDirectory :: XdgDirectory -> OsPath -> PathReader m OsPath
  GetXdgDirectoryList :: XdgDirectoryList -> PathReader m [OsPath]
  GetAppUserDataDirectory :: OsPath -> PathReader m OsPath
  GetUserDocumentsDirectory :: PathReader m OsPath
  GetTemporaryDirectory :: PathReader m OsPath
  GetFileSize :: OsPath -> PathReader m Integer
  CanonicalizePath :: OsPath -> PathReader m OsPath
  MakeAbsolute :: OsPath -> PathReader m OsPath
  MakeRelativeToCurrentDirectory :: OsPath -> PathReader m OsPath
  DoesPathExist :: OsPath -> PathReader m Bool
  DoesFileExist :: OsPath -> PathReader m Bool
  DoesDirectoryExist :: OsPath -> PathReader m Bool
  FindExecutable :: OsPath -> PathReader m (Maybe OsPath)
  FindExecutables :: OsPath -> PathReader m [OsPath]
  FindExecutablesInDirectories ::
    [OsPath] ->
    OsPath ->
    PathReader m [OsPath]
  FindFile :: [OsPath] -> OsPath -> PathReader m (Maybe OsPath)
  FindFiles :: [OsPath] -> OsPath -> PathReader m [OsPath]
  FindFileWith ::
    (OsPath -> m Bool) ->
    [OsPath] ->
    OsPath ->
    PathReader m (Maybe OsPath)
  FindFilesWith ::
    (OsPath -> m Bool) ->
    [OsPath] ->
    OsPath ->
    PathReader m [OsPath]
  PathIsSymbolicLink :: OsPath -> PathReader m Bool
  GetSymbolicLinkTarget :: OsPath -> PathReader m OsPath
  GetPermissions :: OsPath -> PathReader m Permissions
  GetAccessTime :: OsPath -> PathReader m UTCTime
  GetModificationTime :: OsPath -> PathReader m UTCTime

-- | @since 0.1
type instance DispatchOf PathReader = Dynamic

-- | @since 0.1
instance ShowEffect PathReader where
  showEffectCons = \case
    ListDirectory _ -> "ListDirectory"
    GetDirectoryContents _ -> "GetDirectoryContents"
    GetCurrentDirectory -> "GetCurrentDirectory"
    GetHomeDirectory -> "GetHomeDirectory"
    GetXdgDirectory _ _ -> "GetXdgDirectory"
    GetXdgDirectoryList _ -> "GetXdgDirectoryList"
    GetAppUserDataDirectory _ -> "GetAppUserDataDirectory"
    GetUserDocumentsDirectory -> "GetUserDocumentsDirectory"
    GetTemporaryDirectory -> "GetTemporaryDirectory"
    GetFileSize _ -> "GetFileSize"
    CanonicalizePath _ -> "CanonicalizePath"
    MakeAbsolute _ -> "MakeAbsolute"
    MakeRelativeToCurrentDirectory _ -> "MakeRelativeToCurrentDirectory"
    DoesPathExist _ -> "DoesPathExist"
    DoesFileExist _ -> "DoesFileExist"
    DoesDirectoryExist _ -> "DoesDirectoryExist"
    FindExecutable _ -> "FindExecutable"
    FindExecutables _ -> "FindExecutables"
    FindExecutablesInDirectories _ _ -> "FindExecutablesInDirectories"
    FindFile _ _ -> "FindFile"
    FindFiles _ _ -> "FindFiles"
    FindFileWith _ _ _ -> "FindFileWith"
    FindFilesWith _ _ _ -> "FindFilesWith"
    PathIsSymbolicLink _ -> "PathIsSymbolicLink"
    GetSymbolicLinkTarget _ -> "GetSymbolicLinkTarget"
    GetPermissions _ -> "GetPermissions"
    GetAccessTime _ -> "GetAccessTime"
    GetModificationTime _ -> "GetModificationTime"

-- | Runs 'PathReader' in 'IO'.
--
-- @since 0.1
runPathReader ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (PathReader : es) a ->
  Eff es a
runPathReader = reinterpret Static.runPathReader $ \env -> \case
  ListDirectory p -> Static.listDirectory p
  GetDirectoryContents p -> Static.getDirectoryContents p
  GetCurrentDirectory -> liftIO Dir.getCurrentDirectory
  GetHomeDirectory -> liftIO Dir.getHomeDirectory
  GetXdgDirectory xdg p -> Static.getXdgDirectory xdg p
  GetXdgDirectoryList xdg -> Static.getXdgDirectoryList xdg
  GetAppUserDataDirectory p -> Static.getAppUserDataDirectory p
  GetUserDocumentsDirectory -> liftIO Dir.getUserDocumentsDirectory
  GetTemporaryDirectory -> liftIO Dir.getTemporaryDirectory
  GetFileSize p -> Static.getFileSize p
  CanonicalizePath p -> Static.canonicalizePath p
  MakeAbsolute p -> Static.makeAbsolute p
  MakeRelativeToCurrentDirectory p -> Static.makeRelativeToCurrentDirectory p
  DoesPathExist p -> Static.doesPathExist p
  DoesFileExist p -> Static.doesFileExist p
  DoesDirectoryExist p -> Static.doesDirectoryExist p
  FindExecutable p -> Static.findExecutable p
  FindExecutables p -> Static.findExecutables p
  FindExecutablesInDirectories ps str ->
    Static.findExecutablesInDirectories ps str
  FindFile xs p -> Static.findFile xs p
  FindFiles xs p -> Static.findFiles xs p
  FindFileWith f ps str -> localSeqUnlift env $ \runInStatic ->
    Static.findFileWith (runInStatic . f) ps str
  FindFilesWith f ps str -> localSeqUnlift env $ \runInStatic ->
    Static.findFilesWith (runInStatic . f) ps str
  PathIsSymbolicLink p -> Static.pathIsSymbolicLink p
  GetSymbolicLinkTarget p -> Static.getSymbolicLinkTarget p
  GetPermissions p -> Static.getPermissions p
  GetAccessTime p -> Static.getAccessTime p
  GetModificationTime p -> Static.getModificationTime p

-- | Search through the given list of directories for the given file.
--
-- The behavior is equivalent to 'findFileWith', returning only the first
-- occurrence. Details can be found in the documentation of 'findFileWith'.
--
-- @since 0.1
findFile ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  [OsPath] ->
  OsPath ->
  Eff es (Maybe OsPath)
findFile xs = send . FindFile xs

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
-- The behavior is equivalent to 'findFilesWith'. Details can be found in the
-- documentation of 'findFilesWith'.
--
-- @since 0.1
findFiles ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  [OsPath] ->
  OsPath ->
  Eff es [OsPath]
findFiles xs = send . FindFiles xs

-- | Lifted 'Dir.listDirectory'.
--
-- @since 0.1
listDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es [OsPath]
listDirectory = send . ListDirectory

-- | Lifted 'Dir.getDirectoryContents'.
--
-- @since 0.1
getDirectoryContents ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es [OsPath]
getDirectoryContents = send . GetDirectoryContents

-- | Lifted 'Dir.getCurrentDirectory'.
--
-- @since 0.1
getCurrentDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es OsPath
getCurrentDirectory = send GetCurrentDirectory

-- | Lifted 'Dir.getHomeDirectory'.
--
-- @since 0.1
getHomeDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es OsPath
getHomeDirectory = send GetHomeDirectory

-- | Lifted 'Dir.getXdgDirectory'.
--
-- @since 0.1
getXdgDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  XdgDirectory ->
  OsPath ->
  Eff es OsPath
getXdgDirectory xdg = send . GetXdgDirectory xdg

-- | Lifted 'Dir.getXdgDirectoryList'.
--
-- @since 0.1
getXdgDirectoryList ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  XdgDirectoryList ->
  Eff es [OsPath]
getXdgDirectoryList = send . GetXdgDirectoryList

-- | Lifted 'Dir.getAppUserDataDirectory'.
--
-- @since 0.1
getAppUserDataDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getAppUserDataDirectory = send . GetAppUserDataDirectory

-- | Lifted 'Dir.getUserDocumentsDirectory'.
--
-- @since 0.1
getUserDocumentsDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es OsPath
getUserDocumentsDirectory = send GetUserDocumentsDirectory

-- | Lifted 'Dir.getTemporaryDirectory'.
--
-- @since 0.1
getTemporaryDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es OsPath
getTemporaryDirectory = send GetTemporaryDirectory

-- | Lifted 'Dir.getFileSize'.
--
-- @since 0.1
getFileSize ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Integer
getFileSize = send . GetFileSize

-- | Lifted 'Dir.canonicalizePath'.
--
-- @since 0.1
canonicalizePath ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
canonicalizePath = send . CanonicalizePath

-- | Lifted 'Dir.makeAbsolute'.
--
-- @since 0.1
makeAbsolute ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
makeAbsolute = send . MakeAbsolute

-- | Lifted 'Dir.makeRelativeToCurrentDirectory'.
--
-- @since 0.1
makeRelativeToCurrentDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
makeRelativeToCurrentDirectory = send . MakeRelativeToCurrentDirectory

-- | Lifted 'Dir.doesPathExist'.
--
-- @since 0.1
doesPathExist ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
doesPathExist = send . DoesPathExist

-- | Lifted 'Dir.doesFileExist'.
--
-- @since 0.1
doesFileExist ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
doesFileExist = send . DoesFileExist

-- | Lifted 'Dir.doesDirectoryExist'.
--
-- @since 0.1
doesDirectoryExist ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
doesDirectoryExist = send . DoesDirectoryExist

-- | Lifted 'Dir.findExecutable'.
--
-- @since 0.1
findExecutable ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es (Maybe OsPath)
findExecutable = send . FindExecutable

-- | Lifted 'Dir.findExecutables'.
--
-- @since 0.1
findExecutables ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es [OsPath]
findExecutables = send . FindExecutables

-- | Lifted 'Dir.findExecutablesInDirectories'.
--
-- @since 0.1
findExecutablesInDirectories ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  [OsPath] ->
  OsPath ->
  Eff es [OsPath]
findExecutablesInDirectories ps = send . FindExecutablesInDirectories ps

-- | Lifted 'Dir.findFileWith'.
--
-- @since 0.1
findFileWith ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  (OsPath -> Eff es Bool) ->
  [OsPath] ->
  OsPath ->
  Eff es (Maybe OsPath)
findFileWith f ps = send . FindFileWith f ps

-- | Lifted 'Dir.findFilesWith'.
--
-- @since 0.1
findFilesWith ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  (OsPath -> Eff es Bool) ->
  [OsPath] ->
  OsPath ->
  Eff es [OsPath]
findFilesWith f ps = send . FindFilesWith f ps

-- | Lifted 'Dir.pathIsSymbolicLink'.
--
-- @since 0.1
pathIsSymbolicLink ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
pathIsSymbolicLink = send . PathIsSymbolicLink

-- | Lifted 'Dir.getSymbolicLinkTarget'.
--
-- @since 0.1
getSymbolicLinkTarget ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getSymbolicLinkTarget = send . GetSymbolicLinkTarget

-- | Lifted 'Dir.getPermissions'.
--
-- @since 0.1
getPermissions ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Permissions
getPermissions = send . GetPermissions

-- | Lifted 'Dir.getAccessTime'.
--
-- @since 0.1
getAccessTime ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es UTCTime
getAccessTime = send . GetAccessTime

-- | Lifted 'Dir.getModificationTime'.
--
-- @since 0.1
getModificationTime ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es UTCTime
getModificationTime = send . GetModificationTime

-- | Retrieves the XDG data directory e.g. @~/.local\/share@.
--
-- @since 0.1
getXdgData ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgData = getXdgDirectory XdgData

-- | Retrieves the XDG config directory e.g. @~/.config@.
--
-- @since 0.1
getXdgConfig ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgConfig = getXdgDirectory XdgConfig

-- | Retrieves the XDG cache directory e.g. @~/.cache@.
--
-- @since 0.1
getXdgCache ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgCache = getXdgDirectory XdgCache

-- | Retrieves the XDG state directory e.g. @~/.local\/state@.
--
-- @since 0.1
getXdgState ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgState = getXdgDirectory XdgState

-- | Returns true if the path is a symbolic link. Does not traverse the link.
--
-- @since 0.1
doesSymbolicLinkExist ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
doesSymbolicLinkExist p =
  -- pathIsSymbolicLink throws an exception if the path does not exist,
  -- so we need to handle this. Note that the obvious alternative, prefacing
  -- the call with doesPathExist does not work, as that operates on the link
  -- target. doesFileExist also behaves this way.
  pathIsSymbolicLink p `catchIO` \_ -> pure False

-- | Retrieves the recursive directory contents; splits the sub folders and
-- directories apart.
--
-- @since 0.1
listDirectoryRecursive ::
  forall es.
  ( HasCallStack,
    PathReader :> es
  ) =>
  -- | Root path.
  OsPath ->
  -- | (files, directories)
  Eff es ([OsPath], [OsPath])
listDirectoryRecursive root = recurseDirs [emptyPath]
  where
    recurseDirs :: [OsPath] -> Eff es ([OsPath], [OsPath])
    recurseDirs [] = pure ([], [])
    recurseDirs (d : ds) = do
      (files, dirs) <- splitPaths root d [] [] =<< listDirectory (root </> d)
      (files', dirs') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs')
    emptyPath = mempty

-- | Like 'listDirectoryRecursive' except symbolic links are not traversed
-- i.e. they are returned separately.
--
-- @since 0.1
listDirectoryRecursiveSymbolicLink ::
  forall es.
  ( HasCallStack,
    PathReader :> es
  ) =>
  -- | Root path.
  OsPath ->
  -- | (files, directories, symbolic links)
  Eff es ([OsPath], [OsPath], [OsPath])
listDirectoryRecursiveSymbolicLink root = recurseDirs [emptyPath]
  where
    recurseDirs :: [OsPath] -> Eff es ([OsPath], [OsPath], [OsPath])
    recurseDirs [] = pure ([], [], [])
    recurseDirs (d : ds) = do
      (files, dirs, symlinks) <-
        splitPathsSymboliclink root d [] [] [] =<< listDirectory (root </> d)
      (files', dirs', symlinks') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs', symlinks ++ symlinks')
    emptyPath = mempty

splitPaths ::
  forall es.
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  OsPath ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  Eff es ([OsPath], [OsPath])
splitPaths root d = go
  where
    go :: [OsPath] -> [OsPath] -> [OsPath] -> Eff es ([OsPath], [OsPath])
    go files dirs [] = pure (reverse files, reverse dirs)
    go files dirs (p : ps) = do
      let dirEntry = d </> p
      isDir <- doesDirectoryExist (root </> dirEntry)
      if isDir
        then go files (dirEntry : dirs) ps
        else go (dirEntry : files) dirs ps

splitPathsSymboliclink ::
  forall es.
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  OsPath ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  Eff es ([OsPath], [OsPath], [OsPath])
splitPathsSymboliclink root d = go
  where
    go :: [OsPath] -> [OsPath] -> [OsPath] -> [OsPath] -> Eff es ([OsPath], [OsPath], [OsPath])
    go files dirs symlinks [] = pure (reverse files, reverse dirs, symlinks)
    go files dirs symlinks (p : ps) = do
      let dirEntry = d </> p
          fullPath = root </> dirEntry

      isSymlink <- doesSymbolicLinkExist fullPath
      if isSymlink
        then go files dirs (dirEntry : symlinks) ps
        else do
          isDir <- doesDirectoryExist fullPath
          if isDir
            then go files (dirEntry : dirs) symlinks ps
            else go (dirEntry : files) dirs symlinks ps

-- | Like 'pathIsSymbolicDirectoryLink' but for files.
--
-- @since 0.1
pathIsSymbolicFileLink ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
pathIsSymbolicFileLink = getSymbolicLinkTarget >=> doesFileExist

-- | Returns true if @p@ is a symbolic link and it points to an extant
-- directory. Throws an exception if the path is not a symbolic link or the
-- target does not exist.
--
-- This function and 'pathIsSymbolicFileLink' are intended to distinguish file
-- and directory links on Windows. This matters for knowing when to use:
--
--     - @createFileLink@ vs. @createDirectoryLink@
--     - @removeFile@ vs. @removeDirectoryLink@
--
-- Suppose we want to copy an arbitrary path @p@. We first determine that
-- @p@ is a symlink via 'doesSymbolicLinkExist'. If
-- 'pathIsSymbolicDirectoryLink' returns true then we know we should use
-- "Effects.FileSystem.PathWriter"'s @createDirectoryLink@. Otherwise we can
-- fall back to @createFileLink@.
--
-- Because this relies on the symlink's target, this is best effort, and it is
-- possible 'pathIsSymbolicDirectoryLink' and 'pathIsSymbolicFileLink' both
-- return false.
--
-- Note that Posix makes no distinction between file and directory symbolic
-- links. Thus if your system only has to work on Posix, you probably don't
-- need this function.
--
-- @since 0.1
pathIsSymbolicDirectoryLink ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
pathIsSymbolicDirectoryLink = getSymbolicLinkTarget >=> doesDirectoryExist

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- @since 0.1
throwIfWrongPathType ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  -- | The location for the thrown exception (e.g. function name)
  String ->
  -- | Expected path type
  PathType ->
  -- | Path
  OsPath ->
  Eff es ()
throwIfWrongPathType location expected path = do
  actual <- getPathType path

  let err =
        mconcat
          [ "Expected path to have type ",
            PathType.displayPathType expected,
            ", but detected ",
            PathType.displayPathType actual
          ]

  unless (expected == actual) $
    IO.throwPathIOError
      path
      location
      InappropriateType
      err

-- | Checks that the path type matches the expectation. Throws
-- 'IOException' if the path does not exist or the type cannot be detected.
--
-- @since 0.1
isPathType ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  -- | Expected path type.
  PathType ->
  -- Path.
  OsPath ->
  Eff es Bool
isPathType expected = fmap (== expected) . getPathType

-- | Returns the type for a given path without following symlinks.
-- Throws 'IOException' if the path does not exist or the type cannot be
-- detected.
--
-- @since 0.1
getPathType ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es PathType
getPathType path = do
  -- This needs to be first as does(Directory|File|Path)Exist acts on the target.
  symlinkExists <- doesSymbolicLinkExist path
  if symlinkExists
    then pure PathTypeSymbolicLink
    else do
      dirExists <- doesDirectoryExist path
      if dirExists
        then pure PathTypeDirectory
        else do
          fileExists <- doesFileExist path
          if fileExists
            then pure PathTypeFile
            else do
              pathExists <- doesPathExist path
              if pathExists
                then pure PathTypeOther
                else
                  IO.throwPathIOError
                    path
                    "getPathType"
                    IO.Error.doesNotExistErrorType
                    "path does not exist"
