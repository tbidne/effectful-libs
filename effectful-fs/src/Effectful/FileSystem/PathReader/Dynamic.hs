{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Provides an effect for reading paths.
--
-- @since 0.1
module Effectful.FileSystem.PathReader.Dynamic
  ( -- * Effect
    PathReaderDynamic (..),
    Path,
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
    runPathReaderDynamicIO,

    -- * Functions
    findFile,
    findFiles,

    -- ** XDG Utils
    getXdgData,
    getXdgConfig,
    getXdgCache,
#if MIN_VERSION_directory(1,3,7)
    getXdgState,
#endif

    -- * Misc
    listDirectoryRecursive,

    -- * Re-exports
    XdgDirectory (..),
    XdgDirectoryList (..),
    Permissions (..),
    UTCTime (..),
  )
where

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
import Effectful.FileSystem.Internal ( Path, (</>) )
import System.Directory
  ( Permissions (..),
    XdgDirectory (..),
    XdgDirectoryList (..),
  )
#if MIN_VERSION_filepath(1,4,100) && MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath qualified as Dir
#else
import System.Directory qualified as Dir
#endif

{- ORMOLU_ENABLE -}

-- | Effect for reading paths.
--
-- @since 0.1
data PathReaderDynamic :: Effect where
  ListDirectory :: Path -> PathReaderDynamic m [Path]
  GetDirectoryContents :: Path -> PathReaderDynamic m [Path]
  GetCurrentDirectory :: PathReaderDynamic m Path
  GetHomeDirectory :: PathReaderDynamic m Path
  GetXdgDirectory :: XdgDirectory -> Path -> PathReaderDynamic m Path
  GetXdgDirectoryList :: XdgDirectoryList -> PathReaderDynamic m [Path]
  GetAppUserDataDirectory :: Path -> PathReaderDynamic m Path
  GetUserDocumentsDirectory :: PathReaderDynamic m Path
  GetTemporaryDirectory :: PathReaderDynamic m Path
  GetFileSize :: Path -> PathReaderDynamic m Integer
  CanonicalizePath :: Path -> PathReaderDynamic m Path
  MakeAbsolute :: Path -> PathReaderDynamic m Path
  MakeRelativeToCurrentDirectory :: Path -> PathReaderDynamic m Path
  DoesPathExist :: Path -> PathReaderDynamic m Bool
  DoesFileExist :: Path -> PathReaderDynamic m Bool
  DoesDirectoryExist :: Path -> PathReaderDynamic m Bool
  FindExecutable :: String -> PathReaderDynamic m (Maybe Path)
  FindExecutables :: String -> PathReaderDynamic m [Path]
  FindExecutablesInDirectories :: [Path] -> String -> PathReaderDynamic m [Path]
  FindFileWith :: (Path -> m Bool) -> [Path] -> String -> PathReaderDynamic m (Maybe Path)
  FindFilesWith :: (Path -> m Bool) -> [Path] -> String -> PathReaderDynamic m [Path]
  PathIsSymbolicLink :: Path -> PathReaderDynamic m Bool
  GetSymbolicLinkTarget :: Path -> PathReaderDynamic m Path
  GetPermissions :: Path -> PathReaderDynamic m Permissions
  GetAccessTime :: Path -> PathReaderDynamic m UTCTime
  GetModificationTime :: Path -> PathReaderDynamic m UTCTime

-- | @since 0.1
type instance DispatchOf PathReaderDynamic = Dynamic

-- | Runs 'PathReaderDynamic' in 'IO'.
--
-- @since 0.1
runPathReaderDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (PathReaderDynamic : es) a ->
  Eff es a
runPathReaderDynamicIO = interpret $ \env -> \case
  ListDirectory p -> liftIO $ Dir.listDirectory p
  GetDirectoryContents p -> liftIO $ Dir.getDirectoryContents p
  GetCurrentDirectory -> liftIO Dir.getCurrentDirectory
  GetHomeDirectory -> liftIO Dir.getHomeDirectory
  GetXdgDirectory xdg p -> liftIO $ Dir.getXdgDirectory xdg p
  GetXdgDirectoryList xdg -> liftIO $ Dir.getXdgDirectoryList xdg
  GetAppUserDataDirectory p -> liftIO $ Dir.getAppUserDataDirectory p
  GetUserDocumentsDirectory -> liftIO Dir.getUserDocumentsDirectory
  GetTemporaryDirectory -> liftIO Dir.getTemporaryDirectory
  GetFileSize p -> liftIO $ Dir.getFileSize p
  CanonicalizePath p -> liftIO $ Dir.canonicalizePath p
  MakeAbsolute p -> liftIO $ Dir.makeAbsolute p
  MakeRelativeToCurrentDirectory p -> liftIO $ Dir.makeRelativeToCurrentDirectory p
  DoesPathExist p -> liftIO $ Dir.doesPathExist p
  DoesFileExist p -> liftIO $ Dir.doesFileExist p
  DoesDirectoryExist p -> liftIO $ Dir.doesDirectoryExist p
  FindExecutable p -> liftIO $ Dir.findExecutable p
  FindExecutables p -> liftIO $ Dir.findExecutables p
  FindExecutablesInDirectories ps str -> liftIO $ Dir.findExecutablesInDirectories ps str
  FindFileWith f ps str -> localSeqUnliftIO env $ \runInDynamicIO ->
    liftIO $ Dir.findFileWith (runInDynamicIO . f) ps str
  FindFilesWith f ps str -> localSeqUnliftIO env $ \runInDynamicIO ->
    liftIO $ Dir.findFilesWith (runInDynamicIO . f) ps str
  PathIsSymbolicLink p -> liftIO $ Dir.pathIsSymbolicLink p
  GetSymbolicLinkTarget p -> liftIO $ Dir.getSymbolicLinkTarget p
  GetPermissions p -> liftIO $ Dir.getPermissions p
  GetAccessTime p -> liftIO $ Dir.getAccessTime p
  GetModificationTime p -> liftIO $ Dir.getModificationTime p

-- | Search through the given list of directories for the given file.
--
-- The behavior is equivalent to 'findFileWith', returning only the first
-- occurrence. Details can be found in the documentation of 'findFileWith'.
--
-- @since 0.1
findFile :: (PathReaderDynamic :> es) => [Path] -> Path -> Eff es (Maybe Path)
findFile = findFileWith (\_ -> pure True)

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
-- The behavior is equivalent to 'findFilesWith'. Details can be found in the
-- documentation of 'findFilesWith'.
--
-- @since 0.1
findFiles :: (PathReaderDynamic :> es) => [Path] -> Path -> Eff es [Path]
findFiles = findFilesWith (\_ -> pure True)

-- | Lifted 'Dir.listDirectory'.
--
-- @since 0.1
listDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es [Path]
listDirectory = send . ListDirectory

-- | Lifted 'Dir.getDirectoryContents'.
--
-- @since 0.1
getDirectoryContents ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es [Path]
getDirectoryContents = send . GetDirectoryContents

-- | Lifted 'Dir.getCurrentDirectory'.
--
-- @since 0.1
getCurrentDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  Eff es Path
getCurrentDirectory = send GetCurrentDirectory

-- | Lifted 'Dir.getHomeDirectory'.
--
-- @since 0.1
getHomeDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  Eff es Path
getHomeDirectory = send GetHomeDirectory

-- | Lifted 'Dir.getXdgDirectory'.
--
-- @since 0.1
getXdgDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  XdgDirectory ->
  Path ->
  Eff es Path
getXdgDirectory xdg = send . GetXdgDirectory xdg

-- | Lifted 'Dir.getXdgDirectoryList'.
--
-- @since 0.1
getXdgDirectoryList ::
  ( PathReaderDynamic :> es
  ) =>
  XdgDirectoryList ->
  Eff es [Path]
getXdgDirectoryList = send . GetXdgDirectoryList

-- | Lifted 'Dir.getAppUserDataDirectory'.
--
-- @since 0.1
getAppUserDataDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Path
getAppUserDataDirectory = send . GetAppUserDataDirectory

-- | Lifted 'Dir.getUserDocumentsDirectory'.
--
-- @since 0.1
getUserDocumentsDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  Eff es Path
getUserDocumentsDirectory = send GetUserDocumentsDirectory

-- | Lifted 'Dir.getTemporaryDirectory'.
--
-- @since 0.1
getTemporaryDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  Eff es Path
getTemporaryDirectory = send GetTemporaryDirectory

-- | Lifted 'Dir.getFileSize'.
--
-- @since 0.1
getFileSize ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Integer
getFileSize = send . GetFileSize

-- | Lifted 'Dir.canonicalizePath'.
--
-- @since 0.1
canonicalizePath ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Path
canonicalizePath = send . CanonicalizePath

-- | Lifted 'Dir.makeAbsolute'.
--
-- @since 0.1
makeAbsolute ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Path
makeAbsolute = send . MakeAbsolute

-- | Lifted 'Dir.makeRelativeToCurrentDirectory'.
--
-- @since 0.1
makeRelativeToCurrentDirectory ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Path
makeRelativeToCurrentDirectory = send . MakeRelativeToCurrentDirectory

-- | Lifted 'Dir.doesPathExist'.
--
-- @since 0.1
doesPathExist ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Bool
doesPathExist = send . DoesPathExist

-- | Lifted 'Dir.doesFileExist'.
--
-- @since 0.1
doesFileExist ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Bool
doesFileExist = send . DoesFileExist

-- | Lifted 'Dir.doesDirectoryExist'.
--
-- @since 0.1
doesDirectoryExist ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Bool
doesDirectoryExist = send . DoesDirectoryExist

-- | Lifted 'Dir.findExecutable'.
--
-- @since 0.1
findExecutable ::
  ( PathReaderDynamic :> es
  ) =>
  String ->
  Eff es (Maybe Path)
findExecutable = send . FindExecutable

-- | Lifted 'Dir.findExecutables'.
--
-- @since 0.1
findExecutables ::
  ( PathReaderDynamic :> es
  ) =>
  String ->
  Eff es [Path]
findExecutables = send . FindExecutables

-- | Lifted 'Dir.findExecutablesInDirectories'.
--
-- @since 0.1
findExecutablesInDirectories ::
  ( PathReaderDynamic :> es
  ) =>
  [Path] ->
  String ->
  Eff es [Path]
findExecutablesInDirectories ps = send . FindExecutablesInDirectories ps

-- | Lifted 'Dir.findFileWith'.
--
-- @since 0.1
findFileWith ::
  ( PathReaderDynamic :> es
  ) =>
  (Path -> Eff es Bool) ->
  [Path] ->
  String ->
  Eff es (Maybe Path)
findFileWith f ps = send . FindFileWith f ps

-- | Lifted 'Dir.findFilesWith'.
--
-- @since 0.1
findFilesWith ::
  ( PathReaderDynamic :> es
  ) =>
  (Path -> Eff es Bool) ->
  [Path] ->
  String ->
  Eff es [Path]
findFilesWith f ps = send . FindFilesWith f ps

-- | Lifted 'Dir.pathIsSymbolicLink'.
--
-- @since 0.1
pathIsSymbolicLink ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Bool
pathIsSymbolicLink = send . PathIsSymbolicLink

-- | Lifted 'Dir.getSymbolicLinkTarget'.
--
-- @since 0.1
getSymbolicLinkTarget ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Path
getSymbolicLinkTarget = send . GetSymbolicLinkTarget

-- | Lifted 'Dir.getPermissions'.
--
-- @since 0.1
getPermissions ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Permissions
getPermissions = send . GetPermissions

-- | Lifted 'Dir.getAccessTime'.
--
-- @since 0.1
getAccessTime ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es UTCTime
getAccessTime = send . GetAccessTime

-- | Lifted 'Dir.getModificationTime'.
--
-- @since 0.1
getModificationTime ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es UTCTime
getModificationTime = send . GetModificationTime

-- | Retrieves the XDG data directory e.g. @~/.local\/share@.
--
-- @since 0.1
getXdgData ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Path
getXdgData = getXdgDirectory XdgData

-- | Retrieves the XDG config directory e.g. @~/.config@.
--
-- @since 0.1
getXdgConfig ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Path
getXdgConfig = getXdgDirectory XdgConfig

-- | Retrieves the XDG cache directory e.g. @~/.cache@.
--
-- @since 0.1
getXdgCache ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Path
getXdgCache = getXdgDirectory XdgCache

#if MIN_VERSION_directory(1,3,7)

-- | Retrieves the XDG state directory e.g. @~/.local\/state@.
--
-- @since 0.1
getXdgState ::
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Eff es Path
getXdgState = getXdgDirectory XdgState

#endif

-- | Retrieves the recursive directory contents; splits the sub folders and
-- directories apart.
--
-- @since 0.1
listDirectoryRecursive ::
  forall es.
  ( PathReaderDynamic :> es
  ) =>
  -- | Root path.
  Path ->
  -- | (files, directories)
  Eff es ([Path], [Path])
listDirectoryRecursive root = recurseDirs [emptyPath]
  where
    recurseDirs :: [Path] -> Eff es ([Path], [Path])
    recurseDirs [] = pure ([], [])
    recurseDirs (d : ds) = do
      (files, dirs) <- splitPaths root d [] [] =<< listDirectory (root </> d)
      (files', dirs') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs')
    emptyPath = mempty

splitPaths ::
  forall es.
  ( PathReaderDynamic :> es
  ) =>
  Path ->
  Path ->
  [Path] ->
  [Path] ->
  [Path] ->
  Eff es ([Path], [Path])
splitPaths root d = go
  where
    go :: [Path] -> [Path] -> [Path] -> Eff es ([Path], [Path])
    go files dirs [] = pure (reverse files, reverse dirs)
    go files dirs (p : ps) = do
      let dirEntry = d </> p
      isDir <- doesDirectoryExist (root </> dirEntry)
      if isDir
        then go files (dirEntry : dirs) ps
        else go (dirEntry : files) dirs ps
