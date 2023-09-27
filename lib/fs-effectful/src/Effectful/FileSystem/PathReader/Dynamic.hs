{-# LANGUAGE UndecidableInstances #-}

-- | Provides a dynamic effect for the readable portion of "System.Directory"'s
-- interface.
--
-- @since 0.1
module Effectful.FileSystem.PathReader.Dynamic
  ( -- * Class
    MonadPathReader (..),

    -- * Effect
    PathReaderDynamic (..),

    -- ** Handlers
    runPathReaderDynamicIO,

    -- * Functions
    findFile,
    findFiles,

    -- ** XDG Utils
    getXdgData,
    getXdgConfig,
    getXdgCache,
    getXdgState,

    -- * Misc
    listDirectoryRecursive,

    -- * Re-exports
    OsPath,
    Permissions,
    UTCTime (..),
    XdgDirectory (..),
    XdgDirectoryList (..),
  )
where

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
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.FileSystem.Utils (OsPath, (</>))
import System.Directory
  ( Permissions,
    XdgDirectory (XdgCache, XdgConfig, XdgData, XdgState),
    XdgDirectoryList (XdgConfigDirs, XdgDataDirs),
  )
import System.Directory.OsPath qualified as Dir

-- | Represents file-system reader effects.
--
-- @since 0.1
class (Monad m) => MonadPathReader m where
  -- | Lifted 'Dir.listDirectory'.
  --
  -- @since 0.1
  listDirectory :: OsPath -> m [OsPath]

  -- | Lifted 'Dir.getDirectoryContents'.
  --
  -- @since 0.1
  getDirectoryContents :: OsPath -> m [OsPath]

  -- | Lifted 'Dir.getCurrentDirectory'.
  --
  -- @since 0.1
  getCurrentDirectory :: m OsPath

  -- | Lifted 'Dir.getHomeDirectory'.
  --
  -- @since 0.1
  getHomeDirectory :: m OsPath

  -- | Lifted 'Dir.getXdgDirectory'.
  --
  -- @since 0.1
  getXdgDirectory :: XdgDirectory -> OsPath -> m OsPath

  -- | Lifted 'Dir.getXdgDirectoryList'.
  --
  -- @since 0.1
  getXdgDirectoryList :: XdgDirectoryList -> m [OsPath]

  -- | Lifted 'Dir.getAppUserDataDirectory'.
  --
  -- @since 0.1
  getAppUserDataDirectory :: OsPath -> m OsPath

  -- | Lifted 'Dir.getUserDocumentsDirectory'.
  --
  -- @since 0.1
  getUserDocumentsDirectory :: m OsPath

  -- | Lifted 'Dir.getTemporaryDirectory'.
  --
  -- @since 0.1
  getTemporaryDirectory :: m OsPath

  -- | Lifted 'Dir.getFileSize'.
  --
  -- @since 0.1
  getFileSize :: OsPath -> m Integer

  -- | Lifted 'Dir.canonicalizePath'.
  --
  -- @since 0.1
  canonicalizePath :: OsPath -> m OsPath

  -- | Lifted 'Dir.makeAbsolute'.
  --
  -- @since 0.1
  makeAbsolute :: OsPath -> m OsPath

  -- | Lifted 'Dir.makeRelativeToCurrentDirectory'.
  --
  -- @since 0.1
  makeRelativeToCurrentDirectory :: OsPath -> m OsPath

  -- | Lifted 'Dir.doesPathExist'.
  --
  -- @since 0.1
  doesPathExist :: OsPath -> m Bool

  -- | Lifted 'Dir.doesFileExist'.
  --
  -- @since 0.1
  doesFileExist :: OsPath -> m Bool

  -- | Lifted 'Dir.doesDirectoryExist'.
  --
  -- @since 0.1
  doesDirectoryExist :: OsPath -> m Bool

  -- | Lifted 'Dir.findExecutable'.
  --
  -- @since 0.1
  findExecutable :: OsPath -> m (Maybe OsPath)

  -- | Lifted 'Dir.findExecutables'.
  --
  -- @since 0.1
  findExecutables :: OsPath -> m [OsPath]

  -- | Lifted 'Dir.findExecutablesInDirectories'.
  --
  -- @since 0.1
  findExecutablesInDirectories :: [OsPath] -> OsPath -> m [OsPath]

  -- | Lifted 'Dir.findFileWith'.
  --
  -- @since 0.1
  findFileWith :: (OsPath -> m Bool) -> [OsPath] -> OsPath -> m (Maybe OsPath)

  -- | Lifted 'Dir.findFilesWith'.
  --
  -- @since 0.1
  findFilesWith :: (OsPath -> m Bool) -> [OsPath] -> OsPath -> m [OsPath]

  -- | Lifted 'Dir.pathIsSymbolicLink'.
  --
  -- @since 0.1
  pathIsSymbolicLink :: OsPath -> m Bool

  -- | Lifted 'Dir.getSymbolicLinkTarget'.
  --
  -- @since 0.1
  getSymbolicLinkTarget :: OsPath -> m OsPath

  -- | Lifted 'Dir.getPermissions'.
  --
  -- @since 0.1
  getPermissions :: OsPath -> m Permissions

  -- | Lifted 'Dir.getAccessTime'.
  --
  -- @since 0.1
  getAccessTime :: OsPath -> m UTCTime

  -- | Lifted 'Dir.getModificationTime'.
  --
  -- @since 0.1
  getModificationTime :: OsPath -> m UTCTime

instance MonadPathReader IO where
  listDirectory = Dir.listDirectory
  getDirectoryContents = Dir.getDirectoryContents
  getCurrentDirectory = Dir.getCurrentDirectory
  getHomeDirectory = Dir.getHomeDirectory
  getXdgDirectory = Dir.getXdgDirectory
  getXdgDirectoryList = Dir.getXdgDirectoryList
  getAppUserDataDirectory = Dir.getAppUserDataDirectory
  getUserDocumentsDirectory = Dir.getUserDocumentsDirectory
  getTemporaryDirectory = Dir.getTemporaryDirectory
  getFileSize = Dir.getFileSize
  canonicalizePath = Dir.canonicalizePath
  makeAbsolute = Dir.makeAbsolute
  makeRelativeToCurrentDirectory = Dir.makeRelativeToCurrentDirectory
  doesPathExist = Dir.doesPathExist
  doesFileExist = Dir.doesFileExist
  doesDirectoryExist = Dir.doesDirectoryExist
  findExecutable = Dir.findExecutable
  findExecutables = Dir.findExecutables
  findExecutablesInDirectories = Dir.findExecutablesInDirectories
  findFileWith = Dir.findFileWith
  findFilesWith = Dir.findFilesWith
  pathIsSymbolicLink = Dir.pathIsSymbolicLink
  getSymbolicLinkTarget = Dir.getSymbolicLinkTarget
  getPermissions = Dir.getPermissions
  getAccessTime = Dir.getAccessTime
  getModificationTime = Dir.getModificationTime

-- | Dynamic effect for reading paths.
--
-- @since 0.1
data PathReaderDynamic :: Effect where
  ListDirectory :: OsPath -> PathReaderDynamic m [OsPath]
  GetDirectoryContents :: OsPath -> PathReaderDynamic m [OsPath]
  GetCurrentDirectory :: PathReaderDynamic m OsPath
  GetHomeDirectory :: PathReaderDynamic m OsPath
  GetXdgDirectory :: XdgDirectory -> OsPath -> PathReaderDynamic m OsPath
  GetXdgDirectoryList :: XdgDirectoryList -> PathReaderDynamic m [OsPath]
  GetAppUserDataDirectory :: OsPath -> PathReaderDynamic m OsPath
  GetUserDocumentsDirectory :: PathReaderDynamic m OsPath
  GetTemporaryDirectory :: PathReaderDynamic m OsPath
  GetFileSize :: OsPath -> PathReaderDynamic m Integer
  CanonicalizePath :: OsPath -> PathReaderDynamic m OsPath
  MakeAbsolute :: OsPath -> PathReaderDynamic m OsPath
  MakeRelativeToCurrentDirectory :: OsPath -> PathReaderDynamic m OsPath
  DoesPathExist :: OsPath -> PathReaderDynamic m Bool
  DoesFileExist :: OsPath -> PathReaderDynamic m Bool
  DoesDirectoryExist :: OsPath -> PathReaderDynamic m Bool
  FindExecutable :: OsPath -> PathReaderDynamic m (Maybe OsPath)
  FindExecutables :: OsPath -> PathReaderDynamic m [OsPath]
  FindExecutablesInDirectories ::
    [OsPath] ->
    OsPath ->
    PathReaderDynamic m [OsPath]
  FindFileWith ::
    (OsPath -> m Bool) ->
    [OsPath] ->
    OsPath ->
    PathReaderDynamic m (Maybe OsPath)
  FindFilesWith ::
    (OsPath -> m Bool) ->
    [OsPath] ->
    OsPath ->
    PathReaderDynamic m [OsPath]
  PathIsSymbolicLink :: OsPath -> PathReaderDynamic m Bool
  GetSymbolicLinkTarget :: OsPath -> PathReaderDynamic m OsPath
  GetPermissions :: OsPath -> PathReaderDynamic m Permissions
  GetAccessTime :: OsPath -> PathReaderDynamic m UTCTime
  GetModificationTime :: OsPath -> PathReaderDynamic m UTCTime

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
  FindExecutablesInDirectories ps str ->
    liftIO $ Dir.findExecutablesInDirectories ps str
  FindFileWith f ps str -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Dir.findFileWith (runInIO . f) ps str
  FindFilesWith f ps str -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Dir.findFilesWith (runInIO . f) ps str
  PathIsSymbolicLink p -> liftIO $ Dir.pathIsSymbolicLink p
  GetSymbolicLinkTarget p -> liftIO $ Dir.getSymbolicLinkTarget p
  GetPermissions p -> liftIO $ Dir.getPermissions p
  GetAccessTime p -> liftIO $ Dir.getAccessTime p
  GetModificationTime p -> liftIO $ Dir.getModificationTime p

-- | @since 0.1
instance (PathReaderDynamic :> es) => MonadPathReader (Eff es) where
  listDirectory = send . ListDirectory
  getDirectoryContents = send . GetDirectoryContents
  getCurrentDirectory = send GetCurrentDirectory
  getHomeDirectory = send GetHomeDirectory
  getXdgDirectory xdg = send . GetXdgDirectory xdg
  getXdgDirectoryList = send . GetXdgDirectoryList
  getAppUserDataDirectory = send . GetAppUserDataDirectory
  getUserDocumentsDirectory = send GetUserDocumentsDirectory
  getTemporaryDirectory = send GetTemporaryDirectory
  getFileSize = send . GetFileSize
  canonicalizePath = send . CanonicalizePath
  makeAbsolute = send . MakeAbsolute
  makeRelativeToCurrentDirectory = send . MakeRelativeToCurrentDirectory
  doesPathExist = send . DoesPathExist
  doesFileExist = send . DoesFileExist
  doesDirectoryExist = send . DoesDirectoryExist
  findExecutable = send . FindExecutable
  findExecutables = send . FindExecutables
  findExecutablesInDirectories ps = send . FindExecutablesInDirectories ps
  findFileWith f ps = send . FindFileWith f ps
  findFilesWith f ps = send . FindFilesWith f ps
  pathIsSymbolicLink = send . PathIsSymbolicLink
  getSymbolicLinkTarget = send . GetSymbolicLinkTarget
  getPermissions = send . GetPermissions
  getAccessTime = send . GetAccessTime
  getModificationTime = send . GetModificationTime

-- | Lifted 'Dir.findFile'.
--
-- @since 0.1
findFile :: (MonadPathReader m) => [OsPath] -> OsPath -> m (Maybe OsPath)
findFile = findFileWith (\_ -> pure True)

-- | Lifted 'Dir.findFiles'.
--
-- @since 0.1
findFiles :: (MonadPathReader m) => [OsPath] -> OsPath -> m [OsPath]
findFiles = findFilesWith (\_ -> pure True)

-- | Retrieves the XDG data directory e.g. @~/.local\/share@.
--
-- @since 0.1
getXdgData ::
  ( MonadPathReader m
  ) =>
  OsPath ->
  m OsPath
getXdgData = getXdgDirectory XdgData

-- | Retrieves the XDG config directory e.g. @~/.config@.
--
-- @since 0.1
getXdgConfig ::
  ( MonadPathReader m
  ) =>
  OsPath ->
  m OsPath
getXdgConfig = getXdgDirectory XdgConfig

-- | Retrieves the XDG cache directory e.g. @~/.cache@.
--
-- @since 0.1
getXdgCache ::
  ( MonadPathReader m
  ) =>
  OsPath ->
  m OsPath
getXdgCache = getXdgDirectory XdgCache

-- | Retrieves the XDG state directory e.g. @~/.local\/state@.
--
-- @since 0.1
getXdgState ::
  ( MonadPathReader m
  ) =>
  OsPath ->
  m OsPath
getXdgState = getXdgDirectory XdgState

-- | Retrieves the recursive directory contents; splits the sub folders and
-- directories apart.
--
-- @since 0.1
listDirectoryRecursive ::
  forall m.
  ( MonadPathReader m
  ) =>
  -- | Root path.
  OsPath ->
  -- | (files, directories)
  m ([OsPath], [OsPath])
listDirectoryRecursive root = recurseDirs [emptyPath]
  where
    recurseDirs :: [OsPath] -> m ([OsPath], [OsPath])
    recurseDirs [] = pure ([], [])
    recurseDirs (d : ds) = do
      (files, dirs) <- splitPaths root d [] [] =<< listDirectory (root </> d)
      (files', dirs') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs')
    emptyPath = mempty

splitPaths ::
  forall m.
  ( MonadPathReader m
  ) =>
  OsPath ->
  OsPath ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  m ([OsPath], [OsPath])
splitPaths root d = go
  where
    go :: [OsPath] -> [OsPath] -> [OsPath] -> m ([OsPath], [OsPath])
    go files dirs [] = pure (reverse files, reverse dirs)
    go files dirs (p : ps) = do
      let dirEntry = d </> p
      isDir <- doesDirectoryExist (root </> dirEntry)
      if isDir
        then go files (dirEntry : dirs) ps
        else go (dirEntry : files) dirs ps
