{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for the readable portion of "System.Directory"'s
-- interface. For the static interface of the entire "System.Directory"
-- interface, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-FileSystem.html.
--
-- @since 0.1
module Effectful.FileSystem.PathReader.Static
  ( -- * Class
    MonadPathReader (..),

    -- * Effect
    PathReaderStatic,

    -- ** Handlers
    runPathReaderStaticIO,

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

-- | Static effect for reading paths.
--
-- @since 0.1
data PathReaderStatic :: Effect

type instance DispatchOf PathReaderStatic = Static WithSideEffects

data instance StaticRep PathReaderStatic = MkPathReaderStatic

-- | Runs an 'PathReaderStatic' effect in IO.
--
-- @since 0.1
runPathReaderStaticIO :: (IOE :> es) => Eff (PathReaderStatic : es) a -> Eff es a
runPathReaderStaticIO = evalStaticRep MkPathReaderStatic

-- | @since 0.1
instance (PathReaderStatic :> es) => MonadPathReader (Eff es) where
  listDirectory = unsafeEff_ . Dir.listDirectory
  getDirectoryContents = unsafeEff_ . Dir.getDirectoryContents
  getCurrentDirectory = unsafeEff_ Dir.getCurrentDirectory
  getHomeDirectory = unsafeEff_ Dir.getHomeDirectory
  getXdgDirectory xdg = unsafeEff_ . Dir.getXdgDirectory xdg
  getXdgDirectoryList = unsafeEff_ . Dir.getXdgDirectoryList
  getAppUserDataDirectory = unsafeEff_ . Dir.getAppUserDataDirectory
  getUserDocumentsDirectory = unsafeEff_ Dir.getUserDocumentsDirectory
  getTemporaryDirectory = unsafeEff_ Dir.getTemporaryDirectory
  getFileSize = unsafeEff_ . Dir.getFileSize
  canonicalizePath = unsafeEff_ . Dir.canonicalizePath
  makeAbsolute = unsafeEff_ . Dir.makeAbsolute
  makeRelativeToCurrentDirectory = unsafeEff_ . Dir.makeRelativeToCurrentDirectory
  doesPathExist = unsafeEff_ . Dir.doesPathExist
  doesFileExist = unsafeEff_ . Dir.doesFileExist
  doesDirectoryExist = unsafeEff_ . Dir.doesDirectoryExist
  findExecutable = unsafeEff_ . Dir.findExecutable
  findExecutables = unsafeEff_ . Dir.findExecutables
  findExecutablesInDirectories ps =
    unsafeEff_ . Dir.findExecutablesInDirectories ps
  findFileWith f ps s =
    unsafeEff $ \env -> seqUnliftIO env $
      \runInIO -> Dir.findFileWith (runInIO . f) ps s
  findFilesWith f ps s =
    unsafeEff $ \env -> seqUnliftIO env $
      \runInIO -> Dir.findFilesWith (runInIO . f) ps s
  pathIsSymbolicLink = unsafeEff_ . Dir.pathIsSymbolicLink
  getSymbolicLinkTarget = unsafeEff_ . Dir.getSymbolicLinkTarget
  getPermissions = unsafeEff_ . Dir.getPermissions
  getAccessTime = unsafeEff_ . Dir.getAccessTime
  getModificationTime = unsafeEff_ . Dir.getModificationTime

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
