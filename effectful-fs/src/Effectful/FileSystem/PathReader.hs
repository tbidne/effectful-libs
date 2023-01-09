{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides an effect for reading paths.
--
-- @since 0.1
module Effectful.FileSystem.PathReader
  ( -- * Effect
    PathReader (..),
    Path,

    -- * Handler
    runPathReaderIO,

    -- * Functions
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
    findFile,
    findFiles,
    findFileWith,
    findFilesWith,
    pathIsSymbolicLink,
    getSymbolicLinkTarget,
    getPermissions,
    getAccessTime,
    getModificationTime,

    -- * Xdg Utils
    getXdgConfig,

    -- * Reexports
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
import Effectful.CallStack
  ( EffectCallStack,
    addCallStack,
  )
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO)
import Effectful.FileSystem.Path (Path)
import Effectful.Internal.Monad (UnliftStrategy (SeqUnlift))
import Effectful.TH (makeEffect_)
import GHC.Stack (HasCallStack)
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

-- | Effect for reading paths.
--
-- @since 0.1
data PathReader :: Effect where
  ListDirectory :: HasCallStack => Path -> PathReader m [Path]
  GetDirectoryContents :: HasCallStack => Path -> PathReader m [Path]
  GetCurrentDirectory :: HasCallStack => PathReader m Path
  GetHomeDirectory :: HasCallStack => PathReader m Path
  GetXdgDirectory :: HasCallStack => XdgDirectory -> Path -> PathReader m Path
  GetXdgDirectoryList :: HasCallStack => XdgDirectoryList -> PathReader m [Path]
  GetAppUserDataDirectory :: HasCallStack => Path -> PathReader m Path
  GetUserDocumentsDirectory :: HasCallStack => PathReader m Path
  GetTemporaryDirectory :: HasCallStack => PathReader m Path
  GetFileSize :: HasCallStack => Path -> PathReader m Integer
  CanonicalizePath :: HasCallStack => Path -> PathReader m Path
  MakeAbsolute :: HasCallStack => Path -> PathReader m Path
  MakeRelativeToCurrentDirectory :: HasCallStack => Path -> PathReader m Path
  DoesPathExist :: HasCallStack => Path -> PathReader m Bool
  DoesFileExist :: HasCallStack => Path -> PathReader m Bool
  DoesDirectoryExist :: HasCallStack => Path -> PathReader m Bool
  FindExecutable :: HasCallStack => String -> PathReader m (Maybe Path)
  FindExecutables :: HasCallStack => String -> PathReader m [Path]
  FindExecutablesInDirectories :: HasCallStack => [Path] -> String -> PathReader m [Path]
  FindFile :: HasCallStack => [Path] -> String -> PathReader m (Maybe Path)
  FindFiles :: HasCallStack => [Path] -> String -> PathReader m [Path]
  FindFileWith :: HasCallStack => (Path -> m Bool) -> [Path] -> String -> PathReader m (Maybe Path)
  FindFilesWith :: HasCallStack => (Path -> m Bool) -> [Path] -> String -> PathReader m [Path]
  PathIsSymbolicLink :: HasCallStack => Path -> PathReader m Bool
  GetSymbolicLinkTarget :: HasCallStack => Path -> PathReader m Path
  GetPermissions :: HasCallStack => Path -> PathReader m Permissions
  GetAccessTime :: HasCallStack => Path -> PathReader m UTCTime
  GetModificationTime :: HasCallStack => Path -> PathReader m UTCTime

-- | @since 0.1
type instance DispatchOf PathReader = Dynamic

-- | Runs 'PathReader' in 'IO'.
--
-- @since 0.1
runPathReaderIO ::
  ( EffectCallStack :> es,
    IOE :> es
  ) =>
  Eff (PathReader : es) a ->
  Eff es a
runPathReaderIO = interpret $ \env -> \case
  ListDirectory p -> addCallStack $ liftIO $ Dir.listDirectory p
  GetDirectoryContents p -> addCallStack $ liftIO $ Dir.getDirectoryContents p
  GetCurrentDirectory -> addCallStack $ liftIO Dir.getCurrentDirectory
  GetHomeDirectory -> addCallStack $ liftIO Dir.getHomeDirectory
  GetXdgDirectory xdg p -> addCallStack $ liftIO $ Dir.getXdgDirectory xdg p
  GetXdgDirectoryList xdg -> addCallStack $ liftIO $ Dir.getXdgDirectoryList xdg
  GetAppUserDataDirectory p -> addCallStack $ liftIO $ Dir.getAppUserDataDirectory p
  GetUserDocumentsDirectory -> addCallStack $ liftIO Dir.getUserDocumentsDirectory
  GetTemporaryDirectory -> addCallStack $ liftIO Dir.getTemporaryDirectory
  GetFileSize p -> addCallStack $ liftIO $ Dir.getFileSize p
  CanonicalizePath p -> addCallStack $ liftIO $ Dir.canonicalizePath p
  MakeAbsolute p -> addCallStack $ liftIO $ Dir.makeAbsolute p
  MakeRelativeToCurrentDirectory p -> addCallStack $ liftIO $ Dir.makeRelativeToCurrentDirectory p
  DoesPathExist p -> addCallStack $ liftIO $ Dir.doesPathExist p
  DoesFileExist p -> addCallStack $ liftIO $ Dir.doesFileExist p
  DoesDirectoryExist p -> addCallStack $ liftIO $ Dir.doesDirectoryExist p
  FindExecutable p -> addCallStack $ liftIO $ Dir.findExecutable p
  FindExecutables p -> addCallStack $ liftIO $ Dir.findExecutables p
  FindExecutablesInDirectories ps str -> addCallStack $ liftIO $ Dir.findExecutablesInDirectories ps str
  FindFile ps str -> addCallStack $ liftIO $ Dir.findFile ps str
  FindFiles ps str -> addCallStack $ liftIO $ Dir.findFiles ps str
  FindFileWith f ps str -> addCallStack $ localUnliftIO env SeqUnlift $ \runInIO ->
    liftIO $ Dir.findFileWith (runInIO . f) ps str
  FindFilesWith f ps str -> addCallStack $ localUnliftIO env SeqUnlift $ \runInIO ->
    liftIO $ Dir.findFilesWith (runInIO . f) ps str
  PathIsSymbolicLink p -> addCallStack $ liftIO $ Dir.pathIsSymbolicLink p
  GetSymbolicLinkTarget p -> addCallStack $ liftIO $ Dir.getSymbolicLinkTarget p
  GetPermissions p -> addCallStack $ liftIO $ Dir.getPermissions p
  GetAccessTime p -> addCallStack $ liftIO $ Dir.getAccessTime p
  GetModificationTime p -> addCallStack $ liftIO $ Dir.getModificationTime p

makeEffect_ ''PathReader

-- | @since 0.1
listDirectory :: (HasCallStack, PathReader :> es) => Path -> Eff es [Path]

-- | @since 0.1
getDirectoryContents :: (HasCallStack, PathReader :> es) => Path -> Eff es [Path]

-- | @since 0.1
getCurrentDirectory :: (HasCallStack, PathReader :> es) => Eff es Path

-- | @since 0.1
getHomeDirectory :: (HasCallStack, PathReader :> es) => Eff es Path

-- | @since 0.1
getXdgDirectory :: (HasCallStack, PathReader :> es) => XdgDirectory -> Path -> Eff es Path

-- | @since 0.1
getXdgDirectoryList :: (HasCallStack, PathReader :> es) => XdgDirectoryList -> Eff es [Path]

-- | @since 0.1
getAppUserDataDirectory :: (HasCallStack, PathReader :> es) => Path -> Eff es Path

-- | @since 0.1
getUserDocumentsDirectory :: (HasCallStack, PathReader :> es) => Eff es Path

-- | @since 0.1
getTemporaryDirectory :: (HasCallStack, PathReader :> es) => Eff es Path

-- | @since 0.1
getFileSize :: (HasCallStack, PathReader :> es) => Path -> Eff es Integer

-- | @since 0.1
canonicalizePath :: (HasCallStack, PathReader :> es) => Path -> Eff es Path

-- | @since 0.1
makeAbsolute :: (HasCallStack, PathReader :> es) => Path -> Eff es Path

-- | @since 0.1
makeRelativeToCurrentDirectory :: (HasCallStack, PathReader :> es) => Path -> Eff es Path

-- | @since 0.1
doesPathExist :: (HasCallStack, PathReader :> es) => Path -> Eff es Bool

-- | @since 0.1
doesFileExist :: (HasCallStack, PathReader :> es) => Path -> Eff es Bool

-- | @since 0.1
doesDirectoryExist :: (HasCallStack, PathReader :> es) => Path -> Eff es Bool

-- | @since 0.1
findExecutable :: (HasCallStack, PathReader :> es) => String -> Eff es (Maybe Path)

-- | @since 0.1
findExecutables :: (HasCallStack, PathReader :> es) => String -> Eff es [Path]

-- | @since 0.1
findExecutablesInDirectories :: (HasCallStack, PathReader :> es) => [Path] -> String -> Eff es [Path]

-- | @since 0.1
findFile :: (HasCallStack, PathReader :> es) => [Path] -> String -> Eff es (Maybe Path)

-- | @since 0.1
findFiles :: (HasCallStack, PathReader :> es) => [Path] -> String -> Eff es [Path]

-- | @since 0.1
findFileWith :: (HasCallStack, PathReader :> es) => (Path -> Eff es Bool) -> [Path] -> String -> Eff es (Maybe Path)

-- | @since 0.1
findFilesWith :: (HasCallStack, PathReader :> es) => (Path -> Eff es Bool) -> [Path] -> String -> Eff es [Path]

-- | @since 0.1
pathIsSymbolicLink :: (HasCallStack, PathReader :> es) => Path -> Eff es Bool

-- | @since 0.1
getSymbolicLinkTarget :: (HasCallStack, PathReader :> es) => Path -> Eff es Path

-- | @since 0.1
getPermissions :: (HasCallStack, PathReader :> es) => Path -> Eff es Permissions

-- | @since 0.1
getAccessTime :: (HasCallStack, PathReader :> es) => Path -> Eff es UTCTime

-- | @since 0.1
getModificationTime :: (HasCallStack, PathReader :> es) => Path -> Eff es UTCTime

-- | Retrieves the Xdg Config directory.
--
-- @since 0.1
getXdgConfig :: (HasCallStack, PathReader :> es) => Path -> Eff es Path
getXdgConfig = getXdgDirectory XdgConfig
