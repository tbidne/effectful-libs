{-# LANGUAGE CPP #-}

-- | Provides an effect for reading paths.
--
-- @since 0.1
module Effectful.FileSystem.PathReader
  ( -- * Effect
    PathReaderEffect (..),
    Path,

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
    findFile,
    findFiles,
    findFileWith,
    findFilesWith,
    pathIsSymbolicLink,
    getSymbolicLinkTarget,
    getPermissions,
    getAccessTime,
    getModificationTime,

    -- ** Handlers
    runPathReaderIO,

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
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.FileSystem.Path (Path)
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
data PathReaderEffect :: Effect where
  ListDirectory :: (HasCallStack) => Path -> PathReaderEffect m [Path]
  GetDirectoryContents :: (HasCallStack) => Path -> PathReaderEffect m [Path]
  GetCurrentDirectory :: (HasCallStack) => PathReaderEffect m Path
  GetHomeDirectory :: (HasCallStack) => PathReaderEffect m Path
  GetXdgDirectory :: (HasCallStack) => XdgDirectory -> Path -> PathReaderEffect m Path
  GetXdgDirectoryList :: (HasCallStack) => XdgDirectoryList -> PathReaderEffect m [Path]
  GetAppUserDataDirectory :: (HasCallStack) => Path -> PathReaderEffect m Path
  GetUserDocumentsDirectory :: (HasCallStack) => PathReaderEffect m Path
  GetTemporaryDirectory :: (HasCallStack) => PathReaderEffect m Path
  GetFileSize :: (HasCallStack) => Path -> PathReaderEffect m Integer
  CanonicalizePath :: (HasCallStack) => Path -> PathReaderEffect m Path
  MakeAbsolute :: (HasCallStack) => Path -> PathReaderEffect m Path
  MakeRelativeToCurrentDirectory :: (HasCallStack) => Path -> PathReaderEffect m Path
  DoesPathExist :: (HasCallStack) => Path -> PathReaderEffect m Bool
  DoesFileExist :: (HasCallStack) => Path -> PathReaderEffect m Bool
  DoesDirectoryExist :: (HasCallStack) => Path -> PathReaderEffect m Bool
  FindExecutable :: (HasCallStack) => String -> PathReaderEffect m (Maybe Path)
  FindExecutables :: (HasCallStack) => String -> PathReaderEffect m [Path]
  FindExecutablesInDirectories :: (HasCallStack) => [Path] -> String -> PathReaderEffect m [Path]
  FindFile :: (HasCallStack) => [Path] -> String -> PathReaderEffect m (Maybe Path)
  FindFiles :: (HasCallStack) => [Path] -> String -> PathReaderEffect m [Path]
  FindFileWith :: (HasCallStack) => (Path -> m Bool) -> [Path] -> String -> PathReaderEffect m (Maybe Path)
  FindFilesWith :: (HasCallStack) => (Path -> m Bool) -> [Path] -> String -> PathReaderEffect m [Path]
  PathIsSymbolicLink :: (HasCallStack) => Path -> PathReaderEffect m Bool
  GetSymbolicLinkTarget :: (HasCallStack) => Path -> PathReaderEffect m Path
  GetPermissions :: (HasCallStack) => Path -> PathReaderEffect m Permissions
  GetAccessTime :: (HasCallStack) => Path -> PathReaderEffect m UTCTime
  GetModificationTime :: (HasCallStack) => Path -> PathReaderEffect m UTCTime

-- | @since 0.1
type instance DispatchOf PathReaderEffect = Dynamic

-- | Runs 'PathReaderEffect' in 'IO'.
--
-- @since 0.1
runPathReaderIO ::
  ( IOE :> es
  ) =>
  Eff (PathReaderEffect : es) a ->
  Eff es a
runPathReaderIO = interpret $ \env -> \case
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
  FindFile ps str -> liftIO $ Dir.findFile ps str
  FindFiles ps str -> liftIO $ Dir.findFiles ps str
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
listDirectory ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es [Path]
listDirectory = send . ListDirectory

-- | @since 0.1
getDirectoryContents ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es [Path]
getDirectoryContents = send . GetDirectoryContents

-- | @since 0.1
getCurrentDirectory ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Eff es Path
getCurrentDirectory = send GetCurrentDirectory

-- | @since 0.1
getHomeDirectory ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Eff es Path
getHomeDirectory = send GetHomeDirectory

-- | @since 0.1
getXdgDirectory ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  XdgDirectory ->
  Path ->
  Eff es Path
getXdgDirectory xdg = send . GetXdgDirectory xdg

-- | @since 0.1
getXdgDirectoryList ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  XdgDirectoryList ->
  Eff es [Path]
getXdgDirectoryList = send . GetXdgDirectoryList

-- | @since 0.1
getAppUserDataDirectory ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
getAppUserDataDirectory = send . GetAppUserDataDirectory

-- | @since 0.1
getUserDocumentsDirectory ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Eff es Path
getUserDocumentsDirectory = send GetUserDocumentsDirectory

-- | @since 0.1
getTemporaryDirectory ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Eff es Path
getTemporaryDirectory = send GetTemporaryDirectory

-- | @since 0.1
getFileSize ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Integer
getFileSize = send . GetFileSize

-- | @since 0.1
canonicalizePath ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
canonicalizePath = send . CanonicalizePath

-- | @since 0.1
makeAbsolute ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
makeAbsolute = send . MakeAbsolute

-- | @since 0.1
makeRelativeToCurrentDirectory ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
makeRelativeToCurrentDirectory = send . MakeRelativeToCurrentDirectory

-- | @since 0.1
doesPathExist ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Bool
doesPathExist = send . DoesPathExist

-- | @since 0.1
doesFileExist ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Bool
doesFileExist = send . DoesFileExist

-- | @since 0.1
doesDirectoryExist ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Bool
doesDirectoryExist = send . DoesDirectoryExist

-- | @since 0.1
findExecutable ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  String ->
  Eff es (Maybe Path)
findExecutable = send . FindExecutable

-- | @since 0.1
findExecutables ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  String ->
  Eff es [Path]
findExecutables = send . FindExecutables

-- | @since 0.1
findExecutablesInDirectories ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  [Path] ->
  String ->
  Eff es [Path]
findExecutablesInDirectories ps = send . FindExecutablesInDirectories ps

-- | @since 0.1
findFile ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  [Path] ->
  String ->
  Eff es (Maybe Path)
findFile ps = send . FindFile ps

-- | @since 0.1
findFiles ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  [Path] ->
  String ->
  Eff es [Path]
findFiles ps = send . FindFiles ps

-- | @since 0.1
findFileWith ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  (Path -> Eff es Bool) ->
  [Path] ->
  String ->
  Eff es (Maybe Path)
findFileWith f ps = send . FindFileWith f ps

-- | @since 0.1
findFilesWith ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  (Path -> Eff es Bool) ->
  [Path] ->
  String ->
  Eff es [Path]
findFilesWith f ps = send . FindFilesWith f ps

-- | @since 0.1
pathIsSymbolicLink ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Bool
pathIsSymbolicLink = send . PathIsSymbolicLink

-- | @since 0.1
getSymbolicLinkTarget ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
getSymbolicLinkTarget = send . GetSymbolicLinkTarget

-- | @since 0.1
getPermissions ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Permissions
getPermissions = send . GetPermissions

-- | @since 0.1
getAccessTime ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es UTCTime
getAccessTime = send . GetAccessTime

-- | @since 0.1
getModificationTime ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es UTCTime
getModificationTime = send . GetModificationTime

-- | Retrieves the Xdg Config directory.
--
-- @since 0.1
getXdgConfig ::
  ( HasCallStack,
    PathReaderEffect :> es
  ) =>
  Path ->
  Eff es Path
getXdgConfig = getXdgDirectory XdgConfig
