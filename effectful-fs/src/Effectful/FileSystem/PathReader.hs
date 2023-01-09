{-# LANGUAGE CPP #-}

-- | Provides an effect for reading paths.
--
-- @since 0.1
module Effectful.FileSystem.PathReader
  ( -- * Effect
    EffectPathReader (..),
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
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO, send)
import Effectful.FileSystem.Path (Path)
import Effectful.Internal.Monad (UnliftStrategy (SeqUnlift))
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
data EffectPathReader :: Effect where
  ListDirectory :: HasCallStack => Path -> EffectPathReader m [Path]
  GetDirectoryContents :: HasCallStack => Path -> EffectPathReader m [Path]
  GetCurrentDirectory :: HasCallStack => EffectPathReader m Path
  GetHomeDirectory :: HasCallStack => EffectPathReader m Path
  GetXdgDirectory :: HasCallStack => XdgDirectory -> Path -> EffectPathReader m Path
  GetXdgDirectoryList :: HasCallStack => XdgDirectoryList -> EffectPathReader m [Path]
  GetAppUserDataDirectory :: HasCallStack => Path -> EffectPathReader m Path
  GetUserDocumentsDirectory :: HasCallStack => EffectPathReader m Path
  GetTemporaryDirectory :: HasCallStack => EffectPathReader m Path
  GetFileSize :: HasCallStack => Path -> EffectPathReader m Integer
  CanonicalizePath :: HasCallStack => Path -> EffectPathReader m Path
  MakeAbsolute :: HasCallStack => Path -> EffectPathReader m Path
  MakeRelativeToCurrentDirectory :: HasCallStack => Path -> EffectPathReader m Path
  DoesPathExist :: HasCallStack => Path -> EffectPathReader m Bool
  DoesFileExist :: HasCallStack => Path -> EffectPathReader m Bool
  DoesDirectoryExist :: HasCallStack => Path -> EffectPathReader m Bool
  FindExecutable :: HasCallStack => String -> EffectPathReader m (Maybe Path)
  FindExecutables :: HasCallStack => String -> EffectPathReader m [Path]
  FindExecutablesInDirectories :: HasCallStack => [Path] -> String -> EffectPathReader m [Path]
  FindFile :: HasCallStack => [Path] -> String -> EffectPathReader m (Maybe Path)
  FindFiles :: HasCallStack => [Path] -> String -> EffectPathReader m [Path]
  FindFileWith :: HasCallStack => (Path -> m Bool) -> [Path] -> String -> EffectPathReader m (Maybe Path)
  FindFilesWith :: HasCallStack => (Path -> m Bool) -> [Path] -> String -> EffectPathReader m [Path]
  PathIsSymbolicLink :: HasCallStack => Path -> EffectPathReader m Bool
  GetSymbolicLinkTarget :: HasCallStack => Path -> EffectPathReader m Path
  GetPermissions :: HasCallStack => Path -> EffectPathReader m Permissions
  GetAccessTime :: HasCallStack => Path -> EffectPathReader m UTCTime
  GetModificationTime :: HasCallStack => Path -> EffectPathReader m UTCTime

-- | @since 0.1
type instance DispatchOf EffectPathReader = Dynamic

-- | Runs 'PathReader' in 'IO'.
--
-- @since 0.1
runPathReaderIO ::
  ( EffectCallStack :> es,
    IOE :> es
  ) =>
  Eff (EffectPathReader : es) a ->
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

-- | @since 0.1
listDirectory ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es [Path]
listDirectory = send . ListDirectory

-- | @since 0.1
getDirectoryContents ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es [Path]
getDirectoryContents = send . GetDirectoryContents

-- | @since 0.1
getCurrentDirectory ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Eff es Path
getCurrentDirectory = send GetCurrentDirectory

-- | @since 0.1
getHomeDirectory ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Eff es Path
getHomeDirectory = send GetHomeDirectory

-- | @since 0.1
getXdgDirectory ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  XdgDirectory ->
  Path ->
  Eff es Path
getXdgDirectory xdg = send . GetXdgDirectory xdg

-- | @since 0.1
getXdgDirectoryList ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  XdgDirectoryList ->
  Eff es [Path]
getXdgDirectoryList = send . GetXdgDirectoryList

-- | @since 0.1
getAppUserDataDirectory ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Path
getAppUserDataDirectory = send . GetAppUserDataDirectory

-- | @since 0.1
getUserDocumentsDirectory ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Eff es Path
getUserDocumentsDirectory = send GetUserDocumentsDirectory

-- | @since 0.1
getTemporaryDirectory ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Eff es Path
getTemporaryDirectory = send GetTemporaryDirectory

-- | @since 0.1
getFileSize ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Integer
getFileSize = send . GetFileSize

-- | @since 0.1
canonicalizePath ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Path
canonicalizePath = send . CanonicalizePath

-- | @since 0.1
makeAbsolute ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Path
makeAbsolute = send . MakeAbsolute

-- | @since 0.1
makeRelativeToCurrentDirectory ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Path
makeRelativeToCurrentDirectory = send . MakeRelativeToCurrentDirectory

-- | @since 0.1
doesPathExist ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Bool
doesPathExist = send . DoesPathExist

-- | @since 0.1
doesFileExist ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Bool
doesFileExist = send . DoesFileExist

-- | @since 0.1
doesDirectoryExist ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Bool
doesDirectoryExist = send . DoesDirectoryExist

-- | @since 0.1
findExecutable ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  String ->
  Eff es (Maybe Path)
findExecutable = send . FindExecutable

-- | @since 0.1
findExecutables ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  String ->
  Eff es [Path]
findExecutables = send . FindExecutables

-- | @since 0.1
findExecutablesInDirectories ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  [Path] ->
  String ->
  Eff es [Path]
findExecutablesInDirectories ps = send . FindExecutablesInDirectories ps

-- | @since 0.1
findFile ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  [Path] ->
  String ->
  Eff es (Maybe Path)
findFile ps = send . FindFile ps

-- | @since 0.1
findFiles ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  [Path] ->
  String ->
  Eff es [Path]
findFiles ps = send . FindFiles ps

-- | @since 0.1
findFileWith ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  (Path -> Eff es Bool) ->
  [Path] ->
  String ->
  Eff es (Maybe Path)
findFileWith f ps = send . FindFileWith f ps

-- | @since 0.1
findFilesWith ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  (Path -> Eff es Bool) ->
  [Path] ->
  String ->
  Eff es [Path]
findFilesWith f ps = send . FindFilesWith f ps

-- | @since 0.1
pathIsSymbolicLink ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Bool
pathIsSymbolicLink = send . PathIsSymbolicLink

-- | @since 0.1
getSymbolicLinkTarget ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Path
getSymbolicLinkTarget = send . GetSymbolicLinkTarget

-- | @since 0.1
getPermissions ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Permissions
getPermissions = send . GetPermissions

-- | @since 0.1
getAccessTime ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es UTCTime
getAccessTime = send . GetAccessTime

-- | @since 0.1
getModificationTime ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es UTCTime
getModificationTime = send . GetModificationTime

-- | Retrieves the Xdg Config directory.
--
-- @since 0.1
getXdgConfig ::
  ( EffectPathReader :> es,
    HasCallStack
  ) =>
  Path ->
  Eff es Path
getXdgConfig = getXdgDirectory XdgConfig
