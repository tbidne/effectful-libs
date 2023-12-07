-- | Provides a dynamic effect for "System.PosixCompat.Files".
--
-- @since 0.1
module Effectful.PosixCompat.Dynamic
  ( -- * Effect
    PosixCompatDynamic (..),
    setFileMode,
    setFdMode,
    setFileCreationMask,
    fileAccess,
    fileExist,
    getFileStatus,
    getFdStatus,
    getSymbolicLinkStatus,
    createNamedPipe,
    createDevice,
    createLink,
    removeLink,
    createSymbolicLink,
    readSymbolicLink,
    rename,
    setOwnerAndGroup,
    setFdOwnerAndGroup,
    setSymbolicLinkOwnerAndGroup,
    setFileTimes,
    touchFile,
    setFileSize,
    setFdSize,
    getPathVar,
    getFdPathVar,

    -- ** Handler
    runPosixCompatDynamicIO,

    -- * PathType
    PathType (..),
    Utils.displayPathType,

    -- ** Functions
    throwIfWrongPathType,
    isPathType,
    getPathType,

    -- ** Optics
    Utils._PathTypeFile,
    Utils._PathTypeDirectory,
    Utils._PathTypeSymbolicLink,

    -- * Utils
    Utils.throwPathIOError,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.PosixCompat.Utils
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeSymbolicLink
      ),
  )
import Effectful.PosixCompat.Utils qualified as Utils
import GHC.IO.Exception (IOErrorType (InappropriateType))
import System.PosixCompat.Files (FileStatus, PathVar)
import System.PosixCompat.Files qualified as PFiles
import System.PosixCompat.Types
  ( DeviceID,
    EpochTime,
    Fd,
    FileMode,
    FileOffset,
    GroupID,
    Limit,
    UserID,
  )

-- | Dynamic effect for "System.PosixCompat.Files".
--
-- @since 0.1
data PosixCompatDynamic :: Effect where
  SetFileMode :: FilePath -> FileMode -> PosixCompatDynamic m ()
  SetFdMode :: Fd -> FileMode -> PosixCompatDynamic m ()
  SetFileCreationMask :: FileMode -> PosixCompatDynamic m FileMode
  FileAccess :: FilePath -> Bool -> Bool -> Bool -> PosixCompatDynamic m Bool
  FileExist :: FilePath -> PosixCompatDynamic m Bool
  GetFileStatus :: FilePath -> PosixCompatDynamic m FileStatus
  GetFdStatus :: Fd -> PosixCompatDynamic m FileStatus
  GetSymbolicLinkStatus :: FilePath -> PosixCompatDynamic m FileStatus
  CreateNamedPipe :: FilePath -> FileMode -> PosixCompatDynamic m ()
  CreateDevice :: FilePath -> FileMode -> DeviceID -> PosixCompatDynamic m ()
  CreateLink :: FilePath -> FilePath -> PosixCompatDynamic m ()
  RemoveLink :: FilePath -> PosixCompatDynamic m ()
  CreateSymbolicLink :: FilePath -> FilePath -> PosixCompatDynamic m ()
  ReadSymbolicLink :: FilePath -> PosixCompatDynamic m FilePath
  Rename :: FilePath -> FilePath -> PosixCompatDynamic m ()
  SetOwnerAndGroup :: FilePath -> UserID -> GroupID -> PosixCompatDynamic m ()
  SetFdOwnerAndGroup :: Fd -> UserID -> GroupID -> PosixCompatDynamic m ()
  SetSymbolicLinkOwnerAndGroup ::
    FilePath -> UserID -> GroupID -> PosixCompatDynamic m ()
  SetFileTimes :: FilePath -> EpochTime -> EpochTime -> PosixCompatDynamic m ()
  TouchFile :: FilePath -> PosixCompatDynamic m ()
  SetFileSize :: FilePath -> FileOffset -> PosixCompatDynamic m ()
  SetFdSize :: Fd -> FileOffset -> PosixCompatDynamic m ()
  GetPathVar :: FilePath -> PathVar -> PosixCompatDynamic m Limit
  GetFdPathVar :: Fd -> PathVar -> PosixCompatDynamic m Limit

-- | @since 0.1
type instance DispatchOf PosixCompatDynamic = Dynamic

-- | Runs 'PosixCompatDynamic' in 'IO'.
--
-- @since 0.1
runPosixCompatDynamicIO ::
  (IOE :> es) =>
  Eff (PosixCompatDynamic : es) a ->
  Eff es a
runPosixCompatDynamicIO = interpret $ \_ -> \case
  SetFileMode p m -> liftIO $ PFiles.setFileMode p m
  SetFdMode fd m -> liftIO $ PFiles.setFdMode fd m
  SetFileCreationMask m -> liftIO $ PFiles.setFileCreationMask m
  FileAccess p b c d -> liftIO $ PFiles.fileAccess p b c d
  FileExist p -> liftIO $ PFiles.fileExist p
  GetFileStatus p -> liftIO $ PFiles.getFileStatus p
  GetFdStatus fd -> liftIO $ PFiles.getFdStatus fd
  GetSymbolicLinkStatus p -> liftIO $ PFiles.getSymbolicLinkStatus p
  CreateNamedPipe p m -> liftIO $ PFiles.createNamedPipe p m
  CreateDevice p m did -> liftIO $ PFiles.createDevice p m did
  CreateLink p1 p2 -> liftIO $ PFiles.createLink p1 p2
  RemoveLink p -> liftIO $ PFiles.removeLink p
  CreateSymbolicLink p1 p2 -> liftIO $ PFiles.createSymbolicLink p1 p2
  ReadSymbolicLink p -> liftIO $ PFiles.readSymbolicLink p
  Rename p1 p2 -> liftIO $ PFiles.rename p1 p2
  SetOwnerAndGroup p uid gid -> liftIO $ PFiles.setOwnerAndGroup p uid gid
  SetFdOwnerAndGroup fd uid gid ->
    liftIO $ PFiles.setFdOwnerAndGroup fd uid gid
  SetSymbolicLinkOwnerAndGroup p uid gid ->
    liftIO $ PFiles.setSymbolicLinkOwnerAndGroup p uid gid
  SetFileTimes p t1 t2 -> liftIO $ PFiles.setFileTimes p t1 t2
  TouchFile p -> liftIO $ PFiles.touchFile p
  SetFileSize p oset -> liftIO $ PFiles.setFileSize p oset
  SetFdSize fd oset -> liftIO $ PFiles.setFdSize fd oset
  GetPathVar p m -> liftIO $ PFiles.getPathVar p m
  GetFdPathVar fd m -> liftIO $ PFiles.getFdPathVar fd m

-- | Lifted 'PFiles.setFileMode'.
--
-- @since 0.1
setFileMode :: (PosixCompatDynamic :> es) => FilePath -> FileMode -> Eff es ()
setFileMode p = send . SetFileMode p

-- | Lifted 'PFiles.setFdMode'.
--
-- @since 0.1
setFdMode :: (PosixCompatDynamic :> es) => Fd -> FileMode -> Eff es ()
setFdMode p = send . SetFdMode p

-- | Lifted 'PFiles.setFileCreationMask'.
--
-- @since 0.1
setFileCreationMask :: (PosixCompatDynamic :> es) => FileMode -> Eff es FileMode
setFileCreationMask = send . SetFileCreationMask

-- | Lifted 'PFiles.fileAccess'.
--
-- @since 0.1
fileAccess ::
  (PosixCompatDynamic :> es) =>
  FilePath ->
  Bool ->
  Bool ->
  Bool ->
  Eff es Bool
fileAccess p b c = send . FileAccess p b c

-- | Lifted 'PFiles.fileExist'.
--
-- @since 0.1
fileExist :: (PosixCompatDynamic :> es) => FilePath -> Eff es Bool
fileExist = send . FileExist

-- | Lifted 'PFiles.getFileStatus'.
--
-- @since 0.1
getFileStatus :: (PosixCompatDynamic :> es) => FilePath -> Eff es FileStatus
getFileStatus = send . GetFileStatus

-- | Lifted 'PFiles.getFdStatus'.
--
-- @since 0.1
getFdStatus :: (PosixCompatDynamic :> es) => Fd -> Eff es FileStatus
getFdStatus = send . GetFdStatus

-- | Lifted 'PFiles.getSymbolicLinkStatus'.
--
-- @since 0.1
getSymbolicLinkStatus ::
  (PosixCompatDynamic :> es) =>
  FilePath ->
  Eff es FileStatus
getSymbolicLinkStatus = send . GetSymbolicLinkStatus

-- | Lifted 'PFiles.createNamedPipe'.
--
-- @since 0.1
createNamedPipe ::
  (PosixCompatDynamic :> es) =>
  FilePath ->
  FileMode ->
  Eff es ()
createNamedPipe p = send . CreateNamedPipe p

-- | Lifted 'PFiles.createDevice'.
--
-- @since 0.1
createDevice ::
  (PosixCompatDynamic :> es) =>
  FilePath ->
  FileMode ->
  DeviceID ->
  Eff es ()
createDevice p m = send . CreateDevice p m

-- | Lifted 'PFiles.createLink'.
--
-- @since 0.1
createLink :: (PosixCompatDynamic :> es) => FilePath -> FilePath -> Eff es ()
createLink p = send . CreateLink p

-- | Lifted 'PFiles.removeLink'.
--
-- @since 0.1
removeLink :: (PosixCompatDynamic :> es) => FilePath -> Eff es ()
removeLink = send . RemoveLink

-- | Lifted 'PFiles.createSymbolicLink'.
--
-- @since 0.1
createSymbolicLink ::
  (PosixCompatDynamic :> es) =>
  FilePath ->
  FilePath ->
  Eff es ()
createSymbolicLink p = send . CreateSymbolicLink p

-- | Lifted 'PFiles.readSymbolicLink'.
--
-- @since 0.1
readSymbolicLink :: (PosixCompatDynamic :> es) => FilePath -> Eff es FilePath
readSymbolicLink = send . ReadSymbolicLink

-- | Lifted 'PFiles.rename'.
--
-- @since 0.1
rename :: (PosixCompatDynamic :> es) => FilePath -> FilePath -> Eff es ()
rename p = send . Rename p

-- | Lifted 'PFiles.setOwnerAndGroup'.
--
-- @since 0.1
setOwnerAndGroup ::
  (PosixCompatDynamic :> es) =>
  FilePath ->
  UserID ->
  GroupID ->
  Eff es ()
setOwnerAndGroup p uid = send . SetOwnerAndGroup p uid

-- | Lifted 'PFiles.setFdOwnerAndGroup'.
--
-- @since 0.1
setFdOwnerAndGroup ::
  (PosixCompatDynamic :> es) =>
  Fd ->
  UserID ->
  GroupID ->
  Eff es ()
setFdOwnerAndGroup fd uid = send . SetFdOwnerAndGroup fd uid

-- | Lifted 'PFiles.setSymbolicLinkOwnerAndGroup'.
--
-- @since 0.1
setSymbolicLinkOwnerAndGroup ::
  (PosixCompatDynamic :> es) =>
  FilePath ->
  UserID ->
  GroupID ->
  Eff es ()
setSymbolicLinkOwnerAndGroup p uid = send . SetSymbolicLinkOwnerAndGroup p uid

-- | Lifted 'PFiles.setFileTimes'.
--
-- @since 0.1
setFileTimes ::
  (PosixCompatDynamic :> es) =>
  FilePath ->
  EpochTime ->
  EpochTime ->
  Eff es ()
setFileTimes p t = send . SetFileTimes p t

-- | Lifted 'PFiles.touchFile'.
--
-- @since 0.1
touchFile :: (PosixCompatDynamic :> es) => FilePath -> Eff es ()
touchFile = send . TouchFile

-- | Lifted 'PFiles.setFileSize'.
--
-- @since 0.1
setFileSize :: (PosixCompatDynamic :> es) => FilePath -> FileOffset -> Eff es ()
setFileSize p = send . SetFileSize p

-- | Lifted 'PFiles.setFdSize'.
--
-- @since 0.1
setFdSize :: (PosixCompatDynamic :> es) => Fd -> FileOffset -> Eff es ()
setFdSize fd = send . SetFdSize fd

-- | Lifted 'PFiles.getPathVar'.
--
-- @since 0.1
getPathVar :: (PosixCompatDynamic :> es) => FilePath -> PathVar -> Eff es Limit
getPathVar p = send . GetPathVar p

-- | Lifted 'PFiles.getFdPathVar'.
--
-- @since 0.1
getFdPathVar :: (PosixCompatDynamic :> es) => Fd -> PathVar -> Eff es Limit
getFdPathVar fd = send . GetFdPathVar fd

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- @since 0.1
throwIfWrongPathType ::
  ( PosixCompatDynamic :> es
  ) =>
  String ->
  PathType ->
  FilePath ->
  Eff es ()
throwIfWrongPathType location expected path = do
  actual <- getPathType path

  let err =
        mconcat
          [ "Expected path '",
            path,
            "' to have type ",
            Utils.displayPathType expected,
            ", but detected ",
            Utils.displayPathType actual
          ]

  unless (expected == actual) $
    Utils.throwPathIOError
      path
      location
      InappropriateType
      err

-- | Checks that the path type matches the expectation. Throws
-- 'IOException' if the path does not exist or the type cannot be detected.
--
-- @since 0.1
isPathType ::
  ( PosixCompatDynamic :> es
  ) =>
  PathType ->
  FilePath ->
  Eff es Bool
isPathType expected = fmap (== expected) . getPathType

-- | Returns the type for a given path without following symlinks.
-- Throws 'IOException' if the path does not exist or the type cannot be
-- detected.
--
-- @since 0.1
getPathType ::
  ( PosixCompatDynamic :> es
  ) =>
  FilePath ->
  Eff es PathType
getPathType path = do
  status <- getSymbolicLinkStatus path
  if
    | PFiles.isSymbolicLink status -> pure PathTypeSymbolicLink
    | PFiles.isDirectory status -> pure PathTypeDirectory
    | PFiles.isRegularFile status -> pure PathTypeFile
    | otherwise ->
        Utils.throwPathIOError
          path
          "getPathType"
          InappropriateType
          "path exists but has unknown type"
