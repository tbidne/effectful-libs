-- | Provides a dynamic effect for "System.PosixCompat.Files".
--
-- @since 0.1
module Effectful.PosixCompat.Dynamic
  ( -- * Effect
    PosixCompat (..),
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
    runPosixCompat,

    -- * PathType
    PathType (..),
    displayPathType,

    -- ** Functions
    throwIfWrongPathType,
    isPathType,
    getPathType,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import FileSystem.IO qualified as FS.IO
import FileSystem.PathType
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeOther,
        PathTypeSymbolicLink
      ),
    displayPathType,
  )
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
data PosixCompat :: Effect where
  SetFileMode :: FilePath -> FileMode -> PosixCompat m ()
  SetFdMode :: Fd -> FileMode -> PosixCompat m ()
  SetFileCreationMask :: FileMode -> PosixCompat m FileMode
  FileAccess :: FilePath -> Bool -> Bool -> Bool -> PosixCompat m Bool
  FileExist :: FilePath -> PosixCompat m Bool
  GetFileStatus :: FilePath -> PosixCompat m FileStatus
  GetFdStatus :: Fd -> PosixCompat m FileStatus
  GetSymbolicLinkStatus :: FilePath -> PosixCompat m FileStatus
  CreateNamedPipe :: FilePath -> FileMode -> PosixCompat m ()
  CreateDevice :: FilePath -> FileMode -> DeviceID -> PosixCompat m ()
  CreateLink :: FilePath -> FilePath -> PosixCompat m ()
  RemoveLink :: FilePath -> PosixCompat m ()
  CreateSymbolicLink :: FilePath -> FilePath -> PosixCompat m ()
  ReadSymbolicLink :: FilePath -> PosixCompat m FilePath
  Rename :: FilePath -> FilePath -> PosixCompat m ()
  SetOwnerAndGroup :: FilePath -> UserID -> GroupID -> PosixCompat m ()
  SetFdOwnerAndGroup :: Fd -> UserID -> GroupID -> PosixCompat m ()
  SetSymbolicLinkOwnerAndGroup ::
    FilePath -> UserID -> GroupID -> PosixCompat m ()
  SetFileTimes :: FilePath -> EpochTime -> EpochTime -> PosixCompat m ()
  TouchFile :: FilePath -> PosixCompat m ()
  SetFileSize :: FilePath -> FileOffset -> PosixCompat m ()
  SetFdSize :: Fd -> FileOffset -> PosixCompat m ()
  GetPathVar :: FilePath -> PathVar -> PosixCompat m Limit
  GetFdPathVar :: Fd -> PathVar -> PosixCompat m Limit

-- | @since 0.1
type instance DispatchOf PosixCompat = Dynamic

-- | Runs 'PosixCompat' in 'IO'.
--
-- @since 0.1
runPosixCompat ::
  (IOE :> es) =>
  Eff (PosixCompat : es) a ->
  Eff es a
runPosixCompat = interpret $ \_ -> \case
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
setFileMode :: (PosixCompat :> es) => FilePath -> FileMode -> Eff es ()
setFileMode p = send . SetFileMode p

-- | Lifted 'PFiles.setFdMode'.
--
-- @since 0.1
setFdMode :: (PosixCompat :> es) => Fd -> FileMode -> Eff es ()
setFdMode p = send . SetFdMode p

-- | Lifted 'PFiles.setFileCreationMask'.
--
-- @since 0.1
setFileCreationMask :: (PosixCompat :> es) => FileMode -> Eff es FileMode
setFileCreationMask = send . SetFileCreationMask

-- | Lifted 'PFiles.fileAccess'.
--
-- @since 0.1
fileAccess ::
  (PosixCompat :> es) =>
  FilePath ->
  Bool ->
  Bool ->
  Bool ->
  Eff es Bool
fileAccess p b c = send . FileAccess p b c

-- | Lifted 'PFiles.fileExist'.
--
-- @since 0.1
fileExist :: (PosixCompat :> es) => FilePath -> Eff es Bool
fileExist = send . FileExist

-- | Lifted 'PFiles.getFileStatus'.
--
-- @since 0.1
getFileStatus :: (PosixCompat :> es) => FilePath -> Eff es FileStatus
getFileStatus = send . GetFileStatus

-- | Lifted 'PFiles.getFdStatus'.
--
-- @since 0.1
getFdStatus :: (PosixCompat :> es) => Fd -> Eff es FileStatus
getFdStatus = send . GetFdStatus

-- | Lifted 'PFiles.getSymbolicLinkStatus'.
--
-- @since 0.1
getSymbolicLinkStatus ::
  (PosixCompat :> es) =>
  FilePath ->
  Eff es FileStatus
getSymbolicLinkStatus = send . GetSymbolicLinkStatus

-- | Lifted 'PFiles.createNamedPipe'.
--
-- @since 0.1
createNamedPipe ::
  (PosixCompat :> es) =>
  FilePath ->
  FileMode ->
  Eff es ()
createNamedPipe p = send . CreateNamedPipe p

-- | Lifted 'PFiles.createDevice'.
--
-- @since 0.1
createDevice ::
  (PosixCompat :> es) =>
  FilePath ->
  FileMode ->
  DeviceID ->
  Eff es ()
createDevice p m = send . CreateDevice p m

-- | Lifted 'PFiles.createLink'.
--
-- @since 0.1
createLink :: (PosixCompat :> es) => FilePath -> FilePath -> Eff es ()
createLink p = send . CreateLink p

-- | Lifted 'PFiles.removeLink'.
--
-- @since 0.1
removeLink :: (PosixCompat :> es) => FilePath -> Eff es ()
removeLink = send . RemoveLink

-- | Lifted 'PFiles.createSymbolicLink'.
--
-- @since 0.1
createSymbolicLink ::
  (PosixCompat :> es) =>
  FilePath ->
  FilePath ->
  Eff es ()
createSymbolicLink p = send . CreateSymbolicLink p

-- | Lifted 'PFiles.readSymbolicLink'.
--
-- @since 0.1
readSymbolicLink :: (PosixCompat :> es) => FilePath -> Eff es FilePath
readSymbolicLink = send . ReadSymbolicLink

-- | Lifted 'PFiles.rename'.
--
-- @since 0.1
rename :: (PosixCompat :> es) => FilePath -> FilePath -> Eff es ()
rename p = send . Rename p

-- | Lifted 'PFiles.setOwnerAndGroup'.
--
-- @since 0.1
setOwnerAndGroup ::
  (PosixCompat :> es) =>
  FilePath ->
  UserID ->
  GroupID ->
  Eff es ()
setOwnerAndGroup p uid = send . SetOwnerAndGroup p uid

-- | Lifted 'PFiles.setFdOwnerAndGroup'.
--
-- @since 0.1
setFdOwnerAndGroup ::
  (PosixCompat :> es) =>
  Fd ->
  UserID ->
  GroupID ->
  Eff es ()
setFdOwnerAndGroup fd uid = send . SetFdOwnerAndGroup fd uid

-- | Lifted 'PFiles.setSymbolicLinkOwnerAndGroup'.
--
-- @since 0.1
setSymbolicLinkOwnerAndGroup ::
  (PosixCompat :> es) =>
  FilePath ->
  UserID ->
  GroupID ->
  Eff es ()
setSymbolicLinkOwnerAndGroup p uid = send . SetSymbolicLinkOwnerAndGroup p uid

-- | Lifted 'PFiles.setFileTimes'.
--
-- @since 0.1
setFileTimes ::
  (PosixCompat :> es) =>
  FilePath ->
  EpochTime ->
  EpochTime ->
  Eff es ()
setFileTimes p t = send . SetFileTimes p t

-- | Lifted 'PFiles.touchFile'.
--
-- @since 0.1
touchFile :: (PosixCompat :> es) => FilePath -> Eff es ()
touchFile = send . TouchFile

-- | Lifted 'PFiles.setFileSize'.
--
-- @since 0.1
setFileSize :: (PosixCompat :> es) => FilePath -> FileOffset -> Eff es ()
setFileSize p = send . SetFileSize p

-- | Lifted 'PFiles.setFdSize'.
--
-- @since 0.1
setFdSize :: (PosixCompat :> es) => Fd -> FileOffset -> Eff es ()
setFdSize fd = send . SetFdSize fd

-- | Lifted 'PFiles.getPathVar'.
--
-- @since 0.1
getPathVar :: (PosixCompat :> es) => FilePath -> PathVar -> Eff es Limit
getPathVar p = send . GetPathVar p

-- | Lifted 'PFiles.getFdPathVar'.
--
-- @since 0.1
getFdPathVar :: (PosixCompat :> es) => Fd -> PathVar -> Eff es Limit
getFdPathVar fd = send . GetFdPathVar fd

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- @since 0.1
throwIfWrongPathType ::
  ( PosixCompat :> es
  ) =>
  String ->
  PathType ->
  FilePath ->
  Eff es ()
throwIfWrongPathType location expected path = do
  actual <- getPathType path

  let err =
        mconcat
          [ "Expected path to have type ",
            displayPathType expected,
            ", but detected ",
            displayPathType actual
          ]

  unless (expected == actual) $
    FS.IO.throwPathIOErrorFilePath
      path
      location
      InappropriateType
      err

-- | Checks that the path type matches the expectation. Throws
-- 'IOException' if the path does not exist or the type cannot be detected.
--
-- @since 0.1
isPathType ::
  ( PosixCompat :> es
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
  ( PosixCompat :> es
  ) =>
  FilePath ->
  Eff es PathType
getPathType path =
  getSymbolicLinkStatus path <&> \status ->
    if
      | PFiles.isSymbolicLink status -> PathTypeSymbolicLink
      | PFiles.isDirectory status -> PathTypeDirectory
      | PFiles.isRegularFile status -> PathTypeFile
      | otherwise -> PathTypeOther
