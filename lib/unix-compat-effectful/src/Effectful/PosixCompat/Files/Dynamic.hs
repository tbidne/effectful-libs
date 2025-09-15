-- | Provides a dynamic effect for "System.PosixCompat.Files".
--
-- @since 0.1
module Effectful.PosixCompat.Files.Dynamic
  ( -- * Effect
    PosixCompatFiles (..),
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
    runPosixFilesCompat,

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
import Data.Functor ((<&>))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret_, send)
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.PosixCompat.Files.Static qualified as Static
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
data PosixCompatFiles :: Effect where
  SetFileMode :: FilePath -> FileMode -> PosixCompatFiles m ()
  SetFdMode :: Fd -> FileMode -> PosixCompatFiles m ()
  SetFileCreationMask :: FileMode -> PosixCompatFiles m FileMode
  FileAccess :: FilePath -> Bool -> Bool -> Bool -> PosixCompatFiles m Bool
  FileExist :: FilePath -> PosixCompatFiles m Bool
  GetFileStatus :: FilePath -> PosixCompatFiles m FileStatus
  GetFdStatus :: Fd -> PosixCompatFiles m FileStatus
  GetSymbolicLinkStatus :: FilePath -> PosixCompatFiles m FileStatus
  CreateNamedPipe :: FilePath -> FileMode -> PosixCompatFiles m ()
  CreateDevice :: FilePath -> FileMode -> DeviceID -> PosixCompatFiles m ()
  CreateLink :: FilePath -> FilePath -> PosixCompatFiles m ()
  RemoveLink :: FilePath -> PosixCompatFiles m ()
  CreateSymbolicLink :: FilePath -> FilePath -> PosixCompatFiles m ()
  ReadSymbolicLink :: FilePath -> PosixCompatFiles m FilePath
  Rename :: FilePath -> FilePath -> PosixCompatFiles m ()
  SetOwnerAndGroup :: FilePath -> UserID -> GroupID -> PosixCompatFiles m ()
  SetFdOwnerAndGroup :: Fd -> UserID -> GroupID -> PosixCompatFiles m ()
  SetSymbolicLinkOwnerAndGroup ::
    FilePath -> UserID -> GroupID -> PosixCompatFiles m ()
  SetFileTimes :: FilePath -> EpochTime -> EpochTime -> PosixCompatFiles m ()
  TouchFile :: FilePath -> PosixCompatFiles m ()
  SetFileSize :: FilePath -> FileOffset -> PosixCompatFiles m ()
  SetFdSize :: Fd -> FileOffset -> PosixCompatFiles m ()
  GetPathVar :: FilePath -> PathVar -> PosixCompatFiles m Limit
  GetFdPathVar :: Fd -> PathVar -> PosixCompatFiles m Limit

-- | @since 0.1
type instance DispatchOf PosixCompatFiles = Dynamic

-- | @since 0.1
instance ShowEffect PosixCompatFiles where
  showEffectCons = \case
    SetFileMode _ _ -> "SetFileMode"
    SetFdMode _ _ -> "SetFdMode"
    SetFileCreationMask _ -> "SetFileCreationMask"
    FileAccess _ _ _ _ -> "FileAccess"
    FileExist _ -> "FileExist"
    GetFileStatus _ -> "GetFileStatus"
    GetFdStatus _ -> "GetFdStatus"
    GetSymbolicLinkStatus _ -> "GetSymbolicLinkStatus"
    CreateNamedPipe _ _ -> "CreateNamedPipe"
    CreateDevice _ _ _ -> "CreateDevice"
    CreateLink _ _ -> "CreateLink"
    RemoveLink _ -> "RemoveLink"
    CreateSymbolicLink _ _ -> "CreateSymbolicLink"
    ReadSymbolicLink _ -> "ReadSymbolicLink"
    Rename _ _ -> "Rename"
    SetOwnerAndGroup _ _ _ -> "SetOwnerAndGroup"
    SetFdOwnerAndGroup _ _ _ -> "SetFdOwnerAndGroup"
    SetSymbolicLinkOwnerAndGroup _ _ _ -> "SetSymbolicLinkOwnerAndGroup"
    SetFileTimes _ _ _ -> "SetFileTimes"
    TouchFile _ -> "TouchFile"
    SetFileSize _ _ -> "SetFileSize"
    SetFdSize _ _ -> "SetFdSize"
    GetPathVar _ _ -> "GetPathVar"
    GetFdPathVar _ _ -> "GetFdPathVar"

-- | Runs 'PosixCompatFiles' in 'IO'.
--
-- @since 0.1
runPosixFilesCompat ::
  (HasCallStack, IOE :> es) =>
  Eff (PosixCompatFiles : es) a ->
  Eff es a
runPosixFilesCompat = reinterpret_ Static.runPosixFilesCompat $ \case
  SetFileMode p m -> Static.setFileMode p m
  SetFdMode fd m -> Static.setFdMode fd m
  SetFileCreationMask m -> Static.setFileCreationMask m
  FileAccess p b c d -> Static.fileAccess p b c d
  FileExist p -> Static.fileExist p
  GetFileStatus p -> Static.getFileStatus p
  GetFdStatus fd -> Static.getFdStatus fd
  GetSymbolicLinkStatus p -> Static.getSymbolicLinkStatus p
  CreateNamedPipe p m -> Static.createNamedPipe p m
  CreateDevice p m did -> Static.createDevice p m did
  CreateLink p1 p2 -> Static.createLink p1 p2
  RemoveLink p -> Static.removeLink p
  CreateSymbolicLink p1 p2 -> Static.createSymbolicLink p1 p2
  ReadSymbolicLink p -> Static.readSymbolicLink p
  Rename p1 p2 -> Static.rename p1 p2
  SetOwnerAndGroup p uid gid -> Static.setOwnerAndGroup p uid gid
  SetFdOwnerAndGroup fd uid gid ->
    Static.setFdOwnerAndGroup fd uid gid
  SetSymbolicLinkOwnerAndGroup p uid gid ->
    Static.setSymbolicLinkOwnerAndGroup p uid gid
  SetFileTimes p t1 t2 -> Static.setFileTimes p t1 t2
  TouchFile p -> Static.touchFile p
  SetFileSize p oset -> Static.setFileSize p oset
  SetFdSize fd oset -> Static.setFdSize fd oset
  GetPathVar p m -> Static.getPathVar p m
  GetFdPathVar fd m -> Static.getFdPathVar fd m

-- | Lifted 'PFiles.setFileMode'.
--
-- @since 0.1
setFileMode ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  FileMode ->
  Eff es ()
setFileMode p = send . SetFileMode p

-- | Lifted 'PFiles.setFdMode'.
--
-- @since 0.1
setFdMode ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  Fd ->
  FileMode ->
  Eff es ()
setFdMode p = send . SetFdMode p

-- | Lifted 'PFiles.setFileCreationMask'.
--
-- @since 0.1
setFileCreationMask ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FileMode ->
  Eff es FileMode
setFileCreationMask = send . SetFileCreationMask

-- | Lifted 'PFiles.fileAccess'.
--
-- @since 0.1
fileAccess ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  Bool ->
  Bool ->
  Bool ->
  Eff es Bool
fileAccess p b c = send . FileAccess p b c

-- | Lifted 'PFiles.fileExist'.
--
-- @since 0.1
fileExist ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  Eff es Bool
fileExist = send . FileExist

-- | Lifted 'PFiles.getFileStatus'.
--
-- @since 0.1
getFileStatus ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  Eff es FileStatus
getFileStatus = send . GetFileStatus

-- | Lifted 'PFiles.getFdStatus'.
--
-- @since 0.1
getFdStatus ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  Fd ->
  Eff es FileStatus
getFdStatus = send . GetFdStatus

-- | Lifted 'PFiles.getSymbolicLinkStatus'.
--
-- @since 0.1
getSymbolicLinkStatus ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  Eff es FileStatus
getSymbolicLinkStatus = send . GetSymbolicLinkStatus

-- | Lifted 'PFiles.createNamedPipe'.
--
-- @since 0.1
createNamedPipe ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  FileMode ->
  Eff es ()
createNamedPipe p = send . CreateNamedPipe p

-- | Lifted 'PFiles.createDevice'.
--
-- @since 0.1
createDevice ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  FileMode ->
  DeviceID ->
  Eff es ()
createDevice p m = send . CreateDevice p m

-- | Lifted 'PFiles.createLink'.
--
-- @since 0.1
createLink ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  FilePath ->
  Eff es ()
createLink p = send . CreateLink p

-- | Lifted 'PFiles.removeLink'.
--
-- @since 0.1
removeLink ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  Eff es ()
removeLink = send . RemoveLink

-- | Lifted 'PFiles.createSymbolicLink'.
--
-- @since 0.1
createSymbolicLink ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  FilePath ->
  Eff es ()
createSymbolicLink p = send . CreateSymbolicLink p

-- | Lifted 'PFiles.readSymbolicLink'.
--
-- @since 0.1
readSymbolicLink ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  Eff es FilePath
readSymbolicLink = send . ReadSymbolicLink

-- | Lifted 'PFiles.rename'.
--
-- @since 0.1
rename ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  FilePath ->
  Eff es ()
rename p = send . Rename p

-- | Lifted 'PFiles.setOwnerAndGroup'.
--
-- @since 0.1
setOwnerAndGroup ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  UserID ->
  GroupID ->
  Eff es ()
setOwnerAndGroup p uid = send . SetOwnerAndGroup p uid

-- | Lifted 'PFiles.setFdOwnerAndGroup'.
--
-- @since 0.1
setFdOwnerAndGroup ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  Fd ->
  UserID ->
  GroupID ->
  Eff es ()
setFdOwnerAndGroup fd uid = send . SetFdOwnerAndGroup fd uid

-- | Lifted 'PFiles.setSymbolicLinkOwnerAndGroup'.
--
-- @since 0.1
setSymbolicLinkOwnerAndGroup ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  UserID ->
  GroupID ->
  Eff es ()
setSymbolicLinkOwnerAndGroup p uid = send . SetSymbolicLinkOwnerAndGroup p uid

-- | Lifted 'PFiles.setFileTimes'.
--
-- @since 0.1
setFileTimes ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  EpochTime ->
  EpochTime ->
  Eff es ()
setFileTimes p t = send . SetFileTimes p t

-- | Lifted 'PFiles.touchFile'.
--
-- @since 0.1
touchFile ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  Eff es ()
touchFile = send . TouchFile

-- | Lifted 'PFiles.setFileSize'.
--
-- @since 0.1
setFileSize ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  FileOffset ->
  Eff es ()
setFileSize p = send . SetFileSize p

-- | Lifted 'PFiles.setFdSize'.
--
-- @since 0.1
setFdSize ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  Fd ->
  FileOffset ->
  Eff es ()
setFdSize fd = send . SetFdSize fd

-- | Lifted 'PFiles.getPathVar'.
--
-- @since 0.1
getPathVar ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  FilePath ->
  PathVar ->
  Eff es Limit
getPathVar p = send . GetPathVar p

-- | Lifted 'PFiles.getFdPathVar'.
--
-- @since 0.1
getFdPathVar ::
  ( HasCallStack,
    PosixCompatFiles :> es
  ) =>
  Fd ->
  PathVar ->
  Eff es Limit
getFdPathVar fd = send . GetFdPathVar fd

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- @since 0.1
throwIfWrongPathType ::
  ( HasCallStack,
    PosixCompatFiles :> es
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
    FS.IO.throwFilePathIOError
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
    PosixCompatFiles :> es
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
  ( HasCallStack,
    PosixCompatFiles :> es
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
