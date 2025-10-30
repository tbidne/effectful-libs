-- | Provides a dynamic effect for "System.Posix.Files".
--
-- @since 0.1
module Effectful.Posix.Files.Dynamic
  ( -- * Effect
    PosixFiles (..),
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
    runPosixFiles,

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
import Effectful.Posix.Files.Static qualified as Static
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
import System.OsString.Internal.Types (OsString (OsString))
import System.Posix.Files.PosixString (FileStatus, PathVar)
import System.Posix.Files.PosixString qualified as PFiles
import System.Posix.PosixString (PosixPath)
import System.Posix.Types
  ( DeviceID,
    EpochTime,
    Fd,
    FileMode,
    FileOffset,
    GroupID,
    Limit,
    UserID,
  )

-- | Dynamic effect for "System.Posix.Files".
--
-- @since 0.1
data PosixFiles :: Effect where
  SetFileMode :: PosixPath -> FileMode -> PosixFiles m ()
  SetFdMode :: Fd -> FileMode -> PosixFiles m ()
  SetFileCreationMask :: FileMode -> PosixFiles m FileMode
  FileAccess :: PosixPath -> Bool -> Bool -> Bool -> PosixFiles m Bool
  FileExist :: PosixPath -> PosixFiles m Bool
  GetFileStatus :: PosixPath -> PosixFiles m FileStatus
  GetFdStatus :: Fd -> PosixFiles m FileStatus
  GetSymbolicLinkStatus :: PosixPath -> PosixFiles m FileStatus
  CreateNamedPipe :: PosixPath -> FileMode -> PosixFiles m ()
  CreateDevice :: PosixPath -> FileMode -> DeviceID -> PosixFiles m ()
  CreateLink :: PosixPath -> PosixPath -> PosixFiles m ()
  RemoveLink :: PosixPath -> PosixFiles m ()
  CreateSymbolicLink :: PosixPath -> PosixPath -> PosixFiles m ()
  ReadSymbolicLink :: PosixPath -> PosixFiles m PosixPath
  Rename :: PosixPath -> PosixPath -> PosixFiles m ()
  SetOwnerAndGroup :: PosixPath -> UserID -> GroupID -> PosixFiles m ()
  SetFdOwnerAndGroup :: Fd -> UserID -> GroupID -> PosixFiles m ()
  SetSymbolicLinkOwnerAndGroup ::
    PosixPath -> UserID -> GroupID -> PosixFiles m ()
  SetFileTimes :: PosixPath -> EpochTime -> EpochTime -> PosixFiles m ()
  TouchFile :: PosixPath -> PosixFiles m ()
  SetFileSize :: PosixPath -> FileOffset -> PosixFiles m ()
  SetFdSize :: Fd -> FileOffset -> PosixFiles m ()
  GetPathVar :: PosixPath -> PathVar -> PosixFiles m Limit
  GetFdPathVar :: Fd -> PathVar -> PosixFiles m Limit

-- | @since 0.1
type instance DispatchOf PosixFiles = Dynamic

-- | @since 0.1
instance ShowEffect PosixFiles where
  showEffectCons = \case
    SetFileMode _ _ -> "SetFileMode"
    SetFdMode _ _ -> "SetFdMode"
    SetFileCreationMask _ -> "SetFileCreationMask"
    FileAccess {} -> "FileAccess"
    FileExist _ -> "FileExist"
    GetFileStatus _ -> "GetFileStatus"
    GetFdStatus _ -> "GetFdStatus"
    GetSymbolicLinkStatus _ -> "GetSymbolicLinkStatus"
    CreateNamedPipe _ _ -> "CreateNamedPipe"
    CreateDevice {} -> "CreateDevice"
    CreateLink _ _ -> "CreateLink"
    RemoveLink _ -> "RemoveLink"
    CreateSymbolicLink _ _ -> "CreateSymbolicLink"
    ReadSymbolicLink _ -> "ReadSymbolicLink"
    Rename _ _ -> "Rename"
    SetOwnerAndGroup {} -> "SetOwnerAndGroup"
    SetFdOwnerAndGroup {} -> "SetFdOwnerAndGroup"
    SetSymbolicLinkOwnerAndGroup {} -> "SetSymbolicLinkOwnerAndGroup"
    SetFileTimes {} -> "SetFileTimes"
    TouchFile _ -> "TouchFile"
    SetFileSize _ _ -> "SetFileSize"
    SetFdSize _ _ -> "SetFdSize"
    GetPathVar _ _ -> "GetPathVar"
    GetFdPathVar _ _ -> "GetFdPathVar"

-- | Runs 'PosixFiles' in 'IO'.
--
-- @since 0.1
runPosixFiles ::
  (HasCallStack, IOE :> es) =>
  Eff (PosixFiles : es) a ->
  Eff es a
runPosixFiles = reinterpret_ Static.runPosixFiles $ \case
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
    PosixFiles :> es
  ) =>
  PosixPath ->
  FileMode ->
  Eff es ()
setFileMode p = send . SetFileMode p

-- | Lifted 'PFiles.setFdMode'.
--
-- @since 0.1
setFdMode ::
  ( HasCallStack,
    PosixFiles :> es
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
    PosixFiles :> es
  ) =>
  FileMode ->
  Eff es FileMode
setFileCreationMask = send . SetFileCreationMask

-- | Lifted 'PFiles.fileAccess'.
--
-- @since 0.1
fileAccess ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
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
    PosixFiles :> es
  ) =>
  PosixPath ->
  Eff es Bool
fileExist = send . FileExist

-- | Lifted 'PFiles.getFileStatus'.
--
-- @since 0.1
getFileStatus ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  Eff es FileStatus
getFileStatus = send . GetFileStatus

-- | Lifted 'PFiles.getFdStatus'.
--
-- @since 0.1
getFdStatus ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  Fd ->
  Eff es FileStatus
getFdStatus = send . GetFdStatus

-- | Lifted 'PFiles.getSymbolicLinkStatus'.
--
-- @since 0.1
getSymbolicLinkStatus ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  Eff es FileStatus
getSymbolicLinkStatus = send . GetSymbolicLinkStatus

-- | Lifted 'PFiles.createNamedPipe'.
--
-- @since 0.1
createNamedPipe ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  FileMode ->
  Eff es ()
createNamedPipe p = send . CreateNamedPipe p

-- | Lifted 'PFiles.createDevice'.
--
-- @since 0.1
createDevice ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  FileMode ->
  DeviceID ->
  Eff es ()
createDevice p m = send . CreateDevice p m

-- | Lifted 'PFiles.createLink'.
--
-- @since 0.1
createLink ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  PosixPath ->
  Eff es ()
createLink p = send . CreateLink p

-- | Lifted 'PFiles.removeLink'.
--
-- @since 0.1
removeLink ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  Eff es ()
removeLink = send . RemoveLink

-- | Lifted 'PFiles.createSymbolicLink'.
--
-- @since 0.1
createSymbolicLink ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  PosixPath ->
  Eff es ()
createSymbolicLink p = send . CreateSymbolicLink p

-- | Lifted 'PFiles.readSymbolicLink'.
--
-- @since 0.1
readSymbolicLink ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  Eff es PosixPath
readSymbolicLink = send . ReadSymbolicLink

-- | Lifted 'PFiles.rename'.
--
-- @since 0.1
rename ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  PosixPath ->
  Eff es ()
rename p = send . Rename p

-- | Lifted 'PFiles.setOwnerAndGroup'.
--
-- @since 0.1
setOwnerAndGroup ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  UserID ->
  GroupID ->
  Eff es ()
setOwnerAndGroup p uid = send . SetOwnerAndGroup p uid

-- | Lifted 'PFiles.setFdOwnerAndGroup'.
--
-- @since 0.1
setFdOwnerAndGroup ::
  ( HasCallStack,
    PosixFiles :> es
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
    PosixFiles :> es
  ) =>
  PosixPath ->
  UserID ->
  GroupID ->
  Eff es ()
setSymbolicLinkOwnerAndGroup p uid = send . SetSymbolicLinkOwnerAndGroup p uid

-- | Lifted 'PFiles.setFileTimes'.
--
-- @since 0.1
setFileTimes ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  EpochTime ->
  EpochTime ->
  Eff es ()
setFileTimes p t = send . SetFileTimes p t

-- | Lifted 'PFiles.touchFile'.
--
-- @since 0.1
touchFile ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  Eff es ()
touchFile = send . TouchFile

-- | Lifted 'PFiles.setFileSize'.
--
-- @since 0.1
setFileSize ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  FileOffset ->
  Eff es ()
setFileSize p = send . SetFileSize p

-- | Lifted 'PFiles.setFdSize'.
--
-- @since 0.1
setFdSize ::
  ( HasCallStack,
    PosixFiles :> es
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
    PosixFiles :> es
  ) =>
  PosixPath ->
  PathVar ->
  Eff es Limit
getPathVar p = send . GetPathVar p

-- | Lifted 'PFiles.getFdPathVar'.
--
-- @since 0.1
getFdPathVar ::
  ( HasCallStack,
    PosixFiles :> es
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
    PosixFiles :> es
  ) =>
  String ->
  PathType ->
  PosixPath ->
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
    FS.IO.throwPathIOError
      (OsString path)
      location
      InappropriateType
      err

-- | Checks that the path type matches the expectation. Throws
-- 'IOException' if the path does not exist or the type cannot be detected.
--
-- @since 0.1
isPathType ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PathType ->
  PosixPath ->
  Eff es Bool
isPathType expected = fmap (== expected) . getPathType

-- | Returns the type for a given path without following symlinks.
-- Throws 'IOException' if the path does not exist or the type cannot be
-- detected.
--
-- @since 0.1
getPathType ::
  ( HasCallStack,
    PosixFiles :> es
  ) =>
  PosixPath ->
  Eff es PathType
getPathType path =
  getSymbolicLinkStatus path <&> \status ->
    if
      | PFiles.isSymbolicLink status -> PathTypeSymbolicLink
      | PFiles.isDirectory status -> PathTypeDirectory
      | PFiles.isRegularFile status -> PathTypeFile
      | otherwise -> PathTypeOther
