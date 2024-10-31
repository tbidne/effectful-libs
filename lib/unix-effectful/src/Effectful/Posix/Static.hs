{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for "System.Posix.Files".
--
-- @since 0.1
module Effectful.Posix.Static
  ( -- * Effect
    Posix,
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
    runPosix,

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
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( HasCallStack,
    SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    unsafeEff_,
  )
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

-- | Provides a static effect for "System.Posix.Files".
--
-- @since 0.1
data Posix :: Effect

type instance DispatchOf Posix = Static WithSideEffects

data instance StaticRep Posix = MkPosix

-- | Runs a Posix effect.
--
-- @since 0.1
runPosix ::
  (HasCallStack, IOE :> es) =>
  Eff (Posix : es) a ->
  Eff es a
runPosix = evalStaticRep MkPosix

-- | Lifted 'PFiles.setFileMode'.
--
-- @since 0.1
setFileMode ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  FileMode ->
  Eff es ()
setFileMode p = unsafeEff_ . PFiles.setFileMode p

-- | Lifted 'PFiles.setFdMode'.
--
-- @since 0.1
setFdMode ::
  ( HasCallStack,
    Posix :> es
  ) =>
  Fd ->
  FileMode ->
  Eff es ()
setFdMode p = unsafeEff_ . PFiles.setFdMode p

-- | Lifted 'PFiles.setFileCreationMask'.
--
-- @since 0.1
setFileCreationMask ::
  ( HasCallStack,
    Posix :> es
  ) =>
  FileMode ->
  Eff es FileMode
setFileCreationMask = unsafeEff_ . PFiles.setFileCreationMask

-- | Lifted 'PFiles.fileAccess'.
--
-- @since 0.1
fileAccess ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Bool ->
  Bool ->
  Bool ->
  Eff es Bool
fileAccess p b c = unsafeEff_ . PFiles.fileAccess p b c

-- | Lifted 'PFiles.fileExist'.
--
-- @since 0.1
fileExist ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es Bool
fileExist = unsafeEff_ . PFiles.fileExist

-- | Lifted 'PFiles.getFileStatus'.
--
-- @since 0.1
getFileStatus ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es FileStatus
getFileStatus = unsafeEff_ . PFiles.getFileStatus

-- | Lifted 'PFiles.getFdStatus'.
--
-- @since 0.1
getFdStatus ::
  ( HasCallStack,
    Posix :> es
  ) =>
  Fd ->
  Eff es FileStatus
getFdStatus = unsafeEff_ . PFiles.getFdStatus

-- | Lifted 'PFiles.getSymbolicLinkStatus'.
--
-- @since 0.1
getSymbolicLinkStatus ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es FileStatus
getSymbolicLinkStatus = unsafeEff_ . PFiles.getSymbolicLinkStatus

-- | Lifted 'PFiles.createNamedPipe'.
--
-- @since 0.1
createNamedPipe ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  FileMode ->
  Eff es ()
createNamedPipe p = unsafeEff_ . PFiles.createNamedPipe p

-- | Lifted 'PFiles.createDevice'.
--
-- @since 0.1
createDevice ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  FileMode ->
  DeviceID ->
  Eff es ()
createDevice p m = unsafeEff_ . PFiles.createDevice p m

-- | Lifted 'PFiles.createLink'.
--
-- @since 0.1
createLink ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  PosixPath ->
  Eff es ()
createLink p = unsafeEff_ . PFiles.createLink p

-- | Lifted 'PFiles.removeLink'.
--
-- @since 0.1
removeLink ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es ()
removeLink = unsafeEff_ . PFiles.removeLink

-- | Lifted 'PFiles.createSymbolicLink'.
--
-- @since 0.1
createSymbolicLink ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  PosixPath ->
  Eff es ()
createSymbolicLink p = unsafeEff_ . PFiles.createSymbolicLink p

-- | Lifted 'PFiles.readSymbolicLink'.
--
-- @since 0.1
readSymbolicLink ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es PosixPath
readSymbolicLink = unsafeEff_ . PFiles.readSymbolicLink

-- | Lifted 'PFiles.rename'.
--
-- @since 0.1
rename ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  PosixPath ->
  Eff es ()
rename p = unsafeEff_ . PFiles.rename p

-- | Lifted 'PFiles.setOwnerAndGroup'.
--
-- @since 0.1
setOwnerAndGroup ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  UserID ->
  GroupID ->
  Eff es ()
setOwnerAndGroup p uid = unsafeEff_ . PFiles.setOwnerAndGroup p uid

-- | Lifted 'PFiles.setFdOwnerAndGroup'.
--
-- @since 0.1
setFdOwnerAndGroup ::
  ( HasCallStack,
    Posix :> es
  ) =>
  Fd ->
  UserID ->
  GroupID ->
  Eff es ()
setFdOwnerAndGroup fd uid = unsafeEff_ . PFiles.setFdOwnerAndGroup fd uid

-- | Lifted 'PFiles.setSymbolicLinkOwnerAndGroup'.
--
-- @since 0.1
setSymbolicLinkOwnerAndGroup ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  UserID ->
  GroupID ->
  Eff es ()
setSymbolicLinkOwnerAndGroup p uid = unsafeEff_ . PFiles.setSymbolicLinkOwnerAndGroup p uid

-- | Lifted 'PFiles.setFileTimes'.
--
-- @since 0.1
setFileTimes ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  EpochTime ->
  EpochTime ->
  Eff es ()
setFileTimes p t = unsafeEff_ . PFiles.setFileTimes p t

-- | Lifted 'PFiles.touchFile'.
--
-- @since 0.1
touchFile ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  Eff es ()
touchFile = unsafeEff_ . PFiles.touchFile

-- | Lifted 'PFiles.setFileSize'.
--
-- @since 0.1
setFileSize ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  FileOffset ->
  Eff es ()
setFileSize p = unsafeEff_ . PFiles.setFileSize p

-- | Lifted 'PFiles.setFdSize'.
--
-- @since 0.1
setFdSize ::
  ( HasCallStack,
    Posix :> es
  ) =>
  Fd ->
  FileOffset ->
  Eff es ()
setFdSize fd = unsafeEff_ . PFiles.setFdSize fd

-- | Lifted 'PFiles.getPathVar'.
--
-- @since 0.1
getPathVar ::
  ( HasCallStack,
    Posix :> es
  ) =>
  PosixPath ->
  PathVar ->
  Eff es Limit
getPathVar p = unsafeEff_ . PFiles.getPathVar p

-- | Lifted 'PFiles.getFdPathVar'.
--
-- @since 0.1
getFdPathVar ::
  ( HasCallStack,
    Posix :> es
  ) =>
  Fd ->
  PathVar ->
  Eff es Limit
getFdPathVar fd = unsafeEff_ . PFiles.getFdPathVar fd

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- @since 0.1
throwIfWrongPathType ::
  ( HasCallStack,
    Posix :> es
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
    Posix :> es
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
    Posix :> es
  ) =>
  PosixPath ->
  Eff es PathType
getPathType path = do
  getSymbolicLinkStatus path <&> \status ->
    if
      | PFiles.isSymbolicLink status -> PathTypeSymbolicLink
      | PFiles.isDirectory status -> PathTypeDirectory
      | PFiles.isRegularFile status -> PathTypeFile
      | otherwise -> PathTypeOther
