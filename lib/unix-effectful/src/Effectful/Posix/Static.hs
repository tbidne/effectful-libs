{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for "System.Posix.Files".
--
-- @since 0.1
module Effectful.Posix.Static
  ( -- * Effect
    PosixStatic,
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
    runPosixStaticIO,

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
  ( SideEffects (WithSideEffects),
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
data PosixStatic :: Effect

type instance DispatchOf PosixStatic = Static WithSideEffects

data instance StaticRep PosixStatic = MkPosixStatic

-- | Runs an OptparseStatic effect.
--
-- @since 0.1
runPosixStaticIO ::
  (IOE :> es) =>
  Eff (PosixStatic : es) a ->
  Eff es a
runPosixStaticIO = evalStaticRep MkPosixStatic

-- | Lifted 'PFiles.setFileMode'.
--
-- @since 0.1
setFileMode :: (PosixStatic :> es) => PosixPath -> FileMode -> Eff es ()
setFileMode p = unsafeEff_ . PFiles.setFileMode p

-- | Lifted 'PFiles.setFdMode'.
--
-- @since 0.1
setFdMode :: (PosixStatic :> es) => Fd -> FileMode -> Eff es ()
setFdMode p = unsafeEff_ . PFiles.setFdMode p

-- | Lifted 'PFiles.setFileCreationMask'.
--
-- @since 0.1
setFileCreationMask :: (PosixStatic :> es) => FileMode -> Eff es FileMode
setFileCreationMask = unsafeEff_ . PFiles.setFileCreationMask

-- | Lifted 'PFiles.fileAccess'.
--
-- @since 0.1
fileAccess ::
  (PosixStatic :> es) =>
  PosixPath ->
  Bool ->
  Bool ->
  Bool ->
  Eff es Bool
fileAccess p b c = unsafeEff_ . PFiles.fileAccess p b c

-- | Lifted 'PFiles.fileExist'.
--
-- @since 0.1
fileExist :: (PosixStatic :> es) => PosixPath -> Eff es Bool
fileExist = unsafeEff_ . PFiles.fileExist

-- | Lifted 'PFiles.getFileStatus'.
--
-- @since 0.1
getFileStatus :: (PosixStatic :> es) => PosixPath -> Eff es FileStatus
getFileStatus = unsafeEff_ . PFiles.getFileStatus

-- | Lifted 'PFiles.getFdStatus'.
--
-- @since 0.1
getFdStatus :: (PosixStatic :> es) => Fd -> Eff es FileStatus
getFdStatus = unsafeEff_ . PFiles.getFdStatus

-- | Lifted 'PFiles.getSymbolicLinkStatus'.
--
-- @since 0.1
getSymbolicLinkStatus ::
  (PosixStatic :> es) =>
  PosixPath ->
  Eff es FileStatus
getSymbolicLinkStatus = unsafeEff_ . PFiles.getSymbolicLinkStatus

-- | Lifted 'PFiles.createNamedPipe'.
--
-- @since 0.1
createNamedPipe ::
  (PosixStatic :> es) =>
  PosixPath ->
  FileMode ->
  Eff es ()
createNamedPipe p = unsafeEff_ . PFiles.createNamedPipe p

-- | Lifted 'PFiles.createDevice'.
--
-- @since 0.1
createDevice ::
  (PosixStatic :> es) =>
  PosixPath ->
  FileMode ->
  DeviceID ->
  Eff es ()
createDevice p m = unsafeEff_ . PFiles.createDevice p m

-- | Lifted 'PFiles.createLink'.
--
-- @since 0.1
createLink :: (PosixStatic :> es) => PosixPath -> PosixPath -> Eff es ()
createLink p = unsafeEff_ . PFiles.createLink p

-- | Lifted 'PFiles.removeLink'.
--
-- @since 0.1
removeLink :: (PosixStatic :> es) => PosixPath -> Eff es ()
removeLink = unsafeEff_ . PFiles.removeLink

-- | Lifted 'PFiles.createSymbolicLink'.
--
-- @since 0.1
createSymbolicLink ::
  (PosixStatic :> es) =>
  PosixPath ->
  PosixPath ->
  Eff es ()
createSymbolicLink p = unsafeEff_ . PFiles.createSymbolicLink p

-- | Lifted 'PFiles.readSymbolicLink'.
--
-- @since 0.1
readSymbolicLink :: (PosixStatic :> es) => PosixPath -> Eff es PosixPath
readSymbolicLink = unsafeEff_ . PFiles.readSymbolicLink

-- | Lifted 'PFiles.rename'.
--
-- @since 0.1
rename :: (PosixStatic :> es) => PosixPath -> PosixPath -> Eff es ()
rename p = unsafeEff_ . PFiles.rename p

-- | Lifted 'PFiles.setOwnerAndGroup'.
--
-- @since 0.1
setOwnerAndGroup ::
  (PosixStatic :> es) =>
  PosixPath ->
  UserID ->
  GroupID ->
  Eff es ()
setOwnerAndGroup p uid = unsafeEff_ . PFiles.setOwnerAndGroup p uid

-- | Lifted 'PFiles.setFdOwnerAndGroup'.
--
-- @since 0.1
setFdOwnerAndGroup ::
  (PosixStatic :> es) =>
  Fd ->
  UserID ->
  GroupID ->
  Eff es ()
setFdOwnerAndGroup fd uid = unsafeEff_ . PFiles.setFdOwnerAndGroup fd uid

-- | Lifted 'PFiles.setSymbolicLinkOwnerAndGroup'.
--
-- @since 0.1
setSymbolicLinkOwnerAndGroup ::
  (PosixStatic :> es) =>
  PosixPath ->
  UserID ->
  GroupID ->
  Eff es ()
setSymbolicLinkOwnerAndGroup p uid = unsafeEff_ . PFiles.setSymbolicLinkOwnerAndGroup p uid

-- | Lifted 'PFiles.setFileTimes'.
--
-- @since 0.1
setFileTimes ::
  (PosixStatic :> es) =>
  PosixPath ->
  EpochTime ->
  EpochTime ->
  Eff es ()
setFileTimes p t = unsafeEff_ . PFiles.setFileTimes p t

-- | Lifted 'PFiles.touchFile'.
--
-- @since 0.1
touchFile :: (PosixStatic :> es) => PosixPath -> Eff es ()
touchFile = unsafeEff_ . PFiles.touchFile

-- | Lifted 'PFiles.setFileSize'.
--
-- @since 0.1
setFileSize :: (PosixStatic :> es) => PosixPath -> FileOffset -> Eff es ()
setFileSize p = unsafeEff_ . PFiles.setFileSize p

-- | Lifted 'PFiles.setFdSize'.
--
-- @since 0.1
setFdSize :: (PosixStatic :> es) => Fd -> FileOffset -> Eff es ()
setFdSize fd = unsafeEff_ . PFiles.setFdSize fd

-- | Lifted 'PFiles.getPathVar'.
--
-- @since 0.1
getPathVar :: (PosixStatic :> es) => PosixPath -> PathVar -> Eff es Limit
getPathVar p = unsafeEff_ . PFiles.getPathVar p

-- | Lifted 'PFiles.getFdPathVar'.
--
-- @since 0.1
getFdPathVar :: (PosixStatic :> es) => Fd -> PathVar -> Eff es Limit
getFdPathVar fd = unsafeEff_ . PFiles.getFdPathVar fd

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- @since 0.1
throwIfWrongPathType ::
  ( PosixStatic :> es
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
  ( PosixStatic :> es
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
  ( PosixStatic :> es
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
