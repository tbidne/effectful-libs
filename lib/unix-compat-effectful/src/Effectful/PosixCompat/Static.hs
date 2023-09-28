{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for "System.PosixCompat.Files".
--
-- @since 0.1
module Effectful.PosixCompat.Static
  ( -- * Effect
    PosixCompatStatic,
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
    runPosixCompatStaticIO,
  )
where

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

-- | Provides a static effect for "System.PosixCompat.Files".
--
-- @since 0.1
data PosixCompatStatic :: Effect

type instance DispatchOf PosixCompatStatic = Static WithSideEffects

data instance StaticRep PosixCompatStatic = MkPosixCompatStatic

-- | Runs an OptparseStatic effect.
--
-- @since 0.1
runPosixCompatStaticIO ::
  (IOE :> es) =>
  Eff (PosixCompatStatic : es) a ->
  Eff es a
runPosixCompatStaticIO = evalStaticRep MkPosixCompatStatic

-- | Lifted 'PFiles.setFileMode'.
--
-- @since 0.1
setFileMode :: (PosixCompatStatic :> es) => FilePath -> FileMode -> Eff es ()
setFileMode p = unsafeEff_ . PFiles.setFileMode p

-- | Lifted 'PFiles.setFdMode'.
--
-- @since 0.1
setFdMode :: (PosixCompatStatic :> es) => Fd -> FileMode -> Eff es ()
setFdMode p = unsafeEff_ . PFiles.setFdMode p

-- | Lifted 'PFiles.setFileCreationMask'.
--
-- @since 0.1
setFileCreationMask :: (PosixCompatStatic :> es) => FileMode -> Eff es FileMode
setFileCreationMask = unsafeEff_ . PFiles.setFileCreationMask

-- | Lifted 'PFiles.fileAccess'.
--
-- @since 0.1
fileAccess ::
  (PosixCompatStatic :> es) =>
  FilePath ->
  Bool ->
  Bool ->
  Bool ->
  Eff es Bool
fileAccess p b c = unsafeEff_ . PFiles.fileAccess p b c

-- | Lifted 'PFiles.fileExist'.
--
-- @since 0.1
fileExist :: (PosixCompatStatic :> es) => FilePath -> Eff es Bool
fileExist = unsafeEff_ . PFiles.fileExist

-- | Lifted 'PFiles.getFileStatus'.
--
-- @since 0.1
getFileStatus :: (PosixCompatStatic :> es) => FilePath -> Eff es FileStatus
getFileStatus = unsafeEff_ . PFiles.getFileStatus

-- | Lifted 'PFiles.getFdStatus'.
--
-- @since 0.1
getFdStatus :: (PosixCompatStatic :> es) => Fd -> Eff es FileStatus
getFdStatus = unsafeEff_ . PFiles.getFdStatus

-- | Lifted 'PFiles.getSymbolicLinkStatus'.
--
-- @since 0.1
getSymbolicLinkStatus ::
  (PosixCompatStatic :> es) =>
  FilePath ->
  Eff es FileStatus
getSymbolicLinkStatus = unsafeEff_ . PFiles.getSymbolicLinkStatus

-- | Lifted 'PFiles.createNamedPipe'.
--
-- @since 0.1
createNamedPipe ::
  (PosixCompatStatic :> es) =>
  FilePath ->
  FileMode ->
  Eff es ()
createNamedPipe p = unsafeEff_ . PFiles.createNamedPipe p

-- | Lifted 'PFiles.createDevice'.
--
-- @since 0.1
createDevice ::
  (PosixCompatStatic :> es) =>
  FilePath ->
  FileMode ->
  DeviceID ->
  Eff es ()
createDevice p m = unsafeEff_ . PFiles.createDevice p m

-- | Lifted 'PFiles.createLink'.
--
-- @since 0.1
createLink :: (PosixCompatStatic :> es) => FilePath -> FilePath -> Eff es ()
createLink p = unsafeEff_ . PFiles.createLink p

-- | Lifted 'PFiles.removeLink'.
--
-- @since 0.1
removeLink :: (PosixCompatStatic :> es) => FilePath -> Eff es ()
removeLink = unsafeEff_ . PFiles.removeLink

-- | Lifted 'PFiles.createSymbolicLink'.
--
-- @since 0.1
createSymbolicLink ::
  (PosixCompatStatic :> es) =>
  FilePath ->
  FilePath ->
  Eff es ()
createSymbolicLink p = unsafeEff_ . PFiles.createSymbolicLink p

-- | Lifted 'PFiles.readSymbolicLink'.
--
-- @since 0.1
readSymbolicLink :: (PosixCompatStatic :> es) => FilePath -> Eff es FilePath
readSymbolicLink = unsafeEff_ . PFiles.readSymbolicLink

-- | Lifted 'PFiles.rename'.
--
-- @since 0.1
rename :: (PosixCompatStatic :> es) => FilePath -> FilePath -> Eff es ()
rename p = unsafeEff_ . PFiles.rename p

-- | Lifted 'PFiles.setOwnerAndGroup'.
--
-- @since 0.1
setOwnerAndGroup ::
  (PosixCompatStatic :> es) =>
  FilePath ->
  UserID ->
  GroupID ->
  Eff es ()
setOwnerAndGroup p uid = unsafeEff_ . PFiles.setOwnerAndGroup p uid

-- | Lifted 'PFiles.setFdOwnerAndGroup'.
--
-- @since 0.1
setFdOwnerAndGroup ::
  (PosixCompatStatic :> es) =>
  Fd ->
  UserID ->
  GroupID ->
  Eff es ()
setFdOwnerAndGroup fd uid = unsafeEff_ . PFiles.setFdOwnerAndGroup fd uid

-- | Lifted 'PFiles.setSymbolicLinkOwnerAndGroup'.
--
-- @since 0.1
setSymbolicLinkOwnerAndGroup ::
  (PosixCompatStatic :> es) =>
  FilePath ->
  UserID ->
  GroupID ->
  Eff es ()
setSymbolicLinkOwnerAndGroup p uid = unsafeEff_ . PFiles.setSymbolicLinkOwnerAndGroup p uid

-- | Lifted 'PFiles.setFileTimes'.
--
-- @since 0.1
setFileTimes ::
  (PosixCompatStatic :> es) =>
  FilePath ->
  EpochTime ->
  EpochTime ->
  Eff es ()
setFileTimes p t = unsafeEff_ . PFiles.setFileTimes p t

-- | Lifted 'PFiles.touchFile'.
--
-- @since 0.1
touchFile :: (PosixCompatStatic :> es) => FilePath -> Eff es ()
touchFile = unsafeEff_ . PFiles.touchFile

-- | Lifted 'PFiles.setFileSize'.
--
-- @since 0.1
setFileSize :: (PosixCompatStatic :> es) => FilePath -> FileOffset -> Eff es ()
setFileSize p = unsafeEff_ . PFiles.setFileSize p

-- | Lifted 'PFiles.setFdSize'.
--
-- @since 0.1
setFdSize :: (PosixCompatStatic :> es) => Fd -> FileOffset -> Eff es ()
setFdSize fd = unsafeEff_ . PFiles.setFdSize fd

-- | Lifted 'PFiles.getPathVar'.
--
-- @since 0.1
getPathVar :: (PosixCompatStatic :> es) => FilePath -> PathVar -> Eff es Limit
getPathVar p = unsafeEff_ . PFiles.getPathVar p

-- | Lifted 'PFiles.getFdPathVar'.
--
-- @since 0.1
getFdPathVar :: (PosixCompatStatic :> es) => Fd -> PathVar -> Eff es Limit
getFdPathVar fd = unsafeEff_ . PFiles.getFdPathVar fd
