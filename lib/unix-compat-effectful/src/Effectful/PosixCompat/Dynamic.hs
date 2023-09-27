{-# LANGUAGE UndecidableInstances #-}

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
  )
where

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

-- | Class for unix-compat effects.
--
-- @since 0.1
class (Monad m) => MonadPosixCompat m where
  -- System.PosixCompat.Files

  -- | @since 0.1
  setFileMode :: FilePath -> FileMode -> m ()

  -- | @since 0.1
  setFdMode :: Fd -> FileMode -> m ()

  -- | @since 0.1
  setFileCreationMask :: FileMode -> m FileMode

  -- | @since 0.1
  fileAccess :: FilePath -> Bool -> Bool -> Bool -> m Bool

  -- | @since 0.1
  fileExist :: FilePath -> m Bool

  -- | @since 0.1
  getFileStatus :: FilePath -> m FileStatus

  -- | @since 0.1
  getFdStatus :: Fd -> m FileStatus

  -- | @since 0.1
  getSymbolicLinkStatus :: FilePath -> m FileStatus

  -- | @since 0.1
  createNamedPipe :: FilePath -> FileMode -> m ()

  -- | @since 0.1
  createDevice :: FilePath -> FileMode -> DeviceID -> m ()

  -- | @since 0.1
  createLink :: FilePath -> FilePath -> m ()

  -- | @since 0.1
  removeLink :: FilePath -> m ()

  -- | @since 0.1
  createSymbolicLink :: FilePath -> FilePath -> m ()

  -- | @since 0.1
  readSymbolicLink :: FilePath -> m FilePath

  -- | @since 0.1
  rename :: FilePath -> FilePath -> m ()

  -- | @since 0.1
  setOwnerAndGroup :: FilePath -> UserID -> GroupID -> m ()

  -- | @since 0.1
  setFdOwnerAndGroup :: Fd -> UserID -> GroupID -> m ()

  -- | @since 0.1
  setSymbolicLinkOwnerAndGroup :: FilePath -> UserID -> GroupID -> m ()

  -- | @since 0.1
  setFileTimes :: FilePath -> EpochTime -> EpochTime -> m ()

  -- | @since 0.1
  touchFile :: FilePath -> m ()

  -- | @since 0.1
  setFileSize :: FilePath -> FileOffset -> m ()

  -- | @since 0.1
  setFdSize :: Fd -> FileOffset -> m ()

  -- | @since 0.1
  getPathVar :: FilePath -> PathVar -> m Limit

  -- | @since 0.1
  getFdPathVar :: Fd -> PathVar -> m Limit

-- | @since 0.1
instance MonadPosixCompat IO where
  setFileMode = PFiles.setFileMode
  setFdMode = PFiles.setFdMode
  setFileCreationMask = PFiles.setFileCreationMask
  fileAccess = PFiles.fileAccess
  fileExist = PFiles.fileExist
  getFileStatus = PFiles.getFileStatus
  getFdStatus = PFiles.getFdStatus
  getSymbolicLinkStatus = PFiles.getSymbolicLinkStatus
  createNamedPipe = PFiles.createNamedPipe
  createDevice = PFiles.createDevice
  createLink = PFiles.createLink
  removeLink = PFiles.removeLink
  createSymbolicLink = PFiles.createSymbolicLink
  readSymbolicLink = PFiles.readSymbolicLink
  rename = PFiles.rename
  setOwnerAndGroup = PFiles.setOwnerAndGroup
  setFdOwnerAndGroup = PFiles.setFdOwnerAndGroup
  setSymbolicLinkOwnerAndGroup = PFiles.setSymbolicLinkOwnerAndGroup
  setFileTimes = PFiles.setFileTimes
  touchFile = PFiles.touchFile
  setFileSize = PFiles.setFileSize
  setFdSize = PFiles.setFdSize
  getPathVar = PFiles.getPathVar
  getFdPathVar = PFiles.getFdPathVar

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

-- | @since 0.1
instance (PosixCompatDynamic :> es) => MonadPosixCompat (Eff es) where
  setFileMode p = send . SetFileMode p
  setFdMode p = send . SetFdMode p
  setFileCreationMask = send . SetFileCreationMask
  fileAccess p b c = send . FileAccess p b c
  fileExist = send . FileExist
  getFileStatus = send . GetFileStatus
  getFdStatus = send . GetFdStatus
  getSymbolicLinkStatus = send . GetSymbolicLinkStatus
  createNamedPipe p = send . CreateNamedPipe p
  createDevice p m = send . CreateDevice p m
  createLink p = send . CreateLink p
  removeLink = send . RemoveLink
  createSymbolicLink p = send . CreateSymbolicLink p
  readSymbolicLink = send . ReadSymbolicLink
  rename p = send . Rename p
  setOwnerAndGroup p uid = send . SetOwnerAndGroup p uid
  setFdOwnerAndGroup fd uid = send . SetFdOwnerAndGroup fd uid
  setSymbolicLinkOwnerAndGroup p uid = send . SetSymbolicLinkOwnerAndGroup p uid
  setFileTimes p t = send . SetFileTimes p t
  touchFile = send . TouchFile
  setFileSize p = send . SetFileSize p
  setFdSize fd = send . SetFdSize fd
  getPathVar p = send . GetPathVar p
  getFdPathVar fd = send . GetFdPathVar fd
