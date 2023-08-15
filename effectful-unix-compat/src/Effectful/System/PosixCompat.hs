{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Provides the 'PosixCompat' effect.
--
-- @since 0.1
module Effectful.System.PosixCompat
  ( -- * Effect
    PosixCompatEffect (..),
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
    runPosixCompatIO,
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

-- | Effect for posix-compat.
--
-- @since 0.1
data PosixCompatEffect :: Effect where
  SetFileMode :: FilePath -> FileMode -> PosixCompatEffect m ()
  SetFdMode :: Fd -> FileMode -> PosixCompatEffect m ()
  SetFileCreationMask :: FileMode -> PosixCompatEffect m FileMode
  FileAccess :: FilePath -> Bool -> Bool -> Bool -> PosixCompatEffect m Bool
  FileExist :: FilePath -> PosixCompatEffect m Bool
  GetFileStatus :: FilePath -> PosixCompatEffect m FileStatus
  GetFdStatus :: Fd -> PosixCompatEffect m FileStatus
  GetSymbolicLinkStatus :: FilePath -> PosixCompatEffect m FileStatus
  CreateNamedPipe :: FilePath -> FileMode -> PosixCompatEffect m ()
  CreateDevice :: FilePath -> FileMode -> DeviceID -> PosixCompatEffect m ()
  CreateLink :: FilePath -> FilePath -> PosixCompatEffect m ()
  RemoveLink :: FilePath -> PosixCompatEffect m ()
  CreateSymbolicLink :: FilePath -> FilePath -> PosixCompatEffect m ()
  ReadSymbolicLink :: FilePath -> PosixCompatEffect m FilePath
  Rename :: FilePath -> FilePath -> PosixCompatEffect m ()
  SetOwnerAndGroup :: FilePath -> UserID -> GroupID -> PosixCompatEffect m ()
  SetFdOwnerAndGroup :: Fd -> UserID -> GroupID -> PosixCompatEffect m ()
  SetSymbolicLinkOwnerAndGroup :: FilePath -> UserID -> GroupID -> PosixCompatEffect m ()
  SetFileTimes :: FilePath -> EpochTime -> EpochTime -> PosixCompatEffect m ()
  TouchFile :: FilePath -> PosixCompatEffect m ()
  SetFileSize :: FilePath -> FileOffset -> PosixCompatEffect m ()
  SetFdSize :: Fd -> FileOffset -> PosixCompatEffect m ()
  GetPathVar :: FilePath -> PathVar -> PosixCompatEffect m Limit
  GetFdPathVar :: Fd -> PathVar -> PosixCompatEffect m Limit

-- | @since 0.1
type instance DispatchOf PosixCompatEffect = Dynamic

-- | Runs 'PosixCompatEffect' in 'IO'.
--
-- @since 0.1
runPosixCompatIO ::
  ( IOE :> es
  ) =>
  Eff (PosixCompatEffect : es) a ->
  Eff es a
runPosixCompatIO = interpret $ \_ -> \case
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
  SetFdOwnerAndGroup fd uid gid -> liftIO $ PFiles.setFdOwnerAndGroup fd uid gid
  SetSymbolicLinkOwnerAndGroup p uid gid -> liftIO $ PFiles.setSymbolicLinkOwnerAndGroup p uid gid
  SetFileTimes p t1 t2 -> liftIO $ PFiles.setFileTimes p t1 t2
  TouchFile p -> liftIO $ PFiles.touchFile p
  SetFileSize p oset -> liftIO $ PFiles.setFileSize p oset
  SetFdSize fd oset -> liftIO $ PFiles.setFdSize fd oset
  GetPathVar p m -> liftIO $ PFiles.getPathVar p m
  GetFdPathVar fd m -> liftIO $ PFiles.getFdPathVar fd m

-- | @since 0.1
setFileMode :: (PosixCompatEffect :> es) => FilePath -> FileMode -> Eff es ()
setFileMode p = send . SetFileMode p

-- | @since 0.1
setFdMode :: (PosixCompatEffect :> es) => Fd -> FileMode -> Eff es ()
setFdMode p = send . SetFdMode p

-- | @since 0.1
setFileCreationMask :: (PosixCompatEffect :> es) => FileMode -> Eff es FileMode
setFileCreationMask = send . SetFileCreationMask

-- | @since 0.1
fileAccess :: (PosixCompatEffect :> es) => FilePath -> Bool -> Bool -> Bool -> Eff es Bool
fileAccess p b c = send . FileAccess p b c

-- | @since 0.1
fileExist :: (PosixCompatEffect :> es) => FilePath -> Eff es Bool
fileExist = send . FileExist

-- | @since 0.1
getFileStatus :: (PosixCompatEffect :> es) => FilePath -> Eff es FileStatus
getFileStatus = send . GetFileStatus

-- | @since 0.1
getFdStatus :: (PosixCompatEffect :> es) => Fd -> Eff es FileStatus
getFdStatus = send . GetFdStatus

-- | @since 0.1
getSymbolicLinkStatus :: (PosixCompatEffect :> es) => FilePath -> Eff es FileStatus
getSymbolicLinkStatus = send . GetSymbolicLinkStatus

-- | @since 0.1
createNamedPipe :: (PosixCompatEffect :> es) => FilePath -> FileMode -> Eff es ()
createNamedPipe p = send . CreateNamedPipe p

-- | @since 0.1
createDevice :: (PosixCompatEffect :> es) => FilePath -> FileMode -> DeviceID -> Eff es ()
createDevice p m = send . CreateDevice p m

-- | @since 0.1
createLink :: (PosixCompatEffect :> es) => FilePath -> FilePath -> Eff es ()
createLink p = send . CreateLink p

-- | @since 0.1
removeLink :: (PosixCompatEffect :> es) => FilePath -> Eff es ()
removeLink = send . RemoveLink

-- | @since 0.1
createSymbolicLink :: (PosixCompatEffect :> es) => FilePath -> FilePath -> Eff es ()
createSymbolicLink p = send . CreateSymbolicLink p

-- | @since 0.1
readSymbolicLink :: (PosixCompatEffect :> es) => FilePath -> Eff es FilePath
readSymbolicLink = send . ReadSymbolicLink

-- | @since 0.1
rename :: (PosixCompatEffect :> es) => FilePath -> FilePath -> Eff es ()
rename p = send . Rename p

-- | @since 0.1
setOwnerAndGroup :: (PosixCompatEffect :> es) => FilePath -> UserID -> GroupID -> Eff es ()
setOwnerAndGroup p uid = send . SetOwnerAndGroup p uid

-- | @since 0.1
setFdOwnerAndGroup :: (PosixCompatEffect :> es) => Fd -> UserID -> GroupID -> Eff es ()
setFdOwnerAndGroup fd uid = send . SetFdOwnerAndGroup fd uid

-- | @since 0.1
setSymbolicLinkOwnerAndGroup :: (PosixCompatEffect :> es) => FilePath -> UserID -> GroupID -> Eff es ()
setSymbolicLinkOwnerAndGroup p uid = send . SetSymbolicLinkOwnerAndGroup p uid

-- | @since 0.1
setFileTimes :: (PosixCompatEffect :> es) => FilePath -> EpochTime -> EpochTime -> Eff es ()
setFileTimes p t = send . SetFileTimes p t

-- | @since 0.1
touchFile :: (PosixCompatEffect :> es) => FilePath -> Eff es ()
touchFile = send . TouchFile

-- | @since 0.1
setFileSize :: (PosixCompatEffect :> es) => FilePath -> FileOffset -> Eff es ()
setFileSize p = send . SetFileSize p

-- | @since 0.1
setFdSize :: (PosixCompatEffect :> es) => Fd -> FileOffset -> Eff es ()
setFdSize fd = send . SetFdSize fd

-- | @since 0.1
getPathVar :: (PosixCompatEffect :> es) => FilePath -> PathVar -> Eff es Limit
getPathVar p = send . GetPathVar p

-- | @since 0.1
getFdPathVar :: (PosixCompatEffect :> es) => Fd -> PathVar -> Eff es Limit
getFdPathVar fd = send . GetFdPathVar fd
