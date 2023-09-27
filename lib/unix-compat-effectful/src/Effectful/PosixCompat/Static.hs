{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for "System.PosixCompat.Files".
--
-- @since 0.1
module Effectful.PosixCompat.Static
  ( -- * Class
    MonadPosixCompat (..),

    -- * Effect
    PosixCompatStatic,

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

-- | @since 0.1
instance (PosixCompatStatic :> es) => MonadPosixCompat (Eff es) where
  setFileMode p = unsafeEff_ . PFiles.setFileMode p
  setFdMode p = unsafeEff_ . PFiles.setFdMode p
  setFileCreationMask = unsafeEff_ . PFiles.setFileCreationMask
  fileAccess p b c = unsafeEff_ . PFiles.fileAccess p b c
  fileExist = unsafeEff_ . PFiles.fileExist
  getFileStatus = unsafeEff_ . PFiles.getFileStatus
  getFdStatus = unsafeEff_ . PFiles.getFdStatus
  getSymbolicLinkStatus = unsafeEff_ . PFiles.getSymbolicLinkStatus
  createNamedPipe p = unsafeEff_ . PFiles.createNamedPipe p
  createDevice p m = unsafeEff_ . PFiles.createDevice p m
  createLink p = unsafeEff_ . PFiles.createLink p
  removeLink = unsafeEff_ . PFiles.removeLink
  createSymbolicLink p = unsafeEff_ . PFiles.createSymbolicLink p
  readSymbolicLink = unsafeEff_ . PFiles.readSymbolicLink
  rename p = unsafeEff_ . PFiles.rename p
  setOwnerAndGroup p uid = unsafeEff_ . PFiles.setOwnerAndGroup p uid
  setFdOwnerAndGroup fd uid = unsafeEff_ . PFiles.setFdOwnerAndGroup fd uid
  setSymbolicLinkOwnerAndGroup p uid = unsafeEff_ . PFiles.setSymbolicLinkOwnerAndGroup p uid
  setFileTimes p t = unsafeEff_ . PFiles.setFileTimes p t
  touchFile = unsafeEff_ . PFiles.touchFile
  setFileSize p = unsafeEff_ . PFiles.setFileSize p
  setFdSize fd = unsafeEff_ . PFiles.setFdSize fd
  getPathVar p = unsafeEff_ . PFiles.getPathVar p
  getFdPathVar fd = unsafeEff_ . PFiles.getFdPathVar fd
