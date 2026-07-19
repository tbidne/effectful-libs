{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.FileSystem.HandleRW.Static
  ( -- * Effect
    HandleRW,
    openBinaryFile,
    withBinaryFile,

    -- ** Handlers
    runHandleRW,

    -- * Reexports
    ByteString,
    Handle,
    HandleMode (HandleModeReadWrite),
    OsPath,
  )
where

import Data.ByteString (ByteString)
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
    seqUnliftIO,
    unsafeEff,
    unsafeEff_,
  )
import Effectful.FileSystem.Handle.Internal
  ( Handle (MkHandle),
    HandleMode (HandleModeReadWrite),
  )
import FileSystem.IO qualified as FS.IO
import FileSystem.OsPath (OsPath)
import System.IO (IOMode (ReadWriteMode))

-- | Static effect for reading a handle.
--
-- @since 0.1
data HandleRW :: Effect

type instance DispatchOf HandleRW = Static WithSideEffects

data instance StaticRep HandleRW = MkHandleRW

-- | Runs 'HandleRW' in 'IO'.
--
-- @since 0.1
runHandleRW ::
  (HasCallStack, IOE :> es) =>
  Eff (HandleRW : es) a ->
  Eff es a
runHandleRW = evalStaticRep MkHandleRW

-- | Opens a file in ReadWrite mode.
--
-- @since 0.1
openBinaryFile ::
  ( HandleRW :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es (Handle HandleModeReadWrite)
openBinaryFile p =
  unsafeEff_ $
    MkHandle
      <$> FS.IO.openBinaryFileIO p ReadWriteMode

-- | Opens a file in ReadWrite mode.
--
-- @since 0.1
withBinaryFile ::
  ( HandleRW :> es,
    HasCallStack
  ) =>
  OsPath ->
  (Handle HandleModeReadWrite -> Eff es a) ->
  Eff es a
withBinaryFile p onHandle =
  unsafeEff $ \env -> seqUnliftIO env $ \unlift ->
    FS.IO.withBinaryFileIO p ReadWriteMode (unlift . onHandle . MkHandle)
