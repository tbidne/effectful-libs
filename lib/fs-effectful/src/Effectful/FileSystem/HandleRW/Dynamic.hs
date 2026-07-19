module Effectful.FileSystem.HandleRW.Dynamic
  ( -- * Effect
    HandleRW (..),
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
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( HasCallStack,
    localSeqUnlift,
    reinterpret,
    send,
  )
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.FileSystem.Handle.Internal
  ( Handle,
    HandleMode (HandleModeReadWrite),
  )
import Effectful.FileSystem.HandleRW.Static qualified as Static
import FileSystem.OsPath (OsPath)

-- | @since 0.1
type instance DispatchOf HandleRW = Dynamic

-- | Dynamic effect for writing to a handle.
--
-- @since 0.1
data HandleRW :: Effect where
  OpenBinaryFile :: OsPath -> HandleRW m (Handle HandleModeReadWrite)
  WithBinaryFile :: OsPath -> (Handle HandleModeReadWrite -> m a) -> HandleRW m a

-- | @since 0.1
instance ShowEffect HandleRW where
  showEffectCons = \case
    OpenBinaryFile _ -> "OpenBinaryFile"
    WithBinaryFile {} -> "WithBinaryFile"

-- | Runs 'HandleRW' in 'IO'.
--
-- @since 0.1
runHandleRW ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (HandleRW : es) a ->
  Eff es a
runHandleRW = reinterpret Static.runHandleRW $ \env -> \case
  OpenBinaryFile p -> Static.openBinaryFile p
  WithBinaryFile p f -> localSeqUnlift env $ \unlift ->
    Static.withBinaryFile p (unlift . f)

-- | Lifted 'IO.openBinaryFile'.
--
-- @since 0.1
openBinaryFile ::
  ( HandleRW :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es (Handle HandleModeReadWrite)
openBinaryFile = send . OpenBinaryFile

-- | Lifted 'IO.openBinaryFile'.
--
-- @since 0.1
withBinaryFile ::
  ( HandleRW :> es,
    HasCallStack
  ) =>
  OsPath ->
  (Handle HandleModeReadWrite -> Eff es a) ->
  Eff es a
withBinaryFile p = send . WithBinaryFile p
