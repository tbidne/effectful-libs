{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides dynamic network effects.
--
-- @since 0.1
module Effectful.HTTP.Client.Dynamic
  ( -- * Effect
    Network (..),

    -- ** Handlers
    runNetwork,

    -- * Manager
    newManager,

    -- ** TLS
    newTlsManager,
    newTlsManagerWith,
    applyDigestAuth,

    -- * Queries
    BodyReader,
    withResponse,
    withResponseHistory,

    -- * Consumers
    brRead,
    brReadSome,
    brConsume,

    -- * Helpers
    readResponseJson,
    readResponseUtf8,
    readResponse,

    -- * Exceptions
    NetworkStatusE (..),
    NetworkReadBodyE (..),
    NetworkDecodeUtf8E (..),
    NetworkDecodeJsonE (..),

    -- * Re-exports
    ManagerSettings,
    Manager,
    Request,
    Response,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Text (Text)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    UnliftStrategy (SeqUnlift),
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( localLiftUnlift,
    localSeqUnlift,
    reinterpret,
    send,
  )
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.HTTP.Client.Internal
  ( NetworkDecodeJsonE (MkNetworkDecodeJsonE),
    NetworkDecodeUtf8E (MkNetworkDecodeUtf8E),
    NetworkReadBodyE (MkNetworkReadBodyE),
    NetworkStatusE (MkNetworkStatusE),
  )
import Effectful.HTTP.Client.Internal qualified as Internal
import Effectful.HTTP.Client.Static qualified as Static
import GHC.Stack.Types (HasCallStack)
import Network.HTTP.Client
  ( HistoriedResponse,
    Manager,
    ManagerSettings,
    Request,
    Response,
  )

-- | @since 0.1
type instance DispatchOf Network = Dynamic

-- | Dynamic effect for network effects.
--
-- @since 0.1
data Network :: Effect where
  -- Setup
  NewManager :: ManagerSettings -> Network m Manager
  -- Tls
  NewTlsManager :: Network m Manager
  NewTlsManagerWith :: ManagerSettings -> Network m Manager
  ApplyDigestAuth ::
    forall m n.
    (MonadThrow n) =>
    ByteString ->
    ByteString ->
    Request ->
    Manager ->
    Network m (n Request)
  -- Queries
  WithResponse ::
    Request ->
    Manager ->
    (Response (BodyReader m) -> m a) ->
    Network m a
  WithResponseHistory ::
    Request ->
    Manager ->
    (HistoriedResponse (BodyReader m) -> m a) ->
    Network m a
  -- Consumers
  BrRead :: BodyReader m -> Network m ByteString
  BrReadSome :: BodyReader m -> Int -> Network m LazyByteString
  BrConsume :: BodyReader m -> Network m [ByteString]

-- | @since 0.1
type BodyReader m = m ByteString

-- | @since 0.1
instance ShowEffect Network where
  showEffectCons = \case
    NewManager _ -> "NewManager"
    NewTlsManager -> "NewTlsManager"
    NewTlsManagerWith _ -> "NewTlsManagerWith"
    ApplyDigestAuth {} -> "ApplyDigestAuth"
    WithResponse {} -> "WithResponse"
    WithResponseHistory {} -> "WithResponseHistory"
    BrRead _ -> "BrRead"
    BrReadSome {} -> "BrReadSome"
    BrConsume _ -> "BrConsume"

-- | Runs 'Network' in 'IO'.
--
-- @since 0.1
runNetwork ::
  forall a es.
  ( HasCallStack,
    IOE :> es
  ) =>
  -- | .
  Eff (Network : es) a ->
  Eff es a
runNetwork = reinterpret Static.runNetwork $ \env -> \case
  NewManager s -> Static.newManager s
  NewTlsManager -> Static.newTlsManager
  NewTlsManagerWith s -> Static.newTlsManagerWith s
  ApplyDigestAuth b1 b2 r m -> Static.applyDigestAuth b1 b2 r m
  WithResponse r m k -> localLiftUnlift env SeqUnlift $ \lift unlift ->
    Static.withResponse r m (unlift . k . fmap lift)
  WithResponseHistory r m k -> localLiftUnlift env SeqUnlift $ \lift unlift ->
    Static.withResponseHistory r m (unlift . k . fmap lift)
  BrRead br -> localSeqUnlift env $ \unlift -> Static.brRead (unlift br)
  BrReadSome br n -> localSeqUnlift env $ \unlift -> Static.brReadSome (unlift br) n
  BrConsume br -> localSeqUnlift env $ \unlift -> Static.brConsume (unlift br)

-- | @since 0.1
newManager ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  ManagerSettings ->
  Eff es Manager
newManager = send . NewManager

-- | @since 0.1
newTlsManager ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  Eff es Manager
newTlsManager = send NewTlsManager

-- | @since 0.1
newTlsManagerWith ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  ManagerSettings ->
  Eff es Manager
newTlsManagerWith = send . NewTlsManagerWith

-- | @since 0.1
applyDigestAuth ::
  forall n es.
  ( HasCallStack,
    MonadThrow n,
    Network :> es
  ) =>
  -- | .
  ByteString ->
  ByteString ->
  Request ->
  Manager ->
  Eff es (n Request)
applyDigestAuth b1 b2 r = send . ApplyDigestAuth b1 b2 r

-- | @since 0.1
withResponse ::
  forall a es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  Request ->
  Manager ->
  (Response (BodyReader (Eff es)) -> Eff es a) ->
  Eff es a
withResponse r m = send . WithResponse r m

-- | @since 0.1
withResponseHistory ::
  forall a es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  Request ->
  Manager ->
  (HistoriedResponse (BodyReader (Eff es)) -> Eff es a) ->
  Eff es a
withResponseHistory r m = send . WithResponseHistory r m

-- | @since 0.1
brRead ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  BodyReader (Eff es) ->
  Eff es ByteString
brRead = send . BrRead

-- | @since 0.1
brReadSome ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  BodyReader (Eff es) ->
  Int ->
  Eff es LazyByteString
brReadSome br = send . BrReadSome br

-- | @since 0.1
brConsume ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  BodyReader (Eff es) ->
  Eff es [ByteString]
brConsume = send . BrConsume

-- | Helper for reading a response, checking for status 200 and exceptions
-- thrown by the consumer.
--
-- @since 0.1
readResponse ::
  forall a es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | String url.
  String ->
  -- | Response.
  Response (BodyReader (Eff es)) ->
  -- | Consumer.
  (BodyReader (Eff es) -> Eff es a) ->
  Eff es a
readResponse = Internal.readResponse

-- | Helper for reading a response, decoding to UTF-8.
--
-- @since 0.1
readResponseUtf8 ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | String url.
  String ->
  -- | Response.
  Response (BodyReader (Eff es)) ->
  Eff es Text
readResponseUtf8 url res =
  readResponse url res brConsume >>= Internal.decodeUtf8

-- | Helper for reading a response, decoding JSON.
readResponseJson ::
  forall a es.
  ( FromJSON a,
    HasCallStack,
    Network :> es
  ) =>
  -- | String url.
  String ->
  -- | Response.
  Response (BodyReader (Eff es)) ->
  Eff es a
readResponseJson url res =
  readResponse url res brConsume >>= Internal.decodeJson
