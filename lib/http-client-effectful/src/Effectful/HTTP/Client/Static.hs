{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.HTTP.Client.Static
  ( -- * Effect
    Network,

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
import Effectful.HTTP.Client.Internal
  ( NetworkDecodeJsonE (MkNetworkDecodeJsonE),
    NetworkDecodeUtf8E (MkNetworkDecodeUtf8E),
    NetworkReadBodyE (MkNetworkReadBodyE),
    NetworkStatusE (MkNetworkStatusE),
  )
import Effectful.HTTP.Client.Internal qualified as Internal
import Network.HTTP.Client
  ( HistoriedResponse,
    Manager,
    ManagerSettings,
    Request,
    Response,
  )
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as TLS

-- | Static network effect.
--
-- @since 0.1
data Network :: Effect

-- | @since 0.1
type instance DispatchOf Network = Static WithSideEffects

-- | @since 0.1
data instance StaticRep Network = MkNetwork

-- | @since 0.1
type BodyReader m = m ByteString

-- | Runs 'Network' in 'IOE'.
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
runNetwork = evalStaticRep MkNetwork

-- | @since 0.1
newManager ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  ManagerSettings ->
  Eff es Manager
newManager = unsafeEff_ . HttpClient.newManager

-- | @since 0.1
newTlsManager ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  Eff es Manager
newTlsManager = unsafeEff_ TLS.newTlsManager

-- | @since 0.1
newTlsManagerWith ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  ManagerSettings ->
  Eff es Manager
newTlsManagerWith = unsafeEff_ . TLS.newTlsManagerWith

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
applyDigestAuth b1 b2 r = unsafeEff_ . TLS.applyDigestAuth b1 b2 r

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
withResponse req manager onResponse =
  unsafeEff $ \env -> seqUnliftIO env $
    \unlift ->
      HttpClient.withResponse
        req
        manager
        (unlift . onResponse . fmap unsafeEff_)

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
withResponseHistory req manager onResponse =
  unsafeEff $ \env -> seqUnliftIO env $
    \unlift ->
      HttpClient.withResponseHistory
        req
        manager
        (unlift . onResponse . fmap unsafeEff_)

-- | @since 0.1
brRead ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  BodyReader (Eff es) ->
  Eff es ByteString
brRead br = unsafeEff $ \env -> seqUnliftIO env $ \unlift ->
  HttpClient.brRead (unlift br)

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
brReadSome br n = unsafeEff $ \env -> seqUnliftIO env $ \unlift ->
  HttpClient.brReadSome (unlift br) n

-- | @since 0.1
brConsume ::
  forall es.
  ( HasCallStack,
    Network :> es
  ) =>
  -- | .
  BodyReader (Eff es) ->
  Eff es [ByteString]
brConsume br = unsafeEff $ \env -> seqUnliftIO env $ \unlift ->
  HttpClient.brConsume (unlift br)

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
