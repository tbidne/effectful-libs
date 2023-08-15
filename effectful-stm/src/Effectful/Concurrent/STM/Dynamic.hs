-- | Effect for 'STM'.
--
-- @since 0.1
module Effectful.Concurrent.STM.Dynamic
  ( -- * STM

    -- ** Effect
    STMEffect (..),
    atomically,

    -- ** Handlers
    runSTMIO,

    -- * Re-exports
    STM,
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
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

-- | Effect for 'STM'.
--
-- @since 0.1
data STMEffect :: Effect where
  Atomically :: STM a -> STMEffect m a

-- | @since 0.1
type instance DispatchOf STMEffect = Dynamic

-- | Runs 'STMEffect' in 'IO'.
--
-- @since 0.1
runSTMIO ::
  ( IOE :> es
  ) =>
  Eff (STMEffect : es) a ->
  Eff es a
runSTMIO = interpret $ \_ -> \case
  Atomically x -> liftIO $ STM.atomically x

-- | Lifted 'STM.atomically'.
--
-- @since 0.1
atomically :: (STMEffect :> es) => STM a -> Eff es a
atomically = send . Atomically
