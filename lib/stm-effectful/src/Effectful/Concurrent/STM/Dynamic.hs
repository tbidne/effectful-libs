-- | Dynamic effect for "Control.Concurrent.STM". For the static version, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-Concurrent-STM.html.
--
-- @since 0.1
module Effectful.Concurrent.STM.Dynamic
  ( -- * STM

    -- ** Effect
    STMDynamic (..),
    atomically,

    -- ** Handlers
    runSTMDynamicIO,

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

-- | Dynamic effect for "Control.Concurrent.STM".
--
-- @since 0.1
data STMDynamic :: Effect where
  Atomically :: STM a -> STMDynamic m a

-- | @since 0.1
type instance DispatchOf STMDynamic = Dynamic

-- | Runs 'STMDynamic' in 'IO'.
--
-- @since 0.1
runSTMDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (STMDynamic : es) a ->
  Eff es a
runSTMDynamicIO = interpret $ \_ -> \case
  Atomically x -> liftIO $ STM.atomically x

-- | Lifted 'STM.atomically'.
--
-- @since 0.1
atomically :: (STMDynamic :> es) => STM a -> Eff es a
atomically = send . Atomically
