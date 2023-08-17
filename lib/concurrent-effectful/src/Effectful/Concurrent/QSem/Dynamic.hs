-- | Dynamic effects for "Control.Concurrent.QSem". For static effects, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-Concurrent-QSem.html.
--
-- @since 0.1
module Effectful.Concurrent.QSem.Dynamic
  ( -- * Effect
    QSemDynamic (..),
    newQSem,
    waitQSem,
    signalQSem,

    -- ** Handlers
    runQSemDynamicIO,
  )
where

import Control.Concurrent.QSem (QSem)
import Control.Concurrent.QSem qualified as QSem
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

-- | Dynamic effects for "Control.Concurrent.QSem".
--
-- @since 0.1
data QSemDynamic :: Effect where
  NewQSem :: Int -> QSemDynamic m QSem
  WaitQSem :: QSem -> QSemDynamic m ()
  SignalQSem :: QSem -> QSemDynamic m ()

-- | @since 0.1
type instance DispatchOf QSemDynamic = Dynamic

-- | Runs 'QSemDynamic' in 'IO'.
--
-- @since 0.1
runQSemDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (QSemDynamic : es) a ->
  Eff es a
runQSemDynamicIO = interpret $ \_ -> \case
  NewQSem i -> liftIO $ QSem.newQSem i
  WaitQSem q -> liftIO $ QSem.waitQSem q
  SignalQSem q -> liftIO $ QSem.signalQSem q

-- | Lifted 'QSem.newQSem'.
--
-- @since 0.1
newQSem :: (QSemDynamic :> es) => Int -> Eff es QSem
newQSem = send . NewQSem

-- | Lifted 'QSem.waitQSem'.
--
-- @since 0.1
waitQSem :: (QSemDynamic :> es) => QSem -> Eff es ()
waitQSem = send . WaitQSem

-- | Lifted 'QSem.signalQSem'.
--
-- @since 0.1
signalQSem :: (QSemDynamic :> es) => QSem -> Eff es ()
signalQSem = send . SignalQSem
