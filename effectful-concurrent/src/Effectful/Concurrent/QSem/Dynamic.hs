-- | Effect for 'QSem'.
--
-- @since 0.1
module Effectful.Concurrent.QSem.Dynamic
  ( -- * Effect
    QSemEffect (..),
    newQSem,
    waitQSem,
    signalQSem,

    -- ** Handlers
    runQSemIO,
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

-- | Effects for QSem.
--
-- @since 0.1
data QSemEffect :: Effect where
  NewQSem :: Int -> QSemEffect m QSem
  WaitQSem :: QSem -> QSemEffect m ()
  SignalQSem :: QSem -> QSemEffect m ()

-- | @since 0.1
type instance DispatchOf QSemEffect = Dynamic

-- | Runs 'ConcurrentEffect' in 'IO'.
--
-- @since 0.1
runQSemIO ::
  ( IOE :> es
  ) =>
  Eff (QSemEffect : es) a ->
  Eff es a
runQSemIO = interpret $ \_ -> \case
  NewQSem i -> liftIO $ QSem.newQSem i
  WaitQSem q -> liftIO $ QSem.waitQSem q
  SignalQSem q -> liftIO $ QSem.signalQSem q

-- | Lifted 'QSem.newQSem'.
--
-- @since 0.1
newQSem :: (QSemEffect :> es) => Int -> Eff es QSem
newQSem = send . NewQSem

-- | Lifted 'QSem.waitQSem'.
--
-- @since 0.1
waitQSem :: (QSemEffect :> es) => QSem -> Eff es ()
waitQSem = send . WaitQSem

-- | Lifted 'QSem.signalQSem'.
--
-- @since 0.1
signalQSem :: (QSemEffect :> es) => QSem -> Eff es ()
signalQSem = send . SignalQSem
