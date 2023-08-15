-- | Effect for 'QSemN'.
--
-- @since 0.1
module Effectful.Concurrent.QSemN.Dynamic
  ( -- * Effect
    QSemNEffect (..),
    newQSemN,
    waitQSemN,
    signalQSemN,

    -- ** Handlers
    runQSemNIO,
  )
where

import Control.Concurrent.QSemN (QSemN)
import Control.Concurrent.QSemN qualified as QSemN
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

-- | Effects for QSemN.
--
-- @since 0.1
data QSemNEffect :: Effect where
  NewQSemN :: Int -> QSemNEffect m QSemN
  WaitQSemN :: QSemN -> Int -> QSemNEffect m ()
  SignalQSemN :: QSemN -> Int -> QSemNEffect m ()

-- | @since 0.1
type instance DispatchOf QSemNEffect = Dynamic

-- | Runs 'ConcurrentEffect' in 'IO'.
--
-- @since 0.1
runQSemNIO ::
  ( IOE :> es
  ) =>
  Eff (QSemNEffect : es) a ->
  Eff es a
runQSemNIO = interpret $ \_ -> \case
  NewQSemN i -> liftIO $ QSemN.newQSemN i
  WaitQSemN q i -> liftIO $ QSemN.waitQSemN q i
  SignalQSemN q i -> liftIO $ QSemN.signalQSemN q i

-- | Lifted 'QSemN.newQSemN'.
--
-- @since 0.1
newQSemN :: (QSemNEffect :> es) => Int -> Eff es QSemN
newQSemN = send . NewQSemN

-- | Lifted 'QSemN.waitQSemN'.
--
-- @since 0.1
waitQSemN :: (QSemNEffect :> es) => QSemN -> Int -> Eff es ()
waitQSemN q = send . WaitQSemN q

-- | Lifted 'QSemN.signalQSemN'.
--
-- @since 0.1
signalQSemN :: (QSemNEffect :> es) => QSemN -> Int -> Eff es ()
signalQSemN q = send . SignalQSemN q
