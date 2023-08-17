-- | Dynamic effects for "Control.Concurrent.QSemN". For static effects, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-Concurrent-QSemN.html.
--
-- @since 0.1
module Effectful.Concurrent.QSemN.Dynamic
  ( -- * Effect
    QSemNDynamic (..),
    newQSemN,
    waitQSemN,
    signalQSemN,

    -- ** Handlers
    runQSemNDynamicIO,
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

-- | Dynamic effects for "Control.Concurrent.QSemN".
--
-- @since 0.1
data QSemNDynamic :: Effect where
  NewQSemN :: Int -> QSemNDynamic m QSemN
  WaitQSemN :: QSemN -> Int -> QSemNDynamic m ()
  SignalQSemN :: QSemN -> Int -> QSemNDynamic m ()

-- | @since 0.1
type instance DispatchOf QSemNDynamic = Dynamic

-- | Runs 'QSemNDynamic' in 'IO'.
--
-- @since 0.1
runQSemNDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (QSemNDynamic : es) a ->
  Eff es a
runQSemNDynamicIO = interpret $ \_ -> \case
  NewQSemN i -> liftIO $ QSemN.newQSemN i
  WaitQSemN q i -> liftIO $ QSemN.waitQSemN q i
  SignalQSemN q i -> liftIO $ QSemN.signalQSemN q i

-- | Lifted 'QSemN.newQSemN'.
--
-- @since 0.1
newQSemN :: (QSemNDynamic :> es) => Int -> Eff es QSemN
newQSemN = send . NewQSemN

-- | Lifted 'QSemN.waitQSemN'.
--
-- @since 0.1
waitQSemN :: (QSemNDynamic :> es) => QSemN -> Int -> Eff es ()
waitQSemN q = send . WaitQSemN q

-- | Lifted 'QSemN.signalQSemN'.
--
-- @since 0.1
signalQSemN :: (QSemNDynamic :> es) => QSemN -> Int -> Eff es ()
signalQSemN q = send . SignalQSemN q
