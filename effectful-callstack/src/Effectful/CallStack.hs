{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides an effect for handling exceptions with callstacks.
--
-- @since 0.1
module Effectful.CallStack
  ( -- * Effect
    EffectCallStack (..),
    throwWithCallStack,
    addCallStack,

    -- * Handler
    runECallStackIO,

    -- * Utils
    displayCallStack,

    -- * Reexports
    CallStack,
    HasCallStack,
    AnnotatedException (..),
    Annotation (..),
    Ann.throw,
    Ann.try,
    Ann.catch,
  )
where

import Control.Exception.Annotated.UnliftIO
  ( AnnotatedException (..),
    Annotation (..),
    Exception (displayException, fromException, toException),
    SomeException,
  )
import Control.Exception.Annotated.UnliftIO qualified as Ann
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (Foldable (foldMap'))
import Data.Typeable (cast)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    UnliftStrategy (SeqUnlift),
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO, send)
import GHC.Stack (CallStack, HasCallStack, prettyCallStack)

-- | @since 0.1
type instance DispatchOf EffectCallStack = Dynamic

-- | Effect for adding 'CallStack' to exceptions.
--
-- @since 0.1
data EffectCallStack :: Effect where
  ThrowWithCallStack :: (Exception e, HasCallStack) => e -> EffectCallStack m a
  AddCallStack :: HasCallStack => m a -> EffectCallStack m a

-- | Runs 'EffectCallStack' in 'IO'.
--
-- @since 0.1
runECallStackIO :: IOE :> es => Eff (EffectCallStack : es) a -> Eff es a
runECallStackIO = interpret $ \env -> \case
  ThrowWithCallStack ex -> liftIO $ Ann.throwWithCallStack ex
  AddCallStack m -> localUnliftIO env SeqUnlift $ \runInIO ->
    liftIO $ Ann.checkpointCallStack (runInIO m)

-- | @since 0.1
throwWithCallStack ::
  ( EffectCallStack :> es,
    Exception e,
    HasCallStack
  ) =>
  e ->
  Eff es a
throwWithCallStack e = send (ThrowWithCallStack e)

-- | @since 0.1
addCallStack :: (HasCallStack, EffectCallStack :> es) => Eff es a -> Eff es a
addCallStack = send . AddCallStack

-- | Like 'displayException', except it has extra logic that attempts to
-- display any found 'CallStack's in a pretty way.
--
-- @since 0.1
displayCallStack :: forall e. Exception e => e -> String
displayCallStack ex =
  case fromException @(AnnotatedException SomeException) (toException ex) of
    Nothing -> displayException ex
    Just (AnnotatedException anns anEx) ->
      mconcat
        [ displayException anEx,
          foldMap' (\a -> "\n" <> prettyAnn a) anns
        ]
  where
    prettyAnn :: Annotation -> String
    prettyAnn (Annotation x) = case cast x of
      Just cs -> prettyCallStack cs
      Nothing -> show x
