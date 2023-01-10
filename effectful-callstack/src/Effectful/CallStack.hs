{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides an effect for handling exceptions with callstacks.
--
-- @since 0.1
module Effectful.CallStack
  ( -- * Effect
    CallStackEffect (..),

    -- ** Functions
    throwWithCallStack,
    addCallStack,

    -- ** Handlers
    runCallStackIO,

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
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import GHC.Stack (CallStack, HasCallStack, prettyCallStack)

-- | @since 0.1
type instance DispatchOf CallStackEffect = Dynamic

-- | Effect for adding 'CallStack' to exceptions.
--
-- @since 0.1
data CallStackEffect :: Effect where
  ThrowWithCallStack :: (Exception e, HasCallStack) => e -> CallStackEffect m a
  AddCallStack :: HasCallStack => m a -> CallStackEffect m a

-- | Runs 'CallStackEffect' in 'IO'.
--
-- @since 0.1
runCallStackIO :: IOE :> es => Eff (CallStackEffect : es) a -> Eff es a
runCallStackIO = interpret $ \env -> \case
  ThrowWithCallStack ex -> liftIO $ Ann.throwWithCallStack ex
  AddCallStack m -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Ann.checkpointCallStack (runInIO m)

-- | @since 0.1
throwWithCallStack ::
  ( CallStackEffect :> es,
    Exception e,
    HasCallStack
  ) =>
  e ->
  Eff es a
throwWithCallStack e = send (ThrowWithCallStack e)

-- | @since 0.1
addCallStack :: (CallStackEffect :> es, HasCallStack) => Eff es a -> Eff es a
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
