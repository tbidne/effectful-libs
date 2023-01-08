{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides an effect for handling exceptions with callstacks.
--
-- @since 0.1
module Effectful.CallStack
  ( -- * Effect
    ECallStack,
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
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO)
import Effectful.TH (makeEffect_)
import GHC.Stack (CallStack, HasCallStack, prettyCallStack)

type instance DispatchOf ECallStack = Dynamic

data ECallStack :: Effect where
  ThrowWithCallStack :: (HasCallStack, Exception e) => e -> ECallStack m a
  AddCallStack :: HasCallStack => m a -> ECallStack m a

runECallStackIO :: IOE :> es => Eff (ECallStack : es) a -> Eff es a
runECallStackIO = interpret $ \env -> \case
  ThrowWithCallStack ex -> liftIO $ Ann.throwWithCallStack ex
  AddCallStack m -> localUnliftIO env SeqUnlift $ \runInIO ->
    liftIO $ Ann.checkpointCallStack (runInIO m)

makeEffect_ ''ECallStack

-- | @since 0.1
throwWithCallStack :: (HasCallStack, ECallStack :> es, Exception e) => e -> Eff es a

-- | @since 0.1
addCallStack :: (HasCallStack, ECallStack :> es) => Eff es a -> Eff es a

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
