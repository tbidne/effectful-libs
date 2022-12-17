{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides an effect for handling exceptions with callstacks.
--
-- @since 0.1
module Effectful.CallStack
  ( -- * Effect
    ECallStack,
    getCallStack,
    throwWithCallStack,
    addCallStack,

    -- * Handler
    runECallStack,

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
import Data.Foldable (Foldable (foldMap'))
import Data.Typeable (cast)
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
    unEff,
    unsafeEff,
    unsafeEff_,
  )
import Effectful.Dispatch.Static.Primitive (cloneEnv)
import GHC.Stack (CallStack, callStack, prettyCallStack)

-- | Effect for 'CallStack'.
--
-- @since 0.1
data ECallStack :: Effect

type instance DispatchOf ECallStack = Static WithSideEffects

data instance StaticRep ECallStack = MkECallStack

-- | Run the 'ECallStack' effect.
--
-- @since 0.1
runECallStack :: IOE :> es => Eff (ECallStack : es) a -> Eff es a
runECallStack = evalStaticRep MkECallStack

-- | Retrieves the 'CallStack'.
--
-- @since 0.1
getCallStack ::
  ( ECallStack :> es,
    HasCallStack
  ) =>
  Eff es CallStack
getCallStack = unsafeEff_ (pure callStack)

-- | Throws an exception with 'CallStack'.
--
-- @since 0.1
throwWithCallStack ::
  forall e es a.
  ( ECallStack :> es,
    Exception e,
    HasCallStack
  ) =>
  e ->
  Eff es a
throwWithCallStack = unsafeEff_ . Ann.throwWithCallStack

-- | Adds 'CallStack' to any thrown exceptions.
--
-- @since 0.1
addCallStack ::
  forall es a.
  (ECallStack :> es, HasCallStack) =>
  Eff es a ->
  Eff es a
addCallStack eff = unsafeEff $ \es -> do
  env <- cloneEnv es
  Ann.checkpointCallStack (unEff eff env)

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
