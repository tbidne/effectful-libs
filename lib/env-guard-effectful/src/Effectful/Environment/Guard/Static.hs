{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Effectful.Environment.Guard.Static
  ( -- * Class
    MonadEnvGuard (..),

    -- * Effect
    EnvGuardStatic,

    -- ** Handler
    runEnvGuardStaticIO,

    -- * High level combinators
    ExpectEnv (..),
    withGuard,
    withGuard_,
    guardOrElse,
    guardOrElse',

    -- * Low level functions

    -- ** Checking environment variable is set
    guardSet,
    guardSet_,

    -- ** Checking environment variable match
    guardEquals,
    guardEquals_,

    -- ** Checking environment variable predicate
    guardPredicate,
    guardPredicate_,
  )
where

import Control.Monad (void)
import Data.Char qualified as Ch
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    seqUnliftIO,
    unsafeEff,
  )
import System.Environment.Guard
  ( ExpectEnv
      ( ExpectEnvEquals,
        ExpectEnvPredicate,
        ExpectEnvSet
      ),
  )
import System.Environment.Guard qualified as EnvGuard

-- | @since 0.1
class (Monad m) => MonadEnvGuard m where
  -- | @since 0.1
  guardPredicate :: String -> (String -> Bool) -> m a -> m (Maybe a)

-- | @since 0.1
instance MonadEnvGuard IO where
  guardPredicate = EnvGuard.guardPredicate

-- | Static effect for 'EnvGuard'.
--
-- @since 0.1
data EnvGuardStatic :: Effect

type instance DispatchOf EnvGuardStatic = Static WithSideEffects

data instance StaticRep EnvGuardStatic = MkEnvGuardStatic

-- | @since 0.1
instance MonadEnvGuard (Eff es) where
  guardPredicate envStr p action =
    unsafeEff $ \env -> seqUnliftIO env $
      \runInIO -> EnvGuard.guardPredicate envStr p (runInIO action)

-- | Runs an EnvGuardStatic effect.
--
-- @since 0.1
runEnvGuardStaticIO :: (IOE :> es) => Eff (EnvGuardStatic : es) a -> Eff es a
runEnvGuardStaticIO = evalStaticRep MkEnvGuardStatic

-- | @since 0.1
withGuard :: (MonadEnvGuard m) => String -> ExpectEnv -> m a -> m (Maybe a)
withGuard var expect m =
  case expect of
    ExpectEnvSet -> guardSet var m
    ExpectEnvEquals str -> guardEquals var str m
    ExpectEnvPredicate p -> guardPredicate var p m

-- | @since 0.1
withGuard_ :: (MonadEnvGuard m) => String -> ExpectEnv -> m a -> m ()
withGuard_ var expect = void . withGuard var expect

-- | @since 0.1
guardOrElse ::
  (MonadEnvGuard m) =>
  -- | The environment variable.
  String ->
  -- | The expectation.
  ExpectEnv ->
  -- | The action to run if the expectation succeeds.
  m a ->
  -- | The action to run if the expectation fails.
  m e ->
  -- | The result.
  m (Either e a)
guardOrElse var expect m1 m2 =
  withGuard var expect m1
    >>= \case
      Just x -> pure $ Right x
      Nothing -> Left <$> m2

-- | @since 0.1
guardOrElse' ::
  (MonadEnvGuard m) =>
  -- | The environment variable.
  String ->
  -- | The expectation.
  ExpectEnv ->
  -- | The action to run if the expectation succeeds.
  m a ->
  -- | The action to run if the expectation fails.
  m a ->
  -- | The result.
  m a
guardOrElse' var expect m = fmap (either id id) . guardOrElse var expect m

-- | @since 0.1
guardSet :: (MonadEnvGuard m) => String -> m a -> m (Maybe a)
guardSet var = guardPredicate var (const True)

-- | @since 0.1
guardSet_ :: (MonadEnvGuard m) => String -> m a -> m ()
guardSet_ var = void . guardSet var

-- | @since 0.1
guardEquals :: (MonadEnvGuard m) => String -> String -> m a -> m (Maybe a)
guardEquals var expected = guardPredicate var (eqCaseInsensitive expected)

-- | @since 0.1
guardEquals_ :: (MonadEnvGuard m) => String -> String -> m a -> m ()
guardEquals_ var expected = void . guardEquals var expected

-- | @since 0.1
guardPredicate_ :: (MonadEnvGuard m) => String -> (String -> Bool) -> m a -> m ()
guardPredicate_ var p = void . guardPredicate var p

eqCaseInsensitive :: String -> String -> Bool
eqCaseInsensitive a b = fmap Ch.toLower a == fmap Ch.toLower b
