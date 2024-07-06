{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Environment.Guard.Static
  ( -- * Effect
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

-- | Static effect for 'EnvGuard'.
--
-- @since 0.1
data EnvGuardStatic :: Effect

type instance DispatchOf EnvGuardStatic = Static WithSideEffects

data instance StaticRep EnvGuardStatic = MkEnvGuardStatic

-- | @since 0.1
guardPredicate ::
  (EnvGuardStatic :> es) =>
  String ->
  (String -> Bool) ->
  Eff es a ->
  Eff es (Maybe a)
guardPredicate envStr p action =
  unsafeEff $ \env -> seqUnliftIO env $
    \runInIO -> EnvGuard.guardPredicate envStr p (runInIO action)

-- | Runs an EnvGuardStatic effect.
--
-- @since 0.1
runEnvGuardStaticIO :: (IOE :> es) => Eff (EnvGuardStatic : es) a -> Eff es a
runEnvGuardStaticIO = evalStaticRep MkEnvGuardStatic

-- | @since 0.1
withGuard ::
  (EnvGuardStatic :> es) =>
  String ->
  ExpectEnv ->
  Eff es a ->
  Eff es (Maybe a)
withGuard var expect m =
  case expect of
    ExpectEnvSet -> guardSet var m
    ExpectEnvEquals str -> guardEquals var str m
    ExpectEnvPredicate p -> guardPredicate var p m

-- | @since 0.1
withGuard_ ::
  (EnvGuardStatic :> es) =>
  String ->
  ExpectEnv ->
  Eff es a ->
  Eff es ()
withGuard_ var expect = void . withGuard var expect

-- | @since 0.1
guardOrElse ::
  (EnvGuardStatic :> es) =>
  -- | The environment variable.
  String ->
  -- | The expectation.
  ExpectEnv ->
  -- | The action to run if the expectation succeeds.
  Eff es a ->
  -- | The action to run if the expectation fails.
  Eff es e ->
  -- | The result.
  Eff es (Either e a)
guardOrElse var expect m1 m2 =
  withGuard var expect m1
    >>= \case
      Just x -> pure $ Right x
      Nothing -> Left <$> m2

-- | @since 0.1
guardOrElse' ::
  (EnvGuardStatic :> es) =>
  -- | The environment variable.
  String ->
  -- | The expectation.
  ExpectEnv ->
  -- | The action to run if the expectation succeeds.
  Eff es a ->
  -- | The action to run if the expectation fails.
  Eff es a ->
  -- | The result.
  Eff es a
guardOrElse' var expect m = fmap (either id id) . guardOrElse var expect m

-- | @since 0.1
guardSet ::
  (EnvGuardStatic :> es) =>
  String ->
  Eff es a ->
  Eff es (Maybe a)
guardSet var = guardPredicate var (const True)

-- | @since 0.1
guardSet_ ::
  (EnvGuardStatic :> es) =>
  String ->
  Eff es a ->
  Eff es ()
guardSet_ var = void . guardSet var

-- | @since 0.1
guardEquals ::
  (EnvGuardStatic :> es) =>
  String ->
  String ->
  Eff es a ->
  Eff es (Maybe a)
guardEquals var expected = guardPredicate var (eqCaseInsensitive expected)

-- | @since 0.1
guardEquals_ ::
  (EnvGuardStatic :> es) =>
  String ->
  String ->
  Eff es a ->
  Eff es ()
guardEquals_ var expected = void . guardEquals var expected

-- | @since 0.1
guardPredicate_ ::
  (EnvGuardStatic :> es) =>
  String ->
  (String -> Bool) ->
  Eff es a ->
  Eff es ()
guardPredicate_ var p = void . guardPredicate var p

eqCaseInsensitive :: String -> String -> Bool
eqCaseInsensitive a b = fmap Ch.toLower a == fmap Ch.toLower b
