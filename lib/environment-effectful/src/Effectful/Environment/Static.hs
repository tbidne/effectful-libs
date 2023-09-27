{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Dynamic effects for "System.Environment". The primary interface exists at:
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-Environment.html.
--
-- This module exists to provide a typeclass for functions that we may also
-- want to use in IO to allow for convenient importing e.g.
--
-- @since 0.1
module Effectful.Environment.Static
  ( -- * Class
    MonadEnvironment (..),

    -- * Effect
    Environment,
    EnvironmentStatic,

    -- ** Handlers
    runEnvironment,

    -- * Types
    QueryExePath (..),
  )
where

import Effectful (Eff, type (:>))
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Environment qualified as EffEnv
import Effectful.Environment.Utils (QueryExePath (NoQuery, QueryResult))
import System.Environment qualified as Env

{- ORMOLU_DISABLE -}

-- | Environment effects.
--
-- @since 0.1
class Monad m => MonadEnvironment m where
  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  getArgs :: m [String]

  -- | Lifted 'Env.getProgName'.
  --
  -- @since 0.1
  getProgName :: m String

#if MIN_VERSION_base(4,17,0)
  -- | Lifted 'Env.executablePath'.
  --
  -- @since 0.1
  executablePath :: m QueryExePath
#endif

  -- | Lifted 'Env.getExecutablePath'.
  --
  -- @since 0.1
  getExecutablePath :: m FilePath
  -- | Lifted 'Env.getEnv'.
  --
  -- @since 0.1
  getEnv :: String -> m String
  -- | Lifted 'Env.lookupEnv'.
  --
  -- @since 0.1
  lookupEnv :: String -> m (Maybe String)
  -- | Lifted 'Env.setEnv'.
  --
  -- @since 0.1
  setEnv :: String -> String -> m ()
  -- | Lifted 'Env.unsetEnv'.
  --
  -- @since 0.1
  unsetEnv :: String -> m ()
  -- | Lifted 'Env.withArgs'.
  --
  -- @since 0.1
  withArgs :: [String] -> m a -> m a
  -- | Lifted 'Env.withProgName'.
  --
  -- @since 0.1
  withProgName :: String -> m a -> m a
  -- | Lifted 'Env.getEnvironment'.
  --
  -- @since 0.1
  getEnvironment :: m [(String, String)]

-- | @since 0.1
instance MonadEnvironment IO where
  getArgs = Env.getArgs
  getProgName = Env.getProgName
#if MIN_VERSION_base(4,17,0)
  executablePath = case Env.executablePath of
    Nothing -> pure NoQuery
    Just io -> QueryResult <$> io
#endif
  getExecutablePath = Env.getExecutablePath
  getEnv = Env.getEnv
  lookupEnv = Env.lookupEnv
  setEnv = Env.setEnv
  unsetEnv = Env.unsetEnv
  withArgs = Env.withArgs
  withProgName = Env.withProgName
  getEnvironment = Env.getEnvironment

-- | Alias for 'Environment' effect.
type EnvironmentStatic = Environment

instance (Environment :> es) => MonadEnvironment (Eff es) where
  getArgs = EffEnv.getArgs
  getProgName = EffEnv.getProgName

#if MIN_VERSION_base(4,17,0)
  executablePath = case Env.executablePath of
    Nothing -> pure NoQuery
    Just io -> QueryResult <$> unsafeEff_ io
#endif

  getExecutablePath = EffEnv.getExecutablePath
  getEnv = EffEnv.getEnv
  lookupEnv = EffEnv.lookupEnv
  setEnv = EffEnv.setEnv
  unsetEnv = EffEnv.unsetEnv
  withArgs = EffEnv.withArgs
  withProgName = EffEnv.withProgName
  getEnvironment = EffEnv.getEnvironment

{- ORMOLU_ENABLE -}
