{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Dynamic effects for "System.Environment".
--
-- @since 0.1
module Effectful.Environment.Dynamic
  ( -- * Class
    MonadEnvironment (..),

    -- * Effect
    EnvironmentDynamic (..),

    -- ** Handlers
    runEnvironmentDynamicIO,

    -- * Types
    QueryExePath (..),
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
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

-- | Dynamic effects for "System.Environment".
--
-- @since 0.1
data EnvironmentDynamic :: Effect where
  GetArgs :: EnvironmentDynamic m [String]
  GetProgName :: EnvironmentDynamic m String
#if MIN_VERSION_base(4,17,0)
  ExecutablePath :: (EnvironmentDynamic m QueryExePath)
#endif
  GetExecutablePath :: EnvironmentDynamic m FilePath
  GetEnv :: String -> EnvironmentDynamic m String
  LookupEnv :: String -> EnvironmentDynamic m (Maybe String)
  SetEnv :: String -> String -> EnvironmentDynamic m ()
  UnsetEnv :: String -> EnvironmentDynamic m ()
  WithArgs :: [String] -> m a -> EnvironmentDynamic m a
  WithProgName :: String -> m a -> EnvironmentDynamic m a
  GetEnvironment :: EnvironmentDynamic m [(String, String)]

{- ORMOLU_ENABLE -}

-- | @since 0.1
type instance DispatchOf EnvironmentDynamic = Dynamic

{- ORMOLU_DISABLE -}

-- | Runs 'EnvironmentDynamic' in 'IO'.
--
-- @since 0.1
runEnvironmentDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (EnvironmentDynamic : es) a ->
  Eff es a
runEnvironmentDynamicIO = interpret $ \env -> \case
  GetArgs -> liftIO Env.getArgs
  GetProgName -> liftIO Env.getProgName
#if MIN_VERSION_base(4,17,0)
  ExecutablePath ->
    case Env.executablePath of
      Nothing -> pure NoQuery
      Just mpath -> liftIO $ QueryResult <$> mpath
#endif
  GetExecutablePath -> liftIO Env.getExecutablePath
  GetEnv s -> liftIO $ Env.getEnv s
  LookupEnv s -> liftIO $ Env.lookupEnv s
  SetEnv s t -> liftIO $ Env.setEnv s t
  UnsetEnv s -> liftIO $ Env.unsetEnv s
  WithArgs args m -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Env.withArgs args (runInIO m)
  WithProgName name m -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Env.withProgName name (runInIO m)
  GetEnvironment -> liftIO Env.getEnvironment

instance (EnvironmentDynamic :> es) => MonadEnvironment (Eff es) where
  getArgs = send GetArgs
  getProgName = send GetProgName

#if MIN_VERSION_base(4,17,0)
  executablePath = send ExecutablePath
#endif

  getExecutablePath = send GetExecutablePath
  getEnv = send . GetEnv
  lookupEnv = send . LookupEnv
  setEnv s = send . SetEnv s
  unsetEnv = send . UnsetEnv
  withArgs args = send . WithArgs args
  withProgName name = send . WithProgName name
  getEnvironment = send GetEnvironment

{- ORMOLU_ENABLE -}
