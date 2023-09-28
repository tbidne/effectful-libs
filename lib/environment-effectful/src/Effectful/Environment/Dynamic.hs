{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Dynamic effects for "System.Environment". For static effects, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-Environment.html.
--
-- @since 0.1
module Effectful.Environment.Dynamic
  ( -- * Effect
    EnvironmentDynamic (..),
    getArgs,
    getProgName,
#if MIN_VERSION_base(4,17,0)
    executablePath,
#endif
    getExecutablePath,
    getEnv,
    lookupEnv,
    setEnv,
    unsetEnv,
    withArgs,
    withProgName,
    getEnvironment,

    -- ** Handlers
    runEnvironmentDynamicIO,

    -- * Types
    QueryExePath (..),
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Environment qualified as Env
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>)
  )
import Effectful.Dispatch.Dynamic (interpret, send, localSeqUnliftIO)

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
  WithProgName :: String -> m () -> EnvironmentDynamic m ()
  GetEnvironment :: EnvironmentDynamic m [(String, String)]

{- ORMOLU_ENABLE -}

-- | @since 0.1
type instance DispatchOf EnvironmentDynamic = Dynamic

-- | Result of querying for the executable path.
--
-- @since 0.1
data QueryExePath
  = -- | If the system does not provide a reliable way to determine the
    -- current executable.
    --
    -- @since 0.1
    NoQuery
  | -- | The result of querying the executable name.
    --
    -- @since 0.1
    QueryResult (Maybe FilePath)
  deriving stock (Eq, Show)

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

{- ORMOLU_ENABLE -}

-- | Lifted 'Env.getArgs'.
--
-- @since 0.1
getArgs :: (EnvironmentDynamic :> es) => Eff es [String]
getArgs = send GetArgs

-- | Lifted 'Env.getProgName'.
--
-- @since 0.1
getProgName :: (EnvironmentDynamic :> es) => Eff es String
getProgName = send GetProgName

#if MIN_VERSION_base(4,17,0)

-- | Lifted 'Env.executablePath'.
--
-- @since 0.1
executablePath :: (EnvironmentDynamic :> es) => Eff es QueryExePath
executablePath = send ExecutablePath

#endif

-- | Lifted 'Env.getExecutablePath'.
--
-- @since 0.1
getExecutablePath :: (EnvironmentDynamic :> es) => Eff es FilePath
getExecutablePath = send GetExecutablePath

-- | Lifted 'Env.getEnv'.
--
-- @since 0.1
getEnv :: (EnvironmentDynamic :> es) => String -> Eff es String
getEnv = send . GetEnv

-- | Lifted 'Env.lookupEnv'.
--
-- @since 0.1
lookupEnv :: (EnvironmentDynamic :> es) => String -> Eff es (Maybe String)
lookupEnv = send . LookupEnv

-- | Lifted 'Env.setEnv'.
--
-- @since 0.1
setEnv :: (EnvironmentDynamic :> es) => String -> String -> Eff es ()
setEnv s = send . SetEnv s

-- | Lifted 'Env.unsetEnv'.
--
-- @since 0.1
unsetEnv :: (EnvironmentDynamic :> es) => String -> Eff es ()
unsetEnv = send . UnsetEnv

-- | Lifted 'Env.withArgs'.
--
-- @since 0.1
withArgs :: (EnvironmentDynamic :> es) => [String] -> (Eff es) a -> Eff es a
withArgs args = send . WithArgs args

-- | Lifted 'Env.withProgName'.
--
-- @since 0.1
withProgName :: (EnvironmentDynamic :> es) => String -> (Eff es) () -> Eff es ()
withProgName name = send . WithProgName name

-- | Lifted 'Env.getEnvironment'.
--
-- @since 0.1
getEnvironment :: (EnvironmentDynamic :> es) => Eff es [(String, String)]
getEnvironment = send GetEnvironment
