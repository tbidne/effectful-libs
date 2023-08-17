{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Dynamic effects for "System.Environment". For static effects, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-Environment.html.
--
-- @since 0.1
module Effectful.Environment.Dynamic
  ( -- * Effect
    EnvDynamic (..),
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
    runEnvDynamicIO,

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
data EnvDynamic :: Effect where
  GetArgs :: EnvDynamic m [String]
  GetProgName :: EnvDynamic m String
#if MIN_VERSION_base(4,17,0)
  ExecutablePath :: (EnvDynamic m QueryExePath)
#endif
  GetExecutablePath :: EnvDynamic m FilePath
  GetEnv :: String -> EnvDynamic m String
  LookupEnv :: String -> EnvDynamic m (Maybe String)
  SetEnv :: String -> String -> EnvDynamic m ()
  UnsetEnv :: String -> EnvDynamic m ()
  WithArgs :: [String] -> m a -> EnvDynamic m a
  WithProgName :: String -> m () -> EnvDynamic m ()
  GetEnvironment :: EnvDynamic m [(String, String)]

{- ORMOLU_ENABLE -}

-- | @since 0.1
type instance DispatchOf EnvDynamic = Dynamic

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

-- | Runs 'EnvDynamic' in 'IO'.
--
-- @since 0.1
runEnvDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (EnvDynamic : es) a ->
  Eff es a
runEnvDynamicIO = interpret $ \env -> \case
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
  WithArgs args m -> localSeqUnliftIO env $ \runInDynamicIO ->
    liftIO $ Env.withArgs args (runInDynamicIO m)
  WithProgName name m -> localSeqUnliftIO env $ \runInDynamicIO ->
    liftIO $ Env.withProgName name (runInDynamicIO m)
  GetEnvironment -> liftIO Env.getEnvironment

{- ORMOLU_ENABLE -}

-- | Lifted 'Env.getArgs'.
--
-- @since 0.1
getArgs :: (EnvDynamic :> es) => Eff es [String]
getArgs = send GetArgs

-- | Lifted 'Env.getProgName'.
--
-- @since 0.1
getProgName :: (EnvDynamic :> es) => Eff es String
getProgName = send GetProgName

#if MIN_VERSION_base(4,17,0)

-- | Lifted 'Env.executablePath'.
--
-- @since 0.1
executablePath :: (EnvDynamic :> es) => Eff es QueryExePath
executablePath = send ExecutablePath

#endif

-- | Lifted 'Env.getExecutablePath'.
--
-- @since 0.1
getExecutablePath :: (EnvDynamic :> es) => Eff es FilePath
getExecutablePath = send GetExecutablePath

-- | Lifted 'Env.getEnv'.
--
-- @since 0.1
getEnv :: (EnvDynamic :> es) => String -> Eff es String
getEnv = send . GetEnv

-- | Lifted 'Env.lookupEnv'.
--
-- @since 0.1
lookupEnv :: (EnvDynamic :> es) => String -> Eff es (Maybe String)
lookupEnv = send . LookupEnv

-- | Lifted 'Env.setEnv'.
--
-- @since 0.1
setEnv :: (EnvDynamic :> es) => String -> String -> Eff es ()
setEnv s = send . SetEnv s

-- | Lifted 'Env.unsetEnv'.
--
-- @since 0.1
unsetEnv :: (EnvDynamic :> es) => String -> Eff es ()
unsetEnv = send . UnsetEnv

-- | Lifted 'Env.withArgs'.
--
-- @since 0.1
withArgs :: (EnvDynamic :> es) => [String] -> (Eff es) a -> Eff es a
withArgs args = send . WithArgs args

-- | Lifted 'Env.withProgName'.
--
-- @since 0.1
withProgName :: (EnvDynamic :> es) => String -> (Eff es) () -> Eff es ()
withProgName name = send . WithProgName name

-- | Lifted 'Env.getEnvironment'.
--
-- @since 0.1
getEnvironment :: (EnvDynamic :> es) => Eff es [(String, String)]
getEnvironment = send GetEnvironment
