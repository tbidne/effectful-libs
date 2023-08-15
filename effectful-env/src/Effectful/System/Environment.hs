{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Provides the 'EnvEffect' effect.
--
-- @since 0.1
module Effectful.System.Environment
  ( -- * Effect
    EnvEffect (..),
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
    runEnvIO,

    -- * Types
    QueryExePath (..),
  )
where

import System.Environment qualified as Env
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>), MonadIO (liftIO),
  )
import Effectful.Dispatch.Dynamic (interpret, send, localSeqUnliftIO)

-- | Effect for 'System.Environment'.
--
-- @since 0.1
data EnvEffect :: Effect where
  GetArgs :: EnvEffect m [String]
  GetProgName :: EnvEffect m String
#if MIN_VERSION_base(4,17,0)
  ExecutablePath :: (EnvEffect m QueryExePath)
#endif
  GetExecutablePath :: EnvEffect m FilePath
  GetEnv :: String -> EnvEffect m String
  LookupEnv :: String -> EnvEffect m (Maybe String)
  SetEnv :: String -> String -> EnvEffect m ()
  UnsetEnv :: String -> EnvEffect m ()
  WithArgs :: [String] -> m a -> EnvEffect m a
  WithProgName :: String -> m () -> EnvEffect m ()
  GetEnvironment :: EnvEffect m [(String, String)]

{- ORMOLU_ENABLE -}

-- | @since 0.1
type instance DispatchOf EnvEffect = Dynamic

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

-- | Runs 'STMEffect' in 'IO'.
--
-- @since 0.1
runEnvIO ::
  ( IOE :> es
  ) =>
  Eff (EnvEffect : es) a ->
  Eff es a
runEnvIO = interpret $ \env -> \case
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

-- | @since 0.1
getArgs :: (EnvEffect :> es) => Eff es [String]
getArgs = send GetArgs

-- | @since 0.1
getProgName :: (EnvEffect :> es) => Eff es String
getProgName = send GetProgName

#if MIN_VERSION_base(4,17,0)

-- | @since 0.1
executablePath :: (EnvEffect :> es) => Eff es QueryExePath
executablePath = send ExecutablePath

#endif

-- | @since 0.1
getExecutablePath :: (EnvEffect :> es) => Eff es FilePath
getExecutablePath = send GetExecutablePath

-- | @since 0.1
getEnv :: (EnvEffect :> es) => String -> Eff es String
getEnv = send . GetEnv

-- | @since 0.1
lookupEnv :: (EnvEffect :> es) => String -> Eff es (Maybe String)
lookupEnv = send . LookupEnv

-- | @since 0.1
setEnv :: (EnvEffect :> es) => String -> String -> Eff es ()
setEnv s = send . SetEnv s

-- | @since 0.1
unsetEnv :: (EnvEffect :> es) => String -> Eff es ()
unsetEnv = send . UnsetEnv

-- | @since 0.1
withArgs :: (EnvEffect :> es) => [String] -> (Eff es) a -> Eff es a
withArgs args = send . WithArgs args

-- | @since 0.1
withProgName :: (EnvEffect :> es) => String -> (Eff es) () -> Eff es ()
withProgName name = send . WithProgName name

-- | @since 0.1
getEnvironment :: (EnvEffect :> es) => Eff es [(String, String)]
getEnvironment = send GetEnvironment
