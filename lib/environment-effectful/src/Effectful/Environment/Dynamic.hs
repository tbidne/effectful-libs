{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Dynamic effects for "System.Environment". For static effects, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-Environment.html.
--
-- @since 0.1
module Effectful.Environment.Dynamic
  ( -- * Effect
    Environment (..),
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
    runEnvironment,

    -- * Types
    QueryExePath (..),
  )
where

{- ORMOLU_ENABLE -}

import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( HasCallStack,
    localSeqUnlift,
    reinterpret,
    send,
  )
import Effectful.Environment.Static qualified as Static
import Effectful.Environment.Utils (QueryExePath (NoQuery, QueryResult))

{- ORMOLU_DISABLE -}

-- | Dynamic effects for "System.Environment".
--
-- @since 0.1
data Environment :: Effect where
  GetArgs :: Environment m [String]
  GetProgName :: Environment m String
#if MIN_VERSION_base(4,17,0)
  ExecutablePath :: (Environment m QueryExePath)
#endif
  GetExecutablePath :: Environment m FilePath
  GetEnv :: String -> Environment m String
  LookupEnv :: String -> Environment m (Maybe String)
  SetEnv :: String -> String -> Environment m ()
  UnsetEnv :: String -> Environment m ()
  WithArgs :: [String] -> m a -> Environment m a
  WithProgName :: String -> m () -> Environment m ()
  GetEnvironment :: Environment m [(String, String)]

{- ORMOLU_ENABLE -}

-- | @since 0.1
type instance DispatchOf Environment = Dynamic

{- ORMOLU_DISABLE -}

-- | Runs 'Environment' in 'IO'.
--
-- @since 0.1
runEnvironment ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (Environment : es) a ->
  Eff es a
runEnvironment = reinterpret Static.runEnvironment $ \env -> \case
  GetArgs -> Static.getArgs
  GetProgName -> Static.getProgName
#if MIN_VERSION_base(4,17,0)
  ExecutablePath -> Static.executablePath
#endif
  GetExecutablePath -> Static.getExecutablePath
  GetEnv s -> Static.getEnv s
  LookupEnv s -> Static.lookupEnv s
  SetEnv s t -> Static.setEnv s t
  UnsetEnv s -> Static.unsetEnv s
  WithArgs args m -> localSeqUnlift env $ \runInStatic ->
    Static.withArgs args (runInStatic m)
  WithProgName name m -> localSeqUnlift env $ \runInStatic ->
    Static.withProgName name (runInStatic m)
  GetEnvironment -> Static.getEnvironment

{- ORMOLU_ENABLE -}

-- | Lifted 'Env.getArgs'.
--
-- @since 0.1
getArgs :: (Environment :> es, HasCallStack) => Eff es [String]
getArgs = send GetArgs

-- | Lifted 'Env.getProgName'.
--
-- @since 0.1
getProgName :: (Environment :> es, HasCallStack) => Eff es String
getProgName = send GetProgName

#if MIN_VERSION_base(4,17,0)

-- | Lifted 'Env.executablePath'.
--
-- @since 0.1
executablePath :: (Environment :> es, HasCallStack) => Eff es QueryExePath
executablePath = send ExecutablePath

#endif

-- | Lifted 'Env.getExecutablePath'.
--
-- @since 0.1
getExecutablePath :: (Environment :> es, HasCallStack) => Eff es FilePath
getExecutablePath = send GetExecutablePath

-- | Lifted 'Env.getEnv'.
--
-- @since 0.1
getEnv :: (Environment :> es, HasCallStack) => String -> Eff es String
getEnv = send . GetEnv

-- | Lifted 'Env.lookupEnv'.
--
-- @since 0.1
lookupEnv ::
  (Environment :> es, HasCallStack) =>
  String ->
  Eff es (Maybe String)
lookupEnv = send . LookupEnv

-- | Lifted 'Env.setEnv'.
--
-- @since 0.1
setEnv :: (Environment :> es, HasCallStack) => String -> String -> Eff es ()
setEnv s = send . SetEnv s

-- | Lifted 'Env.unsetEnv'.
--
-- @since 0.1
unsetEnv :: (Environment :> es, HasCallStack) => String -> Eff es ()
unsetEnv = send . UnsetEnv

-- | Lifted 'Env.withArgs'.
--
-- @since 0.1
withArgs ::
  (Environment :> es, HasCallStack) =>
  [String] ->
  Eff es a ->
  Eff es a
withArgs args = send . WithArgs args

-- | Lifted 'Env.withProgName'.
--
-- @since 0.1
withProgName ::
  (Environment :> es, HasCallStack) =>
  String ->
  Eff es () ->
  Eff es ()
withProgName name = send . WithProgName name

-- | Lifted 'Env.getEnvironment'.
--
-- @since 0.1
getEnvironment :: (Environment :> es, HasCallStack) => Eff es [(String, String)]
getEnvironment = send GetEnvironment
