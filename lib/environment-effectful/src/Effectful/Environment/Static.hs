{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- ORMOLU_DISABLE -}

-- | Static effects for "System.Environment". The primary interface exists at:
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-Environment.html.
--
-- This module exists to provide a static version of the
-- "Effectful.Environment.Dynamic", and to provide an Eff version of
-- @executablePath@ for @base >= 4.17@.
--
-- @since 0.1
module Effectful.Environment.Static
  ( -- * Effect
    Environment,

    -- ** Functions
    Eff.Env.getArgs,
    Eff.Env.getProgName,
#if MIN_VERSION_base(4,17,0)
    executablePath,
#endif
    Eff.Env.getExecutablePath,
    Eff.Env.getEnv,
    Eff.Env.lookupEnv,
    Eff.Env.setEnv,
    Eff.Env.unsetEnv,
    Eff.Env.withArgs,
    Eff.Env.withProgName,
    Eff.Env.getEnvironment,

    -- ** Handlers
    runEnvironment,

    -- * Types
    QueryExePath (..),
  )
where

{- ORMOLU_ENABLE -}

#if MIN_VERSION_base(4,17,0)
import Effectful (Eff, type (:>))
import Effectful.Dispatch.Static (unsafeEff_)
import System.Environment qualified as Env
#endif
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Environment qualified as Eff.Env
import Effectful.Environment.Utils (QueryExePath (NoQuery, QueryResult))

{- ORMOLU_DISABLE -}

#if MIN_VERSION_base(4,17,0)
  -- | Lifted 'Env.executablePath'.
  --
  -- @since 0.1
executablePath :: (Environment :> es) => Eff es QueryExePath
executablePath = case Env.executablePath of
    Nothing -> pure NoQuery
    Just io -> QueryResult <$> unsafeEff_ io
#endif

{- ORMOLU_ENABLE -}
