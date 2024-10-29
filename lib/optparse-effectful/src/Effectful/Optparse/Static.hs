{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for optparse-applicative.
--
-- @since 0.1
module Effectful.Optparse.Static
  ( -- * Effect
    Optparse,
    execParser,
    customExecParser,
    handleParseResult,

    -- ** Handler
    runOptparse,

    -- * Misc
    Utils.OsPath,
    Utils.osPath,
    Utils.validOsPath,
  )
where

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
    unsafeEff_,
  )
import Effectful.Optparse.Utils qualified as Utils
import Options.Applicative (ParserInfo, ParserPrefs, ParserResult)
import Options.Applicative qualified as OA

-- | Static effect for optparse-applicative.
--
-- @since 0.1
data Optparse :: Effect

type instance DispatchOf Optparse = Static WithSideEffects

data instance StaticRep Optparse = MkOptparse

-- | Runs an Optparse effect.
--
-- @since 0.1
runOptparse :: (IOE :> es) => Eff (Optparse : es) a -> Eff es a
runOptparse = evalStaticRep MkOptparse

-- | Lifted 'OA.execParser'.
--
-- @since 0.1
execParser :: (Optparse :> es) => ParserInfo a -> Eff es a
execParser = unsafeEff_ . OA.execParser

-- | Lifted 'OA.execParser'.
--
-- @since 0.1
customExecParser ::
  (Optparse :> es) =>
  ParserPrefs ->
  ParserInfo a ->
  Eff es a
customExecParser prefs = unsafeEff_ . OA.customExecParser prefs

-- | Lifted 'OA.execParser'.
--
-- @since 0.1
handleParseResult :: (Optparse :> es) => ParserResult a -> Eff es a
handleParseResult = unsafeEff_ . OA.handleParseResult
