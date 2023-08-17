{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for optparse-applicative.
--
-- @since 0.1
module Effectful.Optparse.Static
  ( -- * Effect
    OptparseStatic,
    execParser,
    customExecParser,
    handleParseResult,

    -- ** Handler
    runOptparseStaticIO,
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
import Options.Applicative (ParserInfo, ParserPrefs, ParserResult)
import Options.Applicative qualified as OA

-- | Static effect for optparse-applicative.
--
-- @since 0.1
data OptparseStatic :: Effect

type instance DispatchOf OptparseStatic = Static WithSideEffects

data instance StaticRep OptparseStatic = MkOptparseStatic

-- | Runs an OptparseStatic effect.
--
-- @since 0.1
runOptparseStaticIO :: (IOE :> es) => Eff (OptparseStatic : es) a -> Eff es a
runOptparseStaticIO = evalStaticRep MkOptparseStatic

-- | Lifted 'OA.execParser'.
--
-- @since 0.1
execParser :: (OptparseStatic :> es) => ParserInfo a -> Eff es a
execParser = unsafeEff_ . OA.execParser

-- | Lifted 'OA.execParser'.
--
-- @since 0.1
customExecParser ::
  (OptparseStatic :> es) =>
  ParserPrefs ->
  ParserInfo a ->
  Eff es a
customExecParser prefs = unsafeEff_ . OA.customExecParser prefs

-- | Lifted 'OA.execParser'.
--
-- @since 0.1
handleParseResult :: (OptparseStatic :> es) => ParserResult a -> Eff es a
handleParseResult = unsafeEff_ . OA.handleParseResult
