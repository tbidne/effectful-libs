-- | Provides a dynamic effect for optparse-applicative.
--
-- @since 0.1
module Effectful.Optparse.Dynamic
  ( -- * Effect
    Optparse (..),
    execParser,
    customExecParser,
    handleParseResult,

    -- ** Handler
    runOptparse,

    -- * Misc
    Utils.osPath,
    Utils.validOsPath,
  )
where

import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret_, send)
import Effectful.Optparse.Static qualified as Static
import Effectful.Optparse.Utils qualified as Utils
import Options.Applicative (ParserInfo, ParserPrefs, ParserResult)

-- | Dynamic effect for optparse-applicative.
--
-- @since 0.1
data Optparse :: Effect where
  ExecParser :: ParserInfo a -> Optparse m a
  CustomExecParser :: ParserPrefs -> ParserInfo a -> Optparse m a
  HandleParseResult :: ParserResult a -> Optparse m a

-- | @since 0.1
type instance DispatchOf Optparse = Dynamic

-- | Runs 'Optparse' in 'IO'.
--
-- @since 0.1
runOptparse ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (Optparse : es) a ->
  Eff es a
runOptparse = reinterpret_ Static.runOptparse $ \case
  ExecParser i -> Static.execParser i
  CustomExecParser prefs i -> Static.customExecParser prefs i
  HandleParseResult r -> Static.handleParseResult r

-- | Lifted 'OA.execParser'.
--
-- @since 0.1
execParser :: (HasCallStack, Optparse :> es) => ParserInfo a -> Eff es a
execParser = send . ExecParser

-- | Lifted 'OA.customExecParser'.
--
-- @since 0.1
customExecParser ::
  (HasCallStack, Optparse :> es) =>
  ParserPrefs ->
  ParserInfo a ->
  Eff es a
customExecParser prefs = send . CustomExecParser prefs

-- | Lifted 'OA.handleParseResult'.
--
-- @since 0.1
handleParseResult :: (HasCallStack, Optparse :> es) => ParserResult a -> Eff es a
handleParseResult = send . HandleParseResult
