-- | Provides a dynamic effect for optparse-applicative.
--
-- @since 0.1
module Effectful.Optparse.Dynamic
  ( -- * Effect
    OptparseDynamic (..),
    execParser,
    customExecParser,
    handleParseResult,

    -- ** Handler
    runOptparseDynamicIO,
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
import Effectful.Dispatch.Dynamic (interpret, send)
import Options.Applicative (ParserInfo, ParserPrefs, ParserResult)
import Options.Applicative qualified as OA

-- | Effect for optparse-applicative.
--
-- @since 0.1
data OptparseDynamic :: Effect where
  ExecParser :: ParserInfo a -> OptparseDynamic m a
  CustomExecParser :: ParserPrefs -> ParserInfo a -> OptparseDynamic m a
  HandleParseResult :: ParserResult a -> OptparseDynamic m a

-- | @since 0.1
type instance DispatchOf OptparseDynamic = Dynamic

-- | Runs 'OptparseDynamic' in 'IO'.
--
-- @since 0.1
runOptparseDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (OptparseDynamic : es) a ->
  Eff es a
runOptparseDynamicIO = interpret $ \_ -> \case
  ExecParser i -> liftIO $ OA.execParser i
  CustomExecParser prefs i -> liftIO $ OA.customExecParser prefs i
  HandleParseResult r -> liftIO $ OA.handleParseResult r

-- | Lifted 'OA.execParser'.
--
-- @since 0.1
execParser :: (OptparseDynamic :> es) => ParserInfo a -> Eff es a
execParser = send . ExecParser

-- | Lifted 'OA.customExecParser'.
--
-- @since 0.1
customExecParser :: (OptparseDynamic :> es) => ParserPrefs -> ParserInfo a -> Eff es a
customExecParser prefs = send . CustomExecParser prefs

-- | Lifted 'OA.handleParseResult'.
--
-- @since 0.1
handleParseResult :: (OptparseDynamic :> es) => ParserResult a -> Eff es a
handleParseResult = send . HandleParseResult
