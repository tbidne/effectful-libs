-- | Provides an effect for optparse-applicative.
--
-- @since 0.1
module Effectful.Optparse
  ( -- * Effect
    OptparseEffect (..),
    execParser,
    customExecParser,
    handleParseResult,

    -- ** Handler
    runOptparseIO,
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
import Options.Applicative (ParserInfo, ParserPrefs, ParserResult)
import Options.Applicative qualified as OA
import Effectful.Dispatch.Dynamic (interpret, send)

-- | Effect for optparse-applicative.
--
-- @since 0.1
data OptparseEffect :: Effect where
  ExecParser :: ParserInfo a -> OptparseEffect m a
  CustomExecParser :: ParserPrefs -> ParserInfo a -> OptparseEffect m a
  HandleParseResult :: ParserResult a -> OptparseEffect m a

-- | @since 0.1
type instance DispatchOf OptparseEffect = Dynamic

-- | Runs 'OptparseEffect' in 'IO'.
--
-- @since 0.1
runOptparseIO ::
  ( IOE :> es
  ) =>
  Eff (OptparseEffect : es) a ->
  Eff es a
runOptparseIO = interpret $ \_ -> \case
  ExecParser i -> liftIO $ OA.execParser i
  CustomExecParser prefs i -> liftIO $ OA.customExecParser prefs i
  HandleParseResult r -> liftIO $ OA.handleParseResult r

-- | @since 0.1
execParser :: (OptparseEffect :> es) => ParserInfo a -> Eff es a
execParser = send . ExecParser

-- | @since 0.1
customExecParser :: (OptparseEffect :> es) => ParserPrefs -> ParserInfo a -> Eff es a
customExecParser prefs = send . CustomExecParser prefs

-- | @since 0.1
handleParseResult :: (OptparseEffect :> es) => ParserResult a -> Eff es a
handleParseResult = send . HandleParseResult
