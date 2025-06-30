{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Haskeline.Static
  ( -- * Effect
    Haskeline,
    haveTerminalUI,
    getInputLine,
    getInputLineWithInitial,
    getInputChar,
    getPassword,
    waitForAnyKey,
    outputStr,
    outputStrLn,
    getHistory,
    putHistory,
    modifyHistory,
    withInterrupt,
    handleInterrupt,

    -- ** Handlers,
    runHaskeline,
    runInputTDefault,
    runInputTWith,

    -- * Haskeline Re-exports

    -- ** IO Runners
    H.runInputT,
    H.runInputTBehavior,
    H.runInputTBehaviorWithPrefs,

    -- ** Config
    H.defaultSettings,
    H.defaultBehavior,
    H.defaultPrefs,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    runEff,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( HasCallStack,
    SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    seqUnliftIO,
    unsafeEff,
    unsafeEff_,
  )
import Effectful.Reader.Static (Reader, ask, runReader)
import System.Console.Haskeline (InputT)
import System.Console.Haskeline qualified as H
import System.Console.Haskeline.History (History)
import System.Console.Haskeline.Internal (InputTEnv)
import System.Console.Haskeline.Internal qualified as HI

-- | Static haskeline effect.
--
-- @since 0.1
data Haskeline :: Effect

type instance DispatchOf Haskeline = Static WithSideEffects

data instance StaticRep Haskeline = MkHaskeline

-- | Runs 'Haskeline' in 'IO'.
--
-- @since 0.1
runHaskeline :: (HasCallStack, IOE :> es) => Eff (Haskeline : es) a -> Eff es a
runHaskeline = evalStaticRep MkHaskeline

-- | 'runInputTWith' with default haskeline settings.
--
-- @since 0.1
runInputTDefault :: Eff [Reader (InputTEnv IO), IOE] a -> IO a
runInputTDefault = runInputTWith (H.runInputT H.defaultSettings)

-- | Runs 'Reader' 'InputTEnv' in 'IO' with 'InputT' runner.
--
-- @since 0.1
runInputTWith ::
  -- | 'InputT' Runner.
  (InputT IO a -> IO a) ->
  -- | 'Eff' action.
  Eff [Reader (InputTEnv IO), IOE] a ->
  -- | IO Result.
  IO a
runInputTWith runInput m = runInput $ HI.fromReaderT $ ReaderT $ \env ->
  runEff
    . runReader env
    $ m

-- | Lifted 'H.haveTerminalUI'.
--
-- @since 0.1
haveTerminalUI ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  Eff es Bool
haveTerminalUI = liftInputT H.haveTerminalUI

-- | Lifted 'H.getInputLine'.
--
-- @since 0.1
getInputLine ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  String ->
  Eff es (Maybe String)
getInputLine = liftInputT . H.getInputLine

-- | Lifted 'H.getInputLineWithInitial'.
--
-- @since 0.1
getInputLineWithInitial ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  String ->
  (String, String) ->
  Eff es (Maybe String)
getInputLineWithInitial s = liftInputT . H.getInputLineWithInitial s

-- | Lifted 'H.getInputChar'.
--
-- @since 0.1
getInputChar ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  String ->
  Eff es (Maybe Char)
getInputChar = liftInputT . H.getInputChar

-- | Lifted 'H.getPassword'.
--
-- @since 0.1
getPassword ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  Maybe Char ->
  String ->
  Eff es (Maybe String)
getPassword c = liftInputT . H.getPassword c

-- | Lifted 'H.waitForAnyKey'.
--
-- @since 0.1
waitForAnyKey ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  String ->
  Eff es Bool
waitForAnyKey = liftInputT . H.waitForAnyKey

-- | Lifted 'H.outputStr'.
--
-- @since 0.1
outputStr ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  String ->
  Eff es ()
outputStr = liftInputT . H.outputStr

-- | Lifted 'H.outputStrLn'.
--
-- @since 0.1
outputStrLn ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  String ->
  Eff es ()
outputStrLn = liftInputT . H.outputStrLn

-- | Lifted 'H.getHistory'.
--
-- @since 0.1
getHistory ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  Eff es History
getHistory = liftInputT H.getHistory

-- | Lifted 'H.putHistory'.
--
-- @since 0.1
putHistory ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  History ->
  Eff es ()
putHistory = liftInputT . H.putHistory

-- | Lifted 'H.modifyHistory'.
--
-- @since 0.1
modifyHistory ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  (History -> History) ->
  Eff es ()
modifyHistory = liftInputT . H.modifyHistory

-- | Lifted 'H.withInterrupt'.
--
-- @since 0.1
withInterrupt ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  Eff es a ->
  Eff es a
withInterrupt action =
  unsafeEff $ \env -> seqUnliftIO env $
    \runInIO ->
      runInIO $
        liftInputT $
          H.withInterrupt $
            liftIO $
              runInIO action

-- | Lifted 'H.handleInterrupt'.
--
-- @since 0.1
handleInterrupt ::
  ( HasCallStack,
    Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  Eff es a ->
  Eff es a ->
  Eff es a
handleInterrupt m1 m2 =
  unsafeEff $ \env -> seqUnliftIO env $
    \runInIO ->
      runInIO $
        liftInputT $
          H.handleInterrupt (liftIO $ runInIO m1) (liftIO $ runInIO m2)

liftInputT :: (Reader (InputTEnv IO) :> es) => InputT IO a -> Eff es a
liftInputT f = ask >>= unsafeEff_ . runReaderT (HI.toReaderT f)
