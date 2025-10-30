{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for haskeline.
--
-- @since 0.1
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

    -- ** Handlers
    runHaskeline,

    -- * Reader
    -- $reader
    runEffInputTEnv,
    runInputTEnv,
    runInputTEnvWith,

    -- * Haskeline Re-exports

    -- ** Types
    InputT,
    InputTEnv,

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

import Control.Monad.Catch (MonadMask)
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
import System.Console.Haskeline.ReaderT (InputTEnv)
import System.Console.Haskeline.ReaderT qualified as HR

-- | Static haskeline effect.
--
-- @since 0.1
data Haskeline :: Effect

type instance DispatchOf Haskeline = Static WithSideEffects

data instance StaticRep Haskeline = MkHaskeline

-- $reader
--
-- These functions allow eliminating the 'Haskeline' effect in terms of
-- 'Control.Monad.Trans.Reader.ReaderT'.
--
-- __Examples:__
--
-- @
--  app :: (Haskeline :> es, Reader (InputTEnv IO) :> es) => Eff es String
--  app = do
--    mLine <- getInputLine "Enter your name: "
--    let name = fromMaybe "\<blank\>" mLine
--    outputStrLn $ "Hello: " ++ name
--    pure name
--
--  appIO :: IO String
--  appIO = do
--    runInputTEnv
--      $ \env -> runEffInputTEnv env
--      $ runHaskeline app
-- @

-- | Runs 'Haskeline' in 'IO'.
--
-- @since 0.1
runHaskeline :: (HasCallStack, IOE :> es) => Eff (Haskeline : es) a -> Eff es a
runHaskeline = evalStaticRep MkHaskeline

-- | Runner for 'Eff' with 'InputTEnv'. Intended for usage with 'runInputTEnv'
-- or 'runInputTEnvWith'.
--
-- @since 0.1
runEffInputTEnv ::
  (HasCallStack) =>
  -- | The 'InputT' environment.
  InputTEnv m ->
  -- | Eff action that requires the 'InputTEnv' environment.
  Eff [Reader (InputTEnv m), IOE] a ->
  -- | IO result.
  IO a
runEffInputTEnv env = runEff . runReader env
{-# INLINEABLE runEffInputTEnv #-}

-- | 'runInputTEnvWith' with default haskeline settings.
--
-- __Examples:__
--
-- @
-- -- eff :: Eff [Reader (InputTEnv m), IOE] a
-- runInputTEnv $ \env -> runEffInputTEnv env eff
-- @
--
-- @since 0.1
runInputTEnv ::
  ( HasCallStack,
    MonadIO m,
    MonadMask m
  ) =>
  -- | Action.
  (InputTEnv m -> m a) ->
  -- | IO result.
  m a
runInputTEnv = runInputTEnvWith (H.runInputT H.defaultSettings)
{-# INLINEABLE runInputTEnv #-}

-- | Runs 'Control.Monad.Reader.ReaderT' 'InputTEnv' in 'IO' with 'InputT'
-- runner.
--
-- __Examples:__
--
-- @
-- -- eff :: Eff [Reader (InputTEnv m), IOE] a
-- runInputTEnvWith runInput $ \env -> runEffInputTEnv env eff
-- @
--
-- @since 0.1
runInputTEnvWith ::
  (HasCallStack) =>
  -- | 'InputT' runner.
  (InputT m a -> m a) ->
  -- | Action.
  (InputTEnv m -> m a) ->
  -- | IO result.
  m a
runInputTEnvWith runInput = runInput . HR.fromReaderT . ReaderT
{-# INLINEABLE runInputTEnvWith #-}

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
liftInputT f = ask >>= unsafeEff_ . runReaderT (HR.toReaderT f)
