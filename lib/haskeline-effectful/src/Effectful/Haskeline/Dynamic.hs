-- | Provides a dynamic effect for haskeline.
--
-- @since 0.1
module Effectful.Haskeline.Dynamic
  ( -- * Effect
    Haskeline (..),
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
    Static.runEffInputTEnv,
    Static.runInputTEnv,
    Static.runInputTEnvWith,

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

import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (HasCallStack, localSeqUnlift, reinterpret, send)
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.Haskeline.Static qualified as Static
import Effectful.Reader.Static (Reader)
import System.Console.Haskeline (InputT)
import System.Console.Haskeline qualified as H
import System.Console.Haskeline.History (History)
import System.Console.Haskeline.ReaderT (InputTEnv)

-- | @since 0.1
type instance DispatchOf Haskeline = Dynamic

-- | Dynamic haskeline effect.
--
-- @since 0.1
data Haskeline :: Effect where
  HaveTerminalUI :: Haskeline m Bool
  GetInputLine :: String -> Haskeline m (Maybe String)
  GetInputLineWithInitial :: String -> (String, String) -> Haskeline m (Maybe String)
  GetInputChar :: String -> Haskeline m (Maybe Char)
  GetPassword :: Maybe Char -> String -> Haskeline m (Maybe String)
  WaitForAnyKey :: String -> Haskeline m Bool
  OutputStr :: String -> Haskeline m ()
  OutputStrLn :: String -> Haskeline m ()
  GetHistory :: Haskeline m History
  PutHistory :: History -> Haskeline m ()
  ModifyHistory :: (History -> History) -> Haskeline m ()
  WithInterrupt :: m a -> Haskeline m a
  HandleInterrupt :: m a -> m a -> Haskeline m a

-- | @since 0.1
instance ShowEffect Haskeline where
  showEffectCons = \case
    HaveTerminalUI -> "HaveTerminalUI"
    GetInputLine _ -> "GetInputLine"
    GetInputLineWithInitial {} -> "GetInputLineWithInitial"
    GetInputChar _ -> "GetInputChar"
    GetPassword {} -> "GetPassword"
    WaitForAnyKey _ -> "WaitForAnyKey"
    OutputStr _ -> "OutputStr"
    OutputStrLn _ -> "OutputStrLn"
    GetHistory -> "GetHistory"
    PutHistory _ -> "PutHistory"
    ModifyHistory _ -> "ModifyHistory"
    WithInterrupt _ -> "WithInterrupt"
    HandleInterrupt {} -> "HandleInterrupt"

-- | Runs 'Haskeline' in 'IO'.
--
-- @since 0.1
runHaskeline ::
  forall es a.
  ( HasCallStack,
    IOE :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  Eff (Haskeline : es) a ->
  Eff es a
runHaskeline = reinterpret Static.runHaskeline $ \env -> \case
  HaveTerminalUI -> Static.haveTerminalUI
  GetInputLine s -> Static.getInputLine s
  GetInputLineWithInitial s t -> Static.getInputLineWithInitial s t
  GetInputChar s -> Static.getInputChar s
  GetPassword s c -> Static.getPassword s c
  WaitForAnyKey s -> Static.waitForAnyKey s
  OutputStr s -> Static.outputStr s
  OutputStrLn s -> Static.outputStrLn s
  GetHistory -> Static.getHistory
  PutHistory h -> Static.putHistory h
  ModifyHistory f -> Static.modifyHistory f
  WithInterrupt m ->
    localSeqUnlift env $ \runInStatic ->
      Static.withInterrupt (runInStatic m)
  HandleInterrupt m1 m2 ->
    localSeqUnlift env $ \runInStatic ->
      Static.handleInterrupt (runInStatic m1) (runInStatic m2)

-- $reader
--
-- These functions allow eliminating the 'Haskeline' effect in terms of
-- 'Control.Monad.Trans.Reader.ReaderT'.
--
-- __Examples:__
--
-- @
--  app :: (Haskeline :> es) => Eff es String
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
--
-- runHaskelinePure :: Eff (Haskeline : es) a -> Eff es a
-- runHaskelinePure = interpret_ $ \\case
--   GetInputLine _ -> pure $ Just "some name"
--   OutputStrLn _ -> pure ()
--   _ -> error "todo"
--
-- appPure :: String
-- appPure = runPureEff $ runHaskelinePure app
-- @

-- | Lifted 'H.haveTerminalUI'.
--
-- @since 0.1
haveTerminalUI :: (HasCallStack, Haskeline :> es) => Eff es Bool
haveTerminalUI = send HaveTerminalUI

-- | Lifted 'H.getInputLine'.
--
-- @since 0.1
getInputLine ::
  ( HasCallStack,
    Haskeline :> es
  ) =>
  String -> Eff es (Maybe String)
getInputLine = send . GetInputLine

-- | Lifted 'H.getInputLineWithInitial'.
--
-- @since 0.1
getInputLineWithInitial ::
  ( HasCallStack,
    Haskeline :> es
  ) =>
  String ->
  (String, String) ->
  Eff es (Maybe String)
getInputLineWithInitial s = send . GetInputLineWithInitial s

-- | Lifted 'H.havegetInputCharTerminalUI'.
--
-- @since 0.1
getInputChar :: (HasCallStack, Haskeline :> es) => String -> Eff es (Maybe Char)
getInputChar = send . GetInputChar

-- | Lifted 'H.getPassword'.
--
-- @since 0.1
getPassword ::
  ( HasCallStack,
    Haskeline :> es
  ) =>
  Maybe Char ->
  String ->
  Eff es (Maybe String)
getPassword c = send . GetPassword c

-- | Lifted 'H.waitForAnyKey'.
--
-- @since 0.1
waitForAnyKey :: (HasCallStack, Haskeline :> es) => String -> Eff es Bool
waitForAnyKey = send . WaitForAnyKey

-- | Lifted 'H.outputStr'.
--
-- @since 0.1
outputStr :: (HasCallStack, Haskeline :> es) => String -> Eff es ()
outputStr = send . OutputStr

-- | Lifted 'H.outputStrLn'.
--
-- @since 0.1
outputStrLn :: (HasCallStack, Haskeline :> es) => String -> Eff es ()
outputStrLn = send . OutputStrLn

-- | Lifted 'H.getHistory'.
--
-- @since 0.1
getHistory :: (HasCallStack, Haskeline :> es) => Eff es History
getHistory = send GetHistory

-- | Lifted 'H.putHistory'.
--
-- @since 0.1
putHistory :: (HasCallStack, Haskeline :> es) => History -> Eff es ()
putHistory = send . PutHistory

-- | Lifted 'H.modifyHistory'.
--
-- @since 0.1
modifyHistory :: (HasCallStack, Haskeline :> es) => (History -> History) -> Eff es ()
modifyHistory = send . ModifyHistory

-- | Lifted 'H.withInterrupt'.
--
-- @since 0.1
withInterrupt :: (HasCallStack, Haskeline :> es) => Eff es a -> Eff es a
withInterrupt = send . WithInterrupt

-- | Lifted 'H.handleInterrupt'.
--
-- @since 0.1
handleInterrupt ::
  ( HasCallStack,
    Haskeline :> es
  ) =>
  Eff es a ->
  Eff es a ->
  Eff es a
handleInterrupt m1 = send . HandleInterrupt m1
