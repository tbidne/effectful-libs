{-# LANGUAGE CPP #-}

module Unit.Notify (tests) where

import Effectful (Eff, IOE, runEff)
import Effectful.Notify.Static (Notify)
import Effectful.Notify.Static qualified as Notify
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

#if LINUX
import Control.Concurrent qualified as CC
#endif

tests :: TestTree
tests =
  testGroup
    "Notify"
    (testSendNotif : osTests)

testSendNotif :: TestTree
testSendNotif = testCase desc $ runner $ do
  env <- Notify.initNotifyEnv Notify.defaultNotifySystemOs
  Notify.notify env note
  where
    desc = "Sends notification with default system"

osTests :: [TestTree]

#if LINUX

osTests = [ testNotifySend ]

testNotifySend :: TestTree
testNotifySend = testCase desc $ do
  -- HACK: Executing this and testSendNotif at the same time leads to an
  -- error:
  --
  --     Created too many similar notifications in quick succession
  --
  -- Half a second is too fast apparently, but a second seems okay?
  CC.threadDelay 1_000_000
  runner $ do
    env <- Notify.initNotifyEnv Notify.NotifySystemOsNotifySend
    Notify.notify env note
  where
    desc = "Sends notification with notify-send"

#else

osTests = []

#endif

note :: Notify.Note
note =
  Notify.setBody (Just "Notify \"body\"")
    . Notify.setTitle (Just "Some \"title\"")
    . Notify.setTimeout (Just $ Notify.NotifyTimeoutMillis 5_000)
    $ Notify.mkNote "A notification \"summary\""

runner :: Eff [Notify, IOE] a -> IO a
runner = runEff . Notify.runNotify
