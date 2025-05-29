module Main (main) where

import Effectful (Eff, IOE, runEff)
import Effectful.Terminal.Dynamic qualified as Dynamic
import Effectful.Terminal.Static qualified as Static
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit Tests"
      [ testUnicodeStr,
        testUnicodeText,
        testUnicodeBS
      ]

testUnicodeStr :: TestTree
testUnicodeStr = testCase desc $ do
  runStatic $ Static.putStrLn "🙂"
  runDynamic $ Dynamic.putStrLn "🙂"
  where
    desc = "Prints unicode emoji string"

testUnicodeText :: TestTree
testUnicodeText = testCase desc $ do
  runStatic $ Static.putTextLn "🙂"
  runDynamic $ Dynamic.putTextLn "🙂"
  where
    desc = "Prints unicode emoji text"

testUnicodeBS :: TestTree
testUnicodeBS = testCase desc $ do
  runStatic $ Static.putBinary "🙂"
  runDynamic $ Dynamic.putBinary "🙂"
  where
    desc = "Prints unicode emoji bytestring"

runDynamic :: Eff [Dynamic.Terminal, IOE] a -> IO a
runDynamic = runEff . Dynamic.runTerminal

runStatic :: Eff [Static.Terminal, IOE] a -> IO a
runStatic = runEff . Static.runTerminal
