module Main (main) where

import Control.Exception (Exception, SomeException, throwIO, try)
import Data.Functor ((<&>))
import Data.String (IsString (fromString))
import Effectful (Eff, IOE, runEff)
import Effectful.CallStack
  ( ECallStack,
    addCallStack,
    displayCallStack,
    getCallStack,
    runECallStack,
    throwWithCallStack,
  )
import Effectful.Dispatch.Static (unsafeEff_)
import GHC.Stack (prettyCallStack)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit Tests"
      [ getsCallStack,
        throwsCallStack,
        addsCallStack
      ]

getsCallStack :: TestTree
getsCallStack =
  goldenVsStringDiff desc diff gpath $ do
    cs <- runECallStackIO getCallStack
    pure $ fromString $ prettyCallStack cs
  where
    desc = "Retrieves callstack"
    gpath = goldenPath </> "get-callstack.golden"

data Ex = MkEx
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

throwsCallStack :: TestTree
throwsCallStack =
  goldenVsStringDiff desc diff gpath $
    try @SomeException (runECallStackIO $ throwWithCallStack MkEx) <&> \case
      Left e -> fromString $ stableCallStack e
      Right _ -> "Error: did not catch expected exception."
  where
    desc = "Throws with callstack"
    gpath = goldenPath </> "throw-callstack.golden"

addsCallStack :: TestTree
addsCallStack =
  goldenVsStringDiff desc diff gpath $
    try @SomeException
      ( runECallStackIO $
          addCallStack $
            unsafeEff_ $
              throwIO MkEx
      )
      <&> \case
        Left e -> fromString $ stableCallStack e
        Right _ -> "Error: did not catch expected exception."
  where
    desc = "Adds callstack"
    gpath = goldenPath </> "add-callstack.golden"

runECallStackIO :: Eff '[ECallStack, IOE] a -> IO a
runECallStackIO = runEff . runECallStack

goldenPath :: FilePath
goldenPath = "test/unit/"

diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]

stableCallStack :: Exception e => e -> String
stableCallStack = unlines . take 2 . lines . displayCallStack
