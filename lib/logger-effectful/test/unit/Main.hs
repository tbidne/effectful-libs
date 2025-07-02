module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Unit.Effectful.Logger.Dynamic qualified as Logger.Dynamic
import Unit.Effectful.Logger.Namespace qualified as Logger.Namespace

main :: IO ()
main =
  defaultMain $
    testGroup
      "Effectful.Logger"
      [ Logger.Dynamic.tests,
        Logger.Namespace.tests
      ]
