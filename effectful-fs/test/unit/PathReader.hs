{-# LANGUAGE CPP #-}

module PathReader (tests) where

import Control.Monad (zipWithM_)
import Data.List qualified as L
import Effectful (Eff, IOE, runEff)
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReaderDynamic,
    runPathReaderDynamicIO,
  )
import Effectful.FileSystem.PathReader.Dynamic qualified as PathReader
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Utils qualified as U

tests :: TestTree
tests =
  testGroup
    "PathReader"
    [ testListDirectoryRecursive
    ]

testListDirectoryRecursive :: TestTree
testListDirectoryRecursive = testCase "Recursively lists sub-files/dirs" $ do
  (files, dirs) <-
    runEffPathReader $
      PathReader.listDirectoryRecursive (U.strToPath "./src")
  let (files', dirs') = (L.sort files, L.sort dirs)
  zipWithM_ (@=?) expectedFiles (U.pathToStr <$> files')
  zipWithM_ (@=?) expectedDirs (U.pathToStr <$> dirs')
  where
#if !WINDOWS
    expectedFiles =
      [ "Effectful/FileSystem/FileReader/Dynamic.hs",
        "Effectful/FileSystem/FileWriter/Dynamic.hs",
        "Effectful/FileSystem/HandleReader/Dynamic.hs",
        "Effectful/FileSystem/HandleWriter/Dynamic.hs",
        "Effectful/FileSystem/Internal.hs",
        "Effectful/FileSystem/Path.hs",
        "Effectful/FileSystem/PathReader/Dynamic.hs",
        "Effectful/FileSystem/PathWriter/Dynamic.hs"
      ]
    expectedDirs =
      [ "Effectful",
        "Effectful/FileSystem"
      ]
#else
    expectedFiles =
      [ "Effectful\\FileSystem\\FileReader\\Dynamic.hs",
        "Effectful\\FileSystem\\FileWriter\\Dynamic.hs",
        "Effectful\\FileSystem\\HandleReader\\Dynamic.hs",
        "Effectful\\FileSystem\\HandleWriter\\Dynamic.hs",
        "Effectful\\FileSystem\\Internal.hs",
        "Effectful\\FileSystem\\Path.hs",
        "Effectful\\FileSystem\\PathReader\\Dynamic.hs",
        "Effectful\\FileSystem\\PathWriter\\Dynamic.hs"
      ]
    expectedDirs =
      [ "Effectful",
        "Effectful\\FileSystem"
      ]
#endif

runEffPathReader :: Eff '[PathReaderDynamic, IOE] a -> IO a
runEffPathReader = runEff . runPathReaderDynamicIO
