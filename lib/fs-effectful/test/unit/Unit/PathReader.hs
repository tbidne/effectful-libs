{-# LANGUAGE QuasiQuotes #-}

module Unit.PathReader (tests) where

import Data.List qualified as L
import Effectful (Eff, IOE, runEff)
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReaderDynamic,
    runPathReaderDynamicIO,
  )
import Effectful.FileSystem.PathReader.Dynamic qualified as PathReader
import FileSystem.OsPath (OsPath, osp, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: IO OsPath -> TestTree
tests getTmpDir =
  testGroup
    "PathReader"
    [ testListDirectoryRecursive,
      testListDirectoryRecursiveSymlinkTargets getTmpDir,
      testListDirectoryRecursiveSymbolicLink getTmpDir
    ]

testListDirectoryRecursive :: TestTree
testListDirectoryRecursive = testCase "Recursively lists sub-files/dirs" $ do
  (files, dirs) <-
    runEffPathReader $ PathReader.listDirectoryRecursive [osp|src|]
  let (files', dirs') = (L.sort files, L.sort dirs)
  expectedFiles @=? files'
  expectedDirs @=? dirs'
  where
    expectedFiles =
      [ [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileReader|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileReader|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileWriter|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileWriter|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleReader|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleReader|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleWriter|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleWriter|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathReader|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathReader|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathWriter|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathWriter|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathWriter|] </> [osp|Utils.hs|]
      ]
    expectedDirs =
      [ [osp|Effectful|],
        [osp|Effectful|] </> [osp|FileSystem|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileReader|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileWriter|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleReader|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleWriter|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathReader|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathWriter|]
      ]

testListDirectoryRecursiveSymlinkTargets :: IO OsPath -> TestTree
testListDirectoryRecursiveSymlinkTargets getTmpDir = testCase desc $ do
  tmpDir <- getTmpDir
  let dataDir = tmpDir </> [osp|data|]

  (files, dirs) <-
    runEffPathReader $ PathReader.listDirectoryRecursive dataDir
  let (files', dirs') = (L.sort files, L.sort dirs)

  expectedFiles @=? files'
  expectedDirs @=? dirs'
  where
    desc = "Symlinks are categorized via targets"
    expectedFiles =
      [ [osp|.hidden|] </> [osp|f1|],
        [osp|bar|],
        [osp|baz|],
        [osp|dir1|] </> [osp|f|],
        [osp|dir2|] </> [osp|f|],
        [osp|dir3|] </> [osp|dir3.1|] </> [osp|f|],
        [osp|dir3|] </> [osp|f|],
        [osp|foo|],
        [osp|l1|],
        [osp|l2|] </> [osp|f|],
        [osp|l3|]
      ]
    expectedDirs =
      [ [osp|.hidden|],
        [osp|dir1|],
        [osp|dir2|],
        [osp|dir3|],
        [osp|dir3|] </> [osp|dir3.1|],
        [osp|l2|]
      ]

testListDirectoryRecursiveSymbolicLink :: IO OsPath -> TestTree
testListDirectoryRecursiveSymbolicLink getTmpDir = testCase desc $ do
  tmpDir <- getTmpDir
  let dataDir = tmpDir </> [osp|data|]

  (files, dirs, symlinks) <-
    runEffPathReader $ PathReader.listDirectoryRecursiveSymbolicLink dataDir
  let (files', dirs', symlinks') = (L.sort files, L.sort dirs, L.sort symlinks)

  expectedFiles @=? files'
  expectedDirs @=? dirs'
  expectedSymlinks @=? symlinks'
  where
    desc = "Recursively lists sub-files/dirs/symlinks"
    expectedFiles =
      [ [osp|.hidden|] </> [osp|f1|],
        [osp|bar|],
        [osp|baz|],
        [osp|dir1|] </> [osp|f|],
        [osp|dir2|] </> [osp|f|],
        [osp|dir3|] </> [osp|dir3.1|] </> [osp|f|],
        [osp|dir3|] </> [osp|f|],
        [osp|foo|]
      ]
    expectedDirs =
      [ [osp|.hidden|],
        [osp|dir1|],
        [osp|dir2|],
        [osp|dir3|],
        [osp|dir3|] </> [osp|dir3.1|]
      ]
    expectedSymlinks =
      [ [osp|l1|],
        [osp|l2|],
        [osp|l3|]
      ]

runEffPathReader :: Eff '[PathReaderDynamic, IOE] a -> IO a
runEffPathReader = runEff . runPathReaderDynamicIO
