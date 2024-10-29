{-# LANGUAGE QuasiQuotes #-}

module Posix.Dynamic (tests) where

import Control.Exception.Utils (trySync)
import Effectful (Eff, IOE, runEff)
import Effectful.Posix.Dynamic
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeSymbolicLink
      ),
    Posix,
  )
import Effectful.Posix.Dynamic qualified as PC
import FileSystem.IO qualified as IO
import FileSystem.OsPath (OsPath, osp, (</>))
import System.Directory.OsPath qualified as Dir
import System.OsString.Internal.Types (OsString (getOsString))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))

tests :: IO OsPath -> TestTree
tests getTmpDir =
  testGroup
    "Posix.Dynamic"
    [ pathTypeTests getTmpDir
    ]

pathTypeTests :: IO OsPath -> TestTree
pathTypeTests getTestDir =
  testGroup
    "PathType"
    [ getPathTypeSymlink getTestDir,
      getPathTypeDirectory getTestDir,
      getPathTypeFile getTestDir,
      getPathTypeBad getTestDir
    ]

getPathTypeSymlink :: IO OsPath -> TestTree
getPathTypeSymlink getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|getPathTypeSymlink|]

  let link1 = (testDir </> [osp|file-link|]).getOsString
      link2 = (testDir </> [osp|dir-link|]).getOsString

  -- getPathType
  pathType1 <- runEffPosix $ PC.getPathType link1
  PathTypeSymbolicLink @=? pathType1

  -- isPathType
  isSymlink <- runEffPosix $ PC.isPathType PathTypeSymbolicLink link1
  assertBool "Should be a symlink" isSymlink

  isDirectory <- runEffPosix $ PC.isPathType PathTypeDirectory link1
  assertBool "Should not be a directory" (not isDirectory)

  isFile <- runEffPosix $ PC.isPathType PathTypeFile link1
  assertBool "Should not be a file" (not isFile)

  -- throwIfWrongPathType
  throwHelper PathTypeSymbolicLink link1
  throwIfNoEx $ throwHelper PathTypeDirectory link1
  throwIfNoEx $ throwHelper PathTypeFile link1

  -- getPathType
  pathType2 <- runEffPosix $ PC.getPathType (testDir </> [osp|dir-link|]).getOsString
  PathTypeSymbolicLink @=? pathType2

  -- isPathType
  isSymlink2 <- runEffPosix $ PC.isPathType PathTypeSymbolicLink link2
  assertBool "Should be a symlink" isSymlink2

  isDirectory2 <- runEffPosix $ PC.isPathType PathTypeDirectory link2
  assertBool "Should not be a directory" (not isDirectory2)

  isFile2 <- runEffPosix $ PC.isPathType PathTypeFile link2
  assertBool "Should not be a file" (not isFile2)

  -- throwIfWrongPathType
  throwHelper PathTypeSymbolicLink link2
  throwIfNoEx $ throwHelper PathTypeDirectory link2
  throwIfNoEx $ throwHelper PathTypeFile link2
  where
    desc = "getPathType recognizes symlinks"
    throwHelper x = runEffPosix . PC.throwIfWrongPathType "getPathTypeSymlink" x

getPathTypeDirectory :: IO OsPath -> TestTree
getPathTypeDirectory getTestDir = testCase desc $ do
  testDir <- (.getOsString) <$> setupLinks getTestDir [osp|getPathTypeDirectory|]

  -- getPathType
  pathType <- runEffPosix $ PC.getPathType testDir
  PathTypeDirectory @=? pathType

  -- isPathType
  isSymlink <- runEffPosix $ PC.isPathType PathTypeSymbolicLink testDir
  assertBool "Should not be a symlink" (not isSymlink)

  isDirectory <- runEffPosix $ PC.isPathType PathTypeDirectory testDir
  assertBool "Should be a directory" isDirectory

  isFile <- runEffPosix $ PC.isPathType PathTypeFile testDir
  assertBool "Should not be a file" (not isFile)

  -- throwIfWrongPathType
  throwIfNoEx $ throwHelper PathTypeSymbolicLink testDir
  throwHelper PathTypeDirectory testDir
  throwIfNoEx $ throwHelper PathTypeFile testDir
  where
    desc = "getPathType recognizes directories"
    throwHelper x = runEffPosix . PC.throwIfWrongPathType "getPathTypeDirectory" x

getPathTypeFile :: IO OsPath -> TestTree
getPathTypeFile getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|getPathTypeFile|]
  let path = (testDir </> [osp|file|]).getOsString

  -- getPathType
  pathType <- runEffPosix $ PC.getPathType path
  PathTypeFile @=? pathType

  -- isPathType
  isSymlink <- runEffPosix $ PC.isPathType PathTypeSymbolicLink path
  assertBool "Should not be a symlink" (not isSymlink)

  isDirectory <- runEffPosix $ PC.isPathType PathTypeDirectory path
  assertBool "Should not be a directory" (not isDirectory)

  isFile <- runEffPosix $ PC.isPathType PathTypeFile path
  assertBool "Should be a file" isFile

  -- throwIfWrongPathType
  throwIfNoEx $ throwHelper PathTypeSymbolicLink path
  throwIfNoEx $ throwHelper PathTypeDirectory path
  runEffPosix $ PC.throwIfWrongPathType "" PathTypeFile path
  where
    desc = "getPathType recognizes files"
    throwHelper x = runEffPosix . PC.throwIfWrongPathType "getPathTypeFile" x

getPathTypeBad :: IO OsPath -> TestTree
getPathTypeBad getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|getPathTypeBad|]

  eResult <- runEffPosix $ trySync $ PC.getPathType (testDir </> [osp|bad file|]).getOsString

  case eResult of
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"
  where
    desc = "getPathType throws exception for non-extant path"

setupLinks :: IO OsPath -> OsPath -> IO OsPath
setupLinks getTestDir suffix = do
  testDir <- (\t -> t </> [osp|path-reader|] </> suffix) <$> getTestDir
  let fileLink = testDir </> [osp|file-link|]
      dirLink = testDir </> [osp|dir-link|]
      file = testDir </> [osp|file|]
      dir = testDir </> [osp|dir|]

  Dir.createDirectoryIfMissing True dir
  IO.writeBinaryFileIO file ""
  Dir.createFileLink file fileLink
  Dir.createDirectoryLink dir dirLink

  pure testDir

throwIfNoEx :: IO a -> IO ()
throwIfNoEx m = do
  trySync m >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"

runEffPosix :: Eff '[Posix, IOE] a -> IO a
runEffPosix = runEff . PC.runPosix
