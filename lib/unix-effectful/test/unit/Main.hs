{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad (when)
import FileSystem.OsPath (OsPath, osp, (</>))
import Posix.Dynamic qualified
import Posix.Static qualified
import System.Directory.OsPath qualified as Dir
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty (defaultMain, testGroup, withResource)

main :: IO ()
main =
  defaultMain $
    withResource setup teardown $ \dirs ->
      testGroup
        "Unit Tests"
        [ Posix.Dynamic.tests ((\(_, d, _) -> d) <$> dirs),
          Posix.Static.tests ((\(_, _, s) -> s) <$> dirs)
        ]

setup :: IO (OsPath, OsPath, OsPath)
setup = do
  tmpDir <-
    (\s -> s </> [osp|unix-effectful|] </> [osp|unit|])
      <$> Dir.getTemporaryDirectory

  let (dynamicDir, staticDir) = (tmpDir </> [osp|dynamic|], tmpDir </> [osp|static|])

  dexists <- Dir.doesDirectoryExist dynamicDir
  when dexists (Dir.removeDirectoryRecursive dynamicDir)
  Dir.createDirectoryIfMissing True dynamicDir

  sexists <- Dir.doesDirectoryExist staticDir
  when sexists (Dir.removeDirectoryRecursive staticDir)
  Dir.createDirectoryIfMissing True staticDir

  pure (tmpDir, dynamicDir, staticDir)

teardown :: (OsPath, OsPath, OsPath) -> IO ()
teardown (rootDir, _, _) = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = Dir.removePathForcibly rootDir
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> show rootDir
