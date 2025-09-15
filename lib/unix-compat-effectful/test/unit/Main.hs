module Main (main) where

import Control.Monad (when)
import PosixCompat.Files.Dynamic qualified
import PosixCompat.Files.Static qualified
import System.Directory qualified as Dir
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.FilePath ((</>))
import Test.Tasty (defaultMain, testGroup, withResource)

main :: IO ()
main =
  defaultMain $
    withResource setup teardown $ \dirs ->
      testGroup
        "Unit Tests"
        [ PosixCompat.Files.Dynamic.tests ((\(_, d, _) -> d) <$> dirs),
          PosixCompat.Files.Static.tests ((\(_, _, s) -> s) <$> dirs)
        ]

-- NOTE: FilePath and not OsPath as the unix-compat API uses FilePath.

setup :: IO (FilePath, FilePath, FilePath)
setup = do
  tmpDir <-
    (\s -> s </> "unix-compat-effectful" </> "unit")
      <$> Dir.getTemporaryDirectory

  let (dynamicDir, staticDir) = (tmpDir </> "dynamic", tmpDir </> "static")

  dexists <- Dir.doesDirectoryExist dynamicDir
  when dexists (Dir.removeDirectoryRecursive dynamicDir)
  Dir.createDirectoryIfMissing True dynamicDir

  sexists <- Dir.doesDirectoryExist staticDir
  when sexists (Dir.removeDirectoryRecursive staticDir)
  Dir.createDirectoryIfMissing True staticDir

  pure (tmpDir, dynamicDir, staticDir)

teardown :: (FilePath, FilePath, FilePath) -> IO ()
teardown (rootDir, _, _) = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = Dir.removePathForcibly rootDir
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> show rootDir
