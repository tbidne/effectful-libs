module Main (main) where

import Effectful (Eff, IOE, runEff)
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReaderDynamic,
    getTemporaryDirectory,
    runPathReaderDynamicIO,
  )
import Effectful.FileSystem.PathWriter.Dynamic
  ( PathWriterDynamic,
    createDirectoryIfMissing,
    removeDirectoryRecursiveIfExists,
    removePathForcibly,
    runPathWriterDynamicIO,
  )
import Effectful.FileSystem.Utils (OsPath, (</>))
import Misc qualified
import PathReader qualified
import PathWriter qualified
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty (defaultMain, testGroup, withResource)
import Utils qualified as U

main :: IO ()
main =
  defaultMain $
    withResource setup teardown $ \args ->
      testGroup
        "Unit Tests"
        [ Misc.tests args,
          PathReader.tests,
          PathWriter.tests args
        ]

setup :: IO OsPath
setup = do
  tmpDir <-
    (\s -> s </> U.strToPath "fs-effectful" </> U.strToPath "unit")
      <$> runPathDynamicIO getTemporaryDirectory

  runPathDynamicIO $ do
    removeDirectoryRecursiveIfExists tmpDir
    createDirectoryIfMissing True tmpDir
  pure tmpDir

teardown :: OsPath -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = runPathDynamicIO $ removePathForcibly fp
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> U.pathToStr fp

runPathDynamicIO :: Eff [PathWriterDynamic, PathReaderDynamic, IOE] a -> IO a
runPathDynamicIO = runEff . runPathReaderDynamicIO . runPathWriterDynamicIO
