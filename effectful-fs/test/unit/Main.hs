module Main (main) where

import Effectful (Eff, IOE, runEff)
import Effectful.FileSystem.Path (Path, (</>))
import Effectful.FileSystem.PathReader (PathReaderEffect, getTemporaryDirectory, runPathReaderIO)
import Effectful.FileSystem.PathWriter
  ( PathWriterEffect,
    createDirectoryIfMissing,
    removeDirectoryRecursiveIfExists,
    removePathForcibly,
    runPathWriterIO,
  )
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
        [ PathReader.tests,
          PathWriter.tests args
        ]

setup :: IO Path
setup = do
  tmpDir <-
    (\s -> s </> U.strToPath "effects-fs" </> U.strToPath "unit")
      <$> runPathIO getTemporaryDirectory

  runPathIO $ do
    removeDirectoryRecursiveIfExists tmpDir
    createDirectoryIfMissing True tmpDir
  pure tmpDir

teardown :: Path -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = runPathIO $ removePathForcibly fp
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> U.pathToStr fp

runPathIO :: Eff [PathWriterEffect, PathReaderEffect, IOE] a -> IO a
runPathIO = runEff . runPathReaderIO . runPathWriterIO
