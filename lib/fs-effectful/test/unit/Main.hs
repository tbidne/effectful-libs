{-# LANGUAGE QuasiQuotes #-}

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
import Effectful.FileSystem.Utils (OsPath, osp, (</>))
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty (defaultMain, testGroup, withResource)
import TestUtils qualified as U
import Unit.Misc qualified
import Unit.PathReader qualified
import Unit.PathWriter qualified

main :: IO ()
main =
  defaultMain $
    withResource setup teardown $ \args ->
      testGroup
        "Unit Tests"
        [ Unit.Misc.tests args,
          Unit.PathReader.tests,
          Unit.PathWriter.tests args
        ]

setup :: IO OsPath
setup = runPathDynamicIO $ do
  tmpDir <-
    (\s -> s </> [osp|fs-effectful|] </> [osp|unit|])
      <$> getTemporaryDirectory
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
