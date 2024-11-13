{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Foldable (for_)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.FileSystem.FileWriter.Static (FileWriter)
import Effectful.FileSystem.FileWriter.Static qualified as FW
import Effectful.FileSystem.PathReader.Static (PathReader)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static (PathWriter)
import Effectful.FileSystem.PathWriter.Static qualified as PW
import FileSystem.OsPath (OsPath, osp, (</>))
import FileSystem.OsPath qualified as FS.OsPath
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty (defaultMain, testGroup, withResource)
import Unit.PathReader qualified
import Unit.PathWriter qualified

main :: IO ()
main =
  defaultMain $
    withResource setup teardown $ \args ->
      testGroup
        "Unit Tests"
        [ Unit.PathReader.tests args,
          Unit.PathWriter.tests args
        ]

setup :: IO OsPath
setup = runEffectsIO $ do
  tmpDir <-
    (\s -> s </> [osp|fs-effectful|] </> [osp|unit|])
      <$> PR.getTemporaryDirectory
  PW.removeDirectoryRecursiveIfExists_ tmpDir
  PW.createDirectoryIfMissing True tmpDir

  createDataDir tmpDir

  pure tmpDir

teardown :: OsPath -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = runEffectsIO $ PW.removePathForcibly fp
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> FS.OsPath.unsafeDecode fp

-- | This is what we want to create:
--
-- @
-- λ. tree -a
-- .
-- ├── bar
-- ├── baz
-- ├── dir1
-- │   └── f
-- ├── dir2
-- │   └── f
-- ├── dir3
-- │   ├── dir3.1
-- │   │   └── f
-- │   └── f
-- ├── foo
-- ├── .hidden
-- │   └── f1
-- ├── l1 -> foo
-- ├── l2 -> dir2
-- └── l3 -> bad
--
-- 7 directories, 10 files
-- @
--
-- Originally we actually had this test data directory committed, but
-- unfortunately the bad sym link (l3 -> bad, which we want!) caused stack
-- to die when checking out this repo during a build. Thus we build the
-- needed directory during the test itself.
createDataDir ::
  ( FileWriter :> es,
    PathReader :> es,
    PathWriter :> es
  ) =>
  OsPath ->
  Eff es ()
createDataDir tmpDir = do
  PW.removeDirectoryIfExists_ dataDir
  PW.createDirectoryIfMissing True dataDir

  createDirs
    [ [osp|dir1|],
      [osp|dir2|],
      [osp|dir3|],
      [osp|dir3|] </> [osp|dir3.1|],
      [osp|.hidden|]
    ]

  createFiles
    [ [osp|bar|],
      [osp|baz|],
      [osp|dir1|] </> [osp|f|],
      [osp|dir2|] </> [osp|f|],
      [osp|dir3|] </> [osp|f|],
      [osp|dir3|] </> [osp|dir3.1|] </> [osp|f|],
      [osp|foo|],
      [osp|.hidden|] </> [osp|f1|]
    ]

  createSymlinks
    [ ([osp|l1|], [osp|foo|], False),
      ([osp|l2|], [osp|dir2|], True),
      ([osp|l3|], [osp|bad|], False)
    ]
  where
    dataDir = tmpDir </> [osp|data|]

    createDirs ds =
      for_ ds $ \p -> PW.createDirectory (dataDir </> p)

    createFiles ps =
      for_ ps $ \p -> FW.writeBinaryFile (dataDir </> p) ""

    createSymlinks ls =
      for_ ls $ \(n, t, isDir) ->
        if isDir
          then PW.createDirectoryLink (dataDir </> t) (dataDir </> n)
          else PW.createFileLink (dataDir </> t) (dataDir </> n)

runEffectsIO :: Eff [PathWriter, PathReader, FileWriter, IOE] a -> IO a
runEffectsIO =
  runEff
    . FW.runFileWriter
    . PR.runPathReader
    . PW.runPathWriter
