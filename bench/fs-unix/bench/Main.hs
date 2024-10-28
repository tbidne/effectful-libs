{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- "Unused" import FilePath here so we can set the transitive dep version,
-- same as effects-unix.
--
-- See NOTE: [Unused FilePath].

module Main (main) where

import Effectful
  ( Eff,
    IOE,
    runEff,
    type (:>),
  )
import Effectful.FileSystem.PathReader.Static (PathReaderStatic)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.Posix.Static (PosixStatic)
import Effectful.Posix.Static qualified as P
import Effectful.PosixCompat.Static (PosixCompatStatic)
import Effectful.PosixCompat.Static qualified as PC
import FileSystem.OsPath (OsPath, combineFilePaths, osp, (</>))
import System.FilePath qualified
import System.OsString.Internal.Types (OsString (getOsString), PosixString)
import Test.Tasty.Bench
  ( bench,
    bgroup,
    defaultMain,
    nfIO,
  )

main :: IO ()
main =
  defaultMain
    [ getPathType
    ]
  where
    getPathType =
      bgroup
        "getPathType"
        [ getFile,
          getDir,
          getFileLink,
          getDirLink
        ]

    getFile =
      bgroup
        "get file"
        [ bench "PathReader: get file" $ nfIO $ runEffPathReader $ PR.getPathType fileOs,
          bench "Posix: get file" $ nfIO $ runEffPosix $ P.getPathType filePosix,
          bench "PosixCompat: get file" $ nfIO $ runEffPosixCompat $ PC.getPathType fileFp
        ]

    getDir =
      bgroup
        "get dir"
        [ bench "PathReader: get dir" $ nfIO $ runEffPathReader $ PR.getPathType dirOs,
          bench "Posix: get dir" $ nfIO $ runEffPosix $ P.getPathType dirPosix,
          bench "PosixCompat: get dir" $ nfIO $ runEffPosixCompat $ PC.getPathType dirFp
        ]

    getFileLink =
      bgroup
        "get file link"
        [ bench "PathReader: get file link" $ nfIO $ runEffPathReader $ PR.getPathType fileLinkOs,
          bench "Posix: get file link" $ nfIO $ runEffPosix $ P.getPathType fileLinkPosix,
          bench "PosixCompat: get file link" $ nfIO $ runEffPosixCompat $ PC.getPathType fileLinkFp
        ]

    getDirLink =
      bgroup
        "get dir link"
        [ bench "PathReader: get dir link" $ nfIO $ runEffPathReader $ PR.getPathType dirLinkOs,
          bench "Posix: get dir link" $ nfIO $ runEffPosix $ P.getPathType dirLinkPosix,
          bench "PosixCompat: get dir link" $ nfIO $ runEffPosixCompat $ PC.getPathType dirLinkFp
        ]

fileFp :: FilePath
fileFp = "bench" `cfp` "data" `cfp` "file"

fileOs :: OsPath
fileOs = [osp|bench|] </> [osp|data|] </> [osp|file|]

filePosix :: PosixString
filePosix = fileOs.getOsString

dirFp :: FilePath
dirFp = "bench" `cfp` "data" `cfp` "dir"

dirOs :: OsPath
dirOs = [osp|bench|] </> [osp|data|] </> [osp|dir|]

dirPosix :: PosixString
dirPosix = dirOs.getOsString

fileLinkFp :: FilePath
fileLinkFp = "bench" `cfp` "data" `cfp` "file-link"

fileLinkOs :: OsPath
fileLinkOs = [osp|bench|] </> [osp|data|] </> [osp|file-link|]

fileLinkPosix :: PosixString
fileLinkPosix = fileLinkOs.getOsString

dirLinkFp :: FilePath
dirLinkFp = "bench" `cfp` "data" `cfp` "dir-link"

dirLinkOs :: OsPath
dirLinkOs = [osp|bench|] </> [osp|data|] </> [osp|dir-link|]

dirLinkPosix :: PosixString
dirLinkPosix = dirLinkOs.getOsString

cfp :: FilePath -> FilePath -> FilePath
cfp = combineFilePaths

runEffPathReader :: Eff '[PathReaderStatic, IOE] a -> IO a
runEffPathReader = runEff . PR.runPathReaderStaticIO

runEffPosix :: Eff '[PosixStatic, IOE] a -> IO a
runEffPosix = runEff . P.runPosixStaticIO

runEffPosixCompat :: Eff '[PosixCompatStatic, IOE] a -> IO a
runEffPosixCompat = runEff . PC.runPosixCompatStaticIO
