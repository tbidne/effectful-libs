{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Effectful (runEff)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.Utils (OsPath, combineFilePaths, osp, (</>))
import Effectful.PosixCompat.Static qualified as PC
import Effectful.PosixCompat.Utils (PathType)
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
        [ bench "PathReader: get file" $ nfIO $ runGetPathTypePR fileOs,
          bench "PosixCompat: get file" $ nfIO $ runGetPathTypePC fileFp
        ]

    getDir =
      bgroup
        "get dir"
        [ bench "PathReader: get dir" $ nfIO $ runGetPathTypePR dirOs,
          bench "PosixCompat: get dir" $ nfIO $ runGetPathTypePC dirFp
        ]

    getFileLink =
      bgroup
        "get file link"
        [ bench "PathReader: get file link" $ nfIO $ runGetPathTypePR fileLinkOs,
          bench "PosixCompat: get file link" $ nfIO $ runGetPathTypePC fileLinkFp
        ]

    getDirLink =
      bgroup
        "get dir link"
        [ bench "PathReader: get dir link" $ nfIO $ runGetPathTypePR dirLinkOs,
          bench "PosixCompat: get dir link" $ nfIO $ runGetPathTypePC dirLinkFp
        ]

fileFp :: FilePath
fileFp = "bench" `cfp` "data" `cfp` "file"

fileOs :: OsPath
fileOs = [osp|bench|] </> [osp|data|] </> [osp|file|]

dirFp :: FilePath
dirFp = "bench" `cfp` "data" `cfp` "dir"

dirOs :: OsPath
dirOs = [osp|bench|] </> [osp|data|] </> [osp|dir|]

fileLinkFp :: FilePath
fileLinkFp = "bench" `cfp` "data" `cfp` "file-link"

fileLinkOs :: OsPath
fileLinkOs = [osp|bench|] </> [osp|data|] </> [osp|file-link|]

dirLinkFp :: FilePath
dirLinkFp = "bench" `cfp` "data" `cfp` "dir-link"

dirLinkOs :: OsPath
dirLinkOs = [osp|bench|] </> [osp|data|] </> [osp|dir-link|]

cfp :: FilePath -> FilePath -> FilePath
cfp = combineFilePaths

runGetPathTypePR :: OsPath -> IO PathType
runGetPathTypePR =
  runEff
    . PR.runPathReaderStaticIO
    . PR.getPathType

runGetPathTypePC :: FilePath -> IO PathType
runGetPathTypePC =
  runEff
    . PC.runPosixCompatStaticIO
    . PC.getPathType
