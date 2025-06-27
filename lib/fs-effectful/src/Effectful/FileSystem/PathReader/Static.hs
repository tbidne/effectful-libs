{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides a static effect for the readable portion of "System.Directory"'s
-- interface. For the static interface of the entire "System.Directory"
-- interface, see
-- https://hackage.haskell.org/package/effectful-2.2.2.0/docs/Effectful-FileSystem.html.
--
-- @since 0.1
module Effectful.FileSystem.PathReader.Static
  ( -- * Effect
    PathReader,

    -- ** Functions
    listDirectory,
    getDirectoryContents,
    getCurrentDirectory,
    getHomeDirectory,
    getXdgDirectory,
    getXdgDirectoryList,
    getAppUserDataDirectory,
    getUserDocumentsDirectory,
    getTemporaryDirectory,
    getFileSize,
    canonicalizePath,
    makeAbsolute,
    makeRelativeToCurrentDirectory,
    doesPathExist,
    doesFileExist,
    doesDirectoryExist,
    findExecutable,
    findExecutables,
    findExecutablesInDirectories,
    findFileWith,
    findFilesWith,
    pathIsSymbolicLink,
    getSymbolicLinkTarget,
    getPermissions,
    getAccessTime,
    getModificationTime,

    -- ** Handlers
    runPathReader,

    -- * Functions
    findFile,
    findFiles,

    -- ** XDG Utils
    getXdgData,
    getXdgConfig,
    getXdgCache,
    getXdgState,

    -- * Path Types
    PathType (..),

    -- ** Functions
    PathType.displayPathType,
    getPathType,
    isPathType,
    throwIfWrongPathType,

    -- * Tilde expansion
    expandTilde,
    forExpandedTilde,
    onExpandedTilde,

    -- * Misc
    listDirectoryRecursive,
    listDirectoryRecursiveSymbolicLink,
    doesSymbolicLinkExist,
    pathIsSymbolicDirectoryLink,
    pathIsSymbolicFileLink,

    -- * Re-exports
    OsPath,
    Permissions,
    UTCTime (..),
    XdgDirectory (..),
    XdgDirectoryList (..),
  )
where

import Control.Category ((>>>))
import Control.Monad (unless, (>=>))
import Data.Time (UTCTime (UTCTime, utctDay, utctDayTime))
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( HasCallStack,
    SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    seqUnliftIO,
    unsafeEff,
    unsafeEff_,
  )
import Effectful.Exception (catchIO)
import FileSystem.IO qualified as IO
import FileSystem.OsPath
  ( OsPath,
    OsPathOrEmpty (OsPathEmpty, OsPathNonEmpty),
    TildePrefixState
      ( TildePrefixStateNone,
        TildePrefixStateStripped
      ),
    (</>),
  )
import FileSystem.OsPath qualified as OsP
import FileSystem.PathType
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeOther,
        PathTypeSymbolicLink
      ),
  )
import FileSystem.PathType qualified as PathType
import GHC.IO.Exception (IOErrorType (InappropriateType))
import System.Directory
  ( Permissions,
    XdgDirectory (XdgCache, XdgConfig, XdgData, XdgState),
    XdgDirectoryList (XdgConfigDirs, XdgDataDirs),
  )
import System.Directory.OsPath qualified as Dir
import System.IO.Error qualified as IO.Error

-- | Static effect for reading paths.
--
-- @since 0.1
data PathReader :: Effect

type instance DispatchOf PathReader = Static WithSideEffects

data instance StaticRep PathReader = MkPathReader

-- | Runs an 'PathReader' effect in IO.
--
-- @since 0.1
runPathReader ::
  ( HasCallStack,
    IOE :> es
  ) =>
  Eff (PathReader : es) a ->
  Eff es a
runPathReader = evalStaticRep MkPathReader

-- | Search through the given list of directories for the given file.
--
-- The behavior is equivalent to 'findFileWith', returning only the first
-- occurrence. Details can be found in the documentation of 'findFileWith'.
--
-- @since 0.1
findFile ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  [OsPath] ->
  OsPath ->
  Eff es (Maybe OsPath)
findFile = findFileWith (\_ -> pure True)

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
-- The behavior is equivalent to 'findFilesWith'. Details can be found in the
-- documentation of 'findFilesWith'.
--
-- @since 0.1
findFiles ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  [OsPath] ->
  OsPath ->
  Eff es [OsPath]
findFiles = findFilesWith (\_ -> pure True)

-- | Lifted 'Dir.listDirectory'.
--
-- @since 0.1
listDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es [OsPath]
listDirectory = unsafeEff_ . Dir.listDirectory

-- | Lifted 'Dir.getDirectoryContents'.
--
-- @since 0.1
getDirectoryContents ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es [OsPath]
getDirectoryContents = unsafeEff_ . Dir.getDirectoryContents

-- | Lifted 'Dir.getCurrentDirectory'.
--
-- @since 0.1
getCurrentDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es OsPath
getCurrentDirectory = unsafeEff_ Dir.getCurrentDirectory

-- | Lifted 'Dir.getHomeDirectory'.
--
-- @since 0.1
getHomeDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es OsPath
getHomeDirectory = unsafeEff_ Dir.getHomeDirectory

-- | Lifted 'Dir.getXdgDirectory'.
--
-- @since 0.1
getXdgDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  XdgDirectory ->
  OsPath ->
  Eff es OsPath
getXdgDirectory xdg = unsafeEff_ . Dir.getXdgDirectory xdg

-- | Lifted 'Dir.getXdgDirectoryList'.
--
-- @since 0.1
getXdgDirectoryList ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  XdgDirectoryList ->
  Eff es [OsPath]
getXdgDirectoryList = unsafeEff_ . Dir.getXdgDirectoryList

-- | Lifted 'Dir.getAppUserDataDirectory'.
--
-- @since 0.1
getAppUserDataDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getAppUserDataDirectory = unsafeEff_ . Dir.getAppUserDataDirectory

-- | Lifted 'Dir.getUserDocumentsDirectory'.
--
-- @since 0.1
getUserDocumentsDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es OsPath
getUserDocumentsDirectory = unsafeEff_ Dir.getUserDocumentsDirectory

-- | Lifted 'Dir.getTemporaryDirectory'.
--
-- @since 0.1
getTemporaryDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es OsPath
getTemporaryDirectory = unsafeEff_ Dir.getTemporaryDirectory

-- | Lifted 'Dir.getFileSize'.
--
-- @since 0.1
getFileSize ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Integer
getFileSize = unsafeEff_ . Dir.getFileSize

-- | Lifted 'Dir.canonicalizePath'.
--
-- @since 0.1
canonicalizePath ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
canonicalizePath = unsafeEff_ . Dir.canonicalizePath

-- | Lifted 'Dir.makeAbsolute'.
--
-- @since 0.1
makeAbsolute ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
makeAbsolute = unsafeEff_ . Dir.makeAbsolute

-- | Lifted 'Dir.makeRelativeToCurrentDirectory'.
--
-- @since 0.1
makeRelativeToCurrentDirectory ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
makeRelativeToCurrentDirectory = unsafeEff_ . Dir.makeRelativeToCurrentDirectory

-- | Lifted 'Dir.doesPathExist'.
--
-- @since 0.1
doesPathExist ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
doesPathExist = unsafeEff_ . Dir.doesPathExist

-- | Lifted 'Dir.doesFileExist'.
--
-- @since 0.1
doesFileExist ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
doesFileExist = unsafeEff_ . Dir.doesFileExist

-- | Lifted 'Dir.doesDirectoryExist'.
--
-- @since 0.1
doesDirectoryExist ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
doesDirectoryExist = unsafeEff_ . Dir.doesDirectoryExist

-- | Lifted 'Dir.findExecutable'.
--
-- @since 0.1
findExecutable ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es (Maybe OsPath)
findExecutable = unsafeEff_ . Dir.findExecutable

-- | Lifted 'Dir.findExecutables'.
--
-- @since 0.1
findExecutables ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es [OsPath]
findExecutables = unsafeEff_ . Dir.findExecutables

-- | Lifted 'Dir.findExecutablesInDirectories'.
--
-- @since 0.1
findExecutablesInDirectories ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  [OsPath] ->
  OsPath ->
  Eff es [OsPath]
findExecutablesInDirectories ps =
  unsafeEff_ . Dir.findExecutablesInDirectories ps

-- | Lifted 'Dir.findFileWith'.
--
-- @since 0.1
findFileWith ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  (OsPath -> Eff es Bool) ->
  [OsPath] ->
  OsPath ->
  Eff es (Maybe OsPath)
findFileWith f ps s =
  unsafeEff $ \env -> seqUnliftIO env $
    \runInIO -> Dir.findFileWith (runInIO . f) ps s

-- | Lifted 'Dir.findFilesWith'.
--
-- @since 0.1
findFilesWith ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  (OsPath -> Eff es Bool) ->
  [OsPath] ->
  OsPath ->
  Eff es [OsPath]
findFilesWith f ps s =
  unsafeEff $ \env -> seqUnliftIO env $
    \runInIO -> Dir.findFilesWith (runInIO . f) ps s

-- | Lifted 'Dir.pathIsSymbolicLink'.
--
-- @since 0.1
pathIsSymbolicLink ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
pathIsSymbolicLink = unsafeEff_ . Dir.pathIsSymbolicLink

-- | Lifted 'Dir.getSymbolicLinkTarget'.
--
-- @since 0.1
getSymbolicLinkTarget ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getSymbolicLinkTarget = unsafeEff_ . Dir.getSymbolicLinkTarget

-- | Lifted 'Dir.getPermissions'.
--
-- @since 0.1
getPermissions ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Permissions
getPermissions = unsafeEff_ . Dir.getPermissions

-- | Lifted 'Dir.getAccessTime'.
--
-- @since 0.1
getAccessTime ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es UTCTime
getAccessTime = unsafeEff_ . Dir.getAccessTime

-- | Lifted 'Dir.getModificationTime'.
--
-- @since 0.1
getModificationTime ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es UTCTime
getModificationTime = unsafeEff_ . Dir.getModificationTime

-- | Retrieves the XDG data directory e.g. @~/.local\/share@.
--
-- @since 0.1
getXdgData ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgData = getXdgDirectory XdgData

-- | Retrieves the XDG config directory e.g. @~/.config@.
--
-- @since 0.1
getXdgConfig ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgConfig = getXdgDirectory XdgConfig

-- | Retrieves the XDG cache directory e.g. @~/.cache@.
--
-- @since 0.1
getXdgCache ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgCache = getXdgDirectory XdgCache

-- | Retrieves the XDG state directory e.g. @~/.local\/state@.
--
-- @since 0.1
getXdgState ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es OsPath
getXdgState = getXdgDirectory XdgState

-- | Returns true if the path is a symbolic link. Does not traverse the link.
--
-- @since 0.1
doesSymbolicLinkExist ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
doesSymbolicLinkExist p =
  -- pathIsSymbolicLink throws an exception if the path does not exist,
  -- so we need to handle this. Note that the obvious alternative, prefacing
  -- the call with doesPathExist does not work, as that operates on the link
  -- target. doesFileExist also behaves this way.
  pathIsSymbolicLink p `catchIO` \_ -> pure False

-- | Retrieves the recursive directory contents; splits the sub folders and
-- directories apart.
--
-- @since 0.1
listDirectoryRecursive ::
  forall es.
  ( HasCallStack,
    PathReader :> es
  ) =>
  -- | Root path.
  OsPath ->
  -- | (files, directories)
  Eff es ([OsPath], [OsPath])
listDirectoryRecursive root = recurseDirs [emptyPath]
  where
    recurseDirs :: [OsPath] -> Eff es ([OsPath], [OsPath])
    recurseDirs [] = pure ([], [])
    recurseDirs (d : ds) = do
      (files, dirs) <- splitPaths root d [] [] =<< listDirectory (root </> d)
      (files', dirs') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs')
    emptyPath = mempty

-- | Like 'listDirectoryRecursive' except symbolic links are not traversed
-- i.e. they are returned separately.
--
-- @since 0.1
listDirectoryRecursiveSymbolicLink ::
  forall es.
  ( HasCallStack,
    PathReader :> es
  ) =>
  -- | Root path.
  OsPath ->
  -- | (files, directories, symbolic links)
  Eff es ([OsPath], [OsPath], [OsPath])
listDirectoryRecursiveSymbolicLink root = recurseDirs [emptyPath]
  where
    recurseDirs :: [OsPath] -> Eff es ([OsPath], [OsPath], [OsPath])
    recurseDirs [] = pure ([], [], [])
    recurseDirs (d : ds) = do
      (files, dirs, symlinks) <-
        splitPathsSymboliclink root d [] [] [] =<< listDirectory (root </> d)
      (files', dirs', symlinks') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs', symlinks ++ symlinks')
    emptyPath = mempty

splitPaths ::
  forall es.
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  OsPath ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  Eff es ([OsPath], [OsPath])
splitPaths root d = go
  where
    go :: [OsPath] -> [OsPath] -> [OsPath] -> Eff es ([OsPath], [OsPath])
    go files dirs [] = pure (reverse files, reverse dirs)
    go files dirs (p : ps) = do
      let dirEntry = d </> p
      isDir <- doesDirectoryExist (root </> dirEntry)
      if isDir
        then go files (dirEntry : dirs) ps
        else go (dirEntry : files) dirs ps

splitPathsSymboliclink ::
  forall es.
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  OsPath ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  Eff es ([OsPath], [OsPath], [OsPath])
splitPathsSymboliclink root d = go
  where
    go :: [OsPath] -> [OsPath] -> [OsPath] -> [OsPath] -> Eff es ([OsPath], [OsPath], [OsPath])
    go files dirs symlinks [] = pure (reverse files, reverse dirs, symlinks)
    go files dirs symlinks (p : ps) = do
      let dirEntry = d </> p
          fullPath = root </> dirEntry

      isSymlink <- doesSymbolicLinkExist fullPath
      if isSymlink
        then go files dirs (dirEntry : symlinks) ps
        else do
          isDir <- doesDirectoryExist fullPath
          if isDir
            then go files (dirEntry : dirs) symlinks ps
            else go (dirEntry : files) dirs symlinks ps

-- | Like 'pathIsSymbolicDirectoryLink' but for files.
--
-- @since 0.1
pathIsSymbolicFileLink ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
pathIsSymbolicFileLink = getSymbolicLinkTarget >=> doesFileExist

-- | Returns true if @p@ is a symbolic link and it points to an extant
-- directory. Throws an exception if the path is not a symbolic link or the
-- target does not exist.
--
-- This function and 'pathIsSymbolicFileLink' are intended to distinguish file
-- and directory links on Windows. This matters for knowing when to use:
--
--     - @createFileLink@ vs. @createDirectoryLink@
--     - @removeFile@ vs. @removeDirectoryLink@
--
-- Suppose we want to copy an arbitrary path @p@. We first determine that
-- @p@ is a symlink via 'doesSymbolicLinkExist'. If
-- 'pathIsSymbolicDirectoryLink' returns true then we know we should use
-- "Effects.FileSystem.PathWriter"'s @createDirectoryLink@. Otherwise we can
-- fall back to @createFileLink@.
--
-- Because this relies on the symlink's target, this is best effort, and it is
-- possible 'pathIsSymbolicDirectoryLink' and 'pathIsSymbolicFileLink' both
-- return false.
--
-- Note that Posix makes no distinction between file and directory symbolic
-- links. Thus if your system only has to work on Posix, you probably don't
-- need this function.
--
-- @since 0.1
pathIsSymbolicDirectoryLink ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es Bool
pathIsSymbolicDirectoryLink = getSymbolicLinkTarget >=> doesDirectoryExist

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- @since 0.1
throwIfWrongPathType ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  -- | The location for the thrown exception (e.g. function name)
  String ->
  -- | Expected path type
  PathType ->
  -- | Path
  OsPath ->
  Eff es ()
throwIfWrongPathType location expected path = do
  actual <- getPathType path

  let err =
        mconcat
          [ "Expected path to have type ",
            PathType.displayPathType expected,
            ", but detected ",
            PathType.displayPathType actual
          ]

  unless (expected == actual) $
    IO.throwPathIOError
      path
      location
      InappropriateType
      err

-- | Checks that the path type matches the expectation. Throws
-- 'IOException' if the path does not exist or the type cannot be detected.
--
-- @since 0.1
isPathType ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  -- | Expected path type.
  PathType ->
  -- Path.
  OsPath ->
  Eff es Bool
isPathType expected = fmap (== expected) . getPathType

-- | Returns the type for a given path without following symlinks.
-- Throws 'IOException' if the path does not exist or the type cannot be
-- detected.
--
-- @since 0.1
getPathType ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es PathType
getPathType path = do
  -- This needs to be first as does(Directory|File|Path)Exist acts on the target.
  symlinkExists <- doesSymbolicLinkExist path
  if symlinkExists
    then pure PathTypeSymbolicLink
    else do
      dirExists <- doesDirectoryExist path
      if dirExists
        then pure PathTypeDirectory
        else do
          fileExists <- doesFileExist path
          if fileExists
            then pure PathTypeFile
            else do
              pathExists <- doesPathExist path
              if pathExists
                then pure PathTypeOther
                else
                  IO.throwPathIOError
                    path
                    "getPathType"
                    IO.Error.doesNotExistErrorType
                    "path does not exist"

-- | 'onExpandedTilde' that simply returns the expanded path.
--
-- @since 0.1
expandTilde ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  -- | Path to potentially expand.
  OsPath ->
  Eff es OsPath
expandTilde = onExpandedTilde pure

-- | Flipped 'onExpandedTilde'.
--
-- @since 0.1
forExpandedTilde ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  -- | Path to potentially expand.
  OsPath ->
  -- | Action to run on the expanded path.
  (OsPath -> Eff es a) ->
  Eff es a
forExpandedTilde = flip onExpandedTilde

-- | Expands a "tilde prefix" (~) with the home directory, running the
-- action on the result. Throws an exception if the 'OsPath' contains any
-- other tildes i.e. the only expansions we allow are:
--
-- - @"~/..."@
-- - @"~"@
-- - @"~\\..."@ (windows only)
--
-- If the path contains no tildes, it is handled normally.
--
-- @since 0.1
onExpandedTilde ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  -- | Action to run on the expanded path.
  (OsPath -> Eff es a) ->
  -- | Path to potentially expand.
  OsPath ->
  Eff es a
onExpandedTilde onPath =
  OsP.toTildePrefixState >>> \case
    TildePrefixStateNone p -> onPath p
    TildePrefixStateStripped pne ->
      getHomeDirectory >>= \d -> case pne of
        OsPathEmpty -> onPath d
        OsPathNonEmpty p -> onPath $ d </> p
