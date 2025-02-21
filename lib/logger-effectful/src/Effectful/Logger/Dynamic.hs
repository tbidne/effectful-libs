{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Dynamic effect for "Control.Monad.Logger".
--
-- @since 0.1
module Effectful.Logger.Dynamic
  ( -- * Effect
    Logger (..),
    loggerLog,

    -- * Types
    LogLevel (..),
    LogLine,
    LogSource,

    -- * Re-export from fast-logger
    LogStr,
    ToLogStr (..),
    fromLogStr,

    -- * TH logging
    logTrace,
    logDebug,
    logInfo,
    logWarn,
    logError,
    logFatal,
    logOther,

    -- * TH logging of showable values
    logTraceSH,
    logDebugSH,
    logInfoSH,
    logWarnSH,
    logErrorSH,
    logFatalSH,
    logOtherSH,

    -- * TH logging with source
    logTraceS,
    logDebugS,
    logInfoS,
    logWarnS,
    logErrorS,
    logFatalS,
    logOtherS,

    -- * TH util
    liftLoc,

    -- * Non-TH logging
    logTraceN,
    logDebugN,
    logInfoN,
    logWarnN,
    logErrorN,
    logFatalN,
    logOtherN,

    -- * Non-TH logging with source
    logWithoutLoc,
    logTraceNS,
    logDebugNS,
    logInfoNS,
    logWarnNS,
    logErrorNS,
    logFatalNS,
    logOtherNS,

    -- * Callstack logging
    logTraceCS,
    logDebugCS,
    logInfoCS,
    logWarnCS,
    logErrorCS,
    logFatalCS,
    logOtherCS,

    -- * utilities for defining your own loggers
    defaultLogStr,
    Loc (..),
    defaultLoc,
    defaultOutput,

    -- * Level checks
    guardLevel,
    shouldLog,

    -- * Optics
    _LevelTrace,
    _LevelInfo,
    _LevelDebug,
    _LevelWarn,
    _LevelError,
    _LevelOther,
    _LevelFatal,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as S8
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (HasCallStack, send)
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import GHC.Generics (Generic)
import GHC.Stack
  ( CallStack,
    SrcLoc
      ( srcLocEndCol,
        srcLocEndLine,
        srcLocFile,
        srcLocModule,
        srcLocPackage,
        srcLocStartCol,
        srcLocStartLine
      ),
    getCallStack,
  )
import Language.Haskell.TH.Syntax
  ( Exp,
    Lift (lift, liftTyped),
    Loc
      ( Loc,
        loc_end,
        loc_filename,
        loc_module,
        loc_package,
        loc_start
      ),
    Q,
    qLocation,
  )
import Optics.Core (Prism')
import Optics.Prism (prism)
import System.IO (Handle)
import System.Log.FastLogger (LogStr, ToLogStr (toLogStr), fromLogStr)

-- | Dynamic logging effect for "Control.Monad.Logger".
--
-- @since 0.1
data Logger :: Effect where
  LoggerLog ::
    (ToLogStr msg) =>
    Loc ->
    LogSource ->
    LogLevel ->
    msg ->
    Logger m ()

-- | @since 0.1
type instance DispatchOf Logger = Dynamic

-- | @since 0.1
instance ShowEffect Logger where
  showEffectCons = \case
    LoggerLog _ _ _ _ -> "LoggerLog"

-- | Writes a log.
--
-- @since 0.1
loggerLog ::
  ( HasCallStack,
    Logger :> es,
    ToLogStr msg
  ) =>
  Loc ->
  LogSource ->
  LogLevel ->
  msg ->
  Eff es ()
loggerLog loc src lvl = send . LoggerLog loc src lvl

-- | @since 0.1
data LogLevel
  = -- | @since 0.1
    LevelTrace
  | -- | @since 0.1
    LevelDebug
  | -- | @since 0.1
    LevelInfo
  | -- | @since 0.1
    LevelWarn
  | -- | @since 0.1
    LevelError
  | -- | @since 0.1
    LevelFatal
  | -- | @since 0.1
    LevelOther Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show,
      -- | @since 0.1
      Read,
      -- | @since 0.1
      Ord
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
type LogSource = Text

-- | @since 0.1
instance Lift LogLevel where
  lift LevelTrace = [|LevelTrace|]
  lift LevelDebug = [|LevelDebug|]
  lift LevelInfo = [|LevelInfo|]
  lift LevelWarn = [|LevelWarn|]
  lift LevelError = [|LevelError|]
  lift LevelFatal = [|LevelFatal|]
  lift (LevelOther x) = [|LevelOther $ pack $(lift $ unpack x)|]

  liftTyped LevelTrace = [||LevelTrace||]
  liftTyped LevelDebug = [||LevelDebug||]
  liftTyped LevelInfo = [||LevelInfo||]
  liftTyped LevelWarn = [||LevelWarn||]
  liftTyped LevelError = [||LevelError||]
  liftTyped LevelFatal = [||LevelFatal||]
  liftTyped (LevelOther x) = [||LevelOther $ pack $$(liftTyped $ unpack x)||]

-- | @since 0.1
logTH :: LogLevel -> Q Exp
logTH level =
  [|
    loggerLog $(qLocation >>= liftLoc) (pack "") $(lift level)
      . (id :: Text -> Text)
    |]

-- | Generates a function that takes a 'LogLevel' and a 'Show a => a'.
--
-- @since 0.1
logTHShow :: LogLevel -> Q Exp
logTHShow level =
  [|
    loggerLog $(qLocation >>= liftLoc) (pack "") $(lift level)
      . ((pack . show) :: (Show a) => a -> Text)
    |]

-- | See 'logDebug'
--
-- @since 0.1
logTrace :: Q Exp
logTrace = logTH LevelTrace

-- | Generates a function that takes a 'Text' and logs a 'LevelDebug' message.
-- Usage:
--
-- > $(logDebug) "This is a debug log message"
--
-- @since 0.1
logDebug :: Q Exp
logDebug = logTH LevelDebug

-- | See 'logDebug'
--
-- @since 0.1
logInfo :: Q Exp
logInfo = logTH LevelInfo

-- | See 'logDebug'
--
-- @since 0.1
logWarn :: Q Exp
logWarn = logTH LevelWarn

-- | See 'logDebug'
--
-- @since 0.1
logError :: Q Exp
logError = logTH LevelError

-- | See 'logDebug'
--
-- @since 0.1
logFatal :: Q Exp
logFatal = logTH LevelFatal

-- | Generates a function that takes a 'Text' and logs a 'LevelOther' message.
-- Usage:
--
-- > $(logOther "My new level") "This is a log message"
--
-- @since 0.1
logOther :: Text -> Q Exp
logOther = logTH . LevelOther

-- | See 'logDebugSH'
--
-- @since 0.1
logTraceSH :: Q Exp
logTraceSH = logTHShow LevelTrace

-- | Generates a function that takes a 'Show a => a' and logs a 'LevelDebug'
-- message. Usage:
--
-- > $(logDebugSH) (Just "This is a debug log message")
--
-- @since 0.1
logDebugSH :: Q Exp
logDebugSH = logTHShow LevelDebug

-- | See 'logDebugSH'
--
-- @since 0.1
logInfoSH :: Q Exp
logInfoSH = logTHShow LevelInfo

-- | See 'logDebugSH'
--
-- @since 0.1
logWarnSH :: Q Exp
logWarnSH = logTHShow LevelWarn

-- | See 'logDebugSH'
--
-- @since 0.1
logErrorSH :: Q Exp
logErrorSH = logTHShow LevelError

-- | See 'logDebugSH'
--
-- @since 0.1
logFatalSH :: Q Exp
logFatalSH = logTHShow LevelFatal

-- | Generates a function that takes a 'Show a => a' and logs a 'LevelOther'
-- message. Usage:
--
-- > $(logOtherSH "My new level") "This is a log message"
--
-- @since 0.1
logOtherSH :: Text -> Q Exp
logOtherSH = logTHShow . LevelOther

-- | Lift a location into an Exp.
--
-- @since 0.1
liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) =
  [|
    Loc
      $(lift a)
      $(lift b)
      $(lift c)
      ($(lift d1), $(lift d2))
      ($(lift e1), $(lift e2))
    |]

-- | See 'logDebugS'
--
-- @since 0.1
logTraceS :: Q Exp
logTraceS =
  [|\a b -> loggerLog $(qLocation >>= liftLoc) a LevelTrace (b :: Text)|]

-- | Generates a function that takes a 'LogSource' and 'Text' and logs a
-- 'LevelDebug' message. Usage:
--
-- > $logDebugS "SomeSource" "This is a debug log message"
--
-- @since 0.1
logDebugS :: Q Exp
logDebugS =
  [|\a b -> loggerLog $(qLocation >>= liftLoc) a LevelDebug (b :: Text)|]

-- | See 'logDebugS'
--
-- @since 0.1
logInfoS :: Q Exp
logInfoS =
  [|\a b -> loggerLog $(qLocation >>= liftLoc) a LevelInfo (b :: Text)|]

-- | See 'logDebugS'
--
-- @since 0.1
logWarnS :: Q Exp
logWarnS =
  [|\a b -> loggerLog $(qLocation >>= liftLoc) a LevelWarn (b :: Text)|]

-- | See 'logDebugS'
--
-- @since 0.1
logErrorS :: Q Exp
logErrorS =
  [|\a b -> loggerLog $(qLocation >>= liftLoc) a LevelError (b :: Text)|]

-- | See 'logDebugS'
--
-- @since 0.1
logFatalS :: Q Exp
logFatalS =
  [|\a b -> loggerLog $(qLocation >>= liftLoc) a LevelFatal (b :: Text)|]

-- | Generates a function that takes a 'LogSource', a level name and a 'Text'
-- and logs a 'LevelOther' message. Usage:
--
-- > $logOtherS "SomeSource" "My new level" "This is a log message"
--
-- @since 0.1
logOtherS :: Q Exp
logOtherS =
  [|
    \src level msg ->
      loggerLog
        $(qLocation >>= liftLoc)
        src
        (LevelOther level)
        (msg :: Text)
    |]

-- | @since 0.1
type LogLine = (Loc, LogSource, LogLevel, LogStr)

-- | A default implementation of 'loggerLog' that accepts a file
-- handle as the first argument.
--
-- This is used in the definition of 'runStdoutLoggingT':
--
-- @
-- 'runStdoutLoggingT' :: 'MonadIO' m => 'LoggingT' m a -> m a
-- 'runStdoutLoggingT' action =
--     'runLoggingT' action ('defaultOutput' 'stdout')
-- @
--
-- @since 0.1
defaultOutput ::
  (HasCallStack) =>
  Handle ->
  Loc ->
  LogSource ->
  LogLevel ->
  LogStr ->
  IO ()
defaultOutput h loc src level msg =
  S8.hPutStr h ls
  where
    ls = defaultLogStrBS loc src level msg

-- | @since 0.1
defaultLogStrBS ::
  Loc ->
  LogSource ->
  LogLevel ->
  LogStr ->
  ByteString
defaultLogStrBS a b c d = fromLogStr $ defaultLogStr a b c d

-- | @since 0.1
defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr level = case level of
  LevelOther t -> toLogStr t
  _ -> toLogStr $ S8.pack $ drop 5 $ show level

-- | @since 0.1
defaultLogStr ::
  Loc ->
  LogSource ->
  LogLevel ->
  LogStr ->
  LogStr
defaultLogStr loc src level msg =
  "["
    <> defaultLogLevelStr level
    <> ( if T.null src
           then mempty
           else "#" <> toLogStr src
       )
    <> "] "
    <> msg
    <> ( if isDefaultLoc loc
           then "\n"
           else
             " @("
               <> toLogStr (S8.pack fileLocStr)
               <> ")\n"
       )
  where
    -- taken from file-location package
    -- turn the TH Loc loaction information into a human readable string
    -- leaving out the loc_end parameter
    fileLocStr =
      loc_package loc
        ++ ':'
        : loc_module loc
        ++ ' '
        : loc_filename loc
        ++ ':'
        : line loc
        ++ ':'
        : char loc
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

-- | dummy location, used with 'logWithoutLoc'
--
-- @since 0.1
defaultLoc :: Loc
defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0, 0) (0, 0)

-- | @since 0.1
isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0, 0) (0, 0)) = True
isDefaultLoc _ = False

-- | @since 0.1
logWithoutLoc ::
  ( HasCallStack,
    Logger :> es,
    ToLogStr msg
  ) =>
  LogSource ->
  LogLevel ->
  msg ->
  Eff es ()
logWithoutLoc = loggerLog defaultLoc

-- | @since 0.1
logTraceN :: (Logger :> es) => Text -> Eff es ()
logTraceN = logWithoutLoc "" LevelTrace

-- | @since 0.1
logDebugN :: (Logger :> es) => Text -> Eff es ()
logDebugN = logWithoutLoc "" LevelDebug

-- | @since 0.1
logInfoN :: (Logger :> es) => Text -> Eff es ()
logInfoN = logWithoutLoc "" LevelInfo

-- | @since 0.1
logWarnN :: (Logger :> es) => Text -> Eff es ()
logWarnN = logWithoutLoc "" LevelWarn

-- | @since 0.1
logErrorN :: (Logger :> es) => Text -> Eff es ()
logErrorN = logWithoutLoc "" LevelError

-- | @since 0.1
logFatalN :: (Logger :> es) => Text -> Eff es ()
logFatalN = logWithoutLoc "" LevelFatal

-- | @since 0.1
logOtherN :: (Logger :> es) => LogLevel -> Text -> Eff es ()
logOtherN = logWithoutLoc ""

-- | @since 0.1
logTraceNS :: (Logger :> es) => LogSource -> Text -> Eff es ()
logTraceNS src = logWithoutLoc src LevelTrace

-- | @since 0.1
logDebugNS :: (Logger :> es) => LogSource -> Text -> Eff es ()
logDebugNS src = logWithoutLoc src LevelDebug

-- | @since 0.1
logInfoNS :: (Logger :> es) => LogSource -> Text -> Eff es ()
logInfoNS src = logWithoutLoc src LevelInfo

-- | @since 0.1
logWarnNS :: (Logger :> es) => LogSource -> Text -> Eff es ()
logWarnNS src = logWithoutLoc src LevelWarn

-- | @since 0.1
logErrorNS :: (Logger :> es) => LogSource -> Text -> Eff es ()
logErrorNS src = logWithoutLoc src LevelError

-- | @since 0.1
logFatalNS :: (Logger :> es) => LogSource -> Text -> Eff es ()
logFatalNS src = logWithoutLoc src LevelFatal

-- | @since 0.1
logOtherNS ::
  (HasCallStack, Logger :> es) =>
  LogSource ->
  LogLevel ->
  Text ->
  Eff es ()
logOtherNS = logWithoutLoc

-- | @since 0.1
mkLoggerLoc :: SrcLoc -> Loc
mkLoggerLoc loc =
  Loc
    { loc_filename = srcLocFile loc,
      loc_package = srcLocPackage loc,
      loc_module = srcLocModule loc,
      loc_start =
        ( srcLocStartLine loc,
          srcLocStartCol loc
        ),
      loc_end =
        ( srcLocEndLine loc,
          srcLocEndCol loc
        )
    }

-- | @since 0.1
locFromCS :: CallStack -> Loc
locFromCS cs = case getCallStack cs of
  ((_, loc) : _) -> mkLoggerLoc loc
  _ -> defaultLoc

-- | @since 0.1
logCS ::
  (HasCallStack, Logger :> es, ToLogStr msg) =>
  CallStack ->
  LogSource ->
  LogLevel ->
  msg ->
  Eff es ()
logCS cs = loggerLog (locFromCS cs)

-- | See 'logDebugCS'
--
-- @since 0.1
logTraceCS :: (HasCallStack, Logger :> es) => CallStack -> Text -> Eff es ()
logTraceCS cs = logCS cs "" LevelTrace

-- | Logs a message with location given by 'CallStack'.
-- See 'Control.Monad.Logger.CallStack' for more convenient
-- functions for 'CallStack' based logging.
--
-- @since 0.1
logDebugCS :: (HasCallStack, Logger :> es) => CallStack -> Text -> Eff es ()
logDebugCS cs = logCS cs "" LevelDebug

-- | See 'logDebugCS'
--
-- @since 0.1
logInfoCS :: (HasCallStack, Logger :> es) => CallStack -> Text -> Eff es ()
logInfoCS cs = logCS cs "" LevelInfo

-- | See 'logDebugCS'
--
-- @since 0.1
logWarnCS :: (HasCallStack, Logger :> es) => CallStack -> Text -> Eff es ()
logWarnCS cs = logCS cs "" LevelWarn

-- | See 'logDebugCS'
--
-- @since 0.1
logErrorCS :: (HasCallStack, Logger :> es) => CallStack -> Text -> Eff es ()
logErrorCS cs = logCS cs "" LevelError

-- | See 'logDebugCS'
--
-- @since 0.1
logFatalCS :: (HasCallStack, Logger :> es) => CallStack -> Text -> Eff es ()
logFatalCS cs = logCS cs "" LevelFatal

-- | See 'logDebugCS'
--
-- @since 0.1
logOtherCS ::
  (HasCallStack, Logger :> es) =>
  CallStack ->
  LogLevel ->
  Text ->
  Eff es ()
logOtherCS cs = logCS cs ""

{- HLINT ignore "Eta reduce" -}

-- | @guardLevel configLvl lvl m@ runs @m@ iff @'shouldLog' configLvl lvl@.
--
-- @since 0.1
guardLevel ::
  (Applicative f, HasCallStack) =>
  -- | The configured log level to check against.
  LogLevel ->
  -- | The log level for this action.
  LogLevel ->
  -- | The logging action to run if the level passes.
  ((HasCallStack) => f ()) ->
  f ()
guardLevel configLvl lvl logAction =
  -- NOTE: No eta reduction, presumably related to DeepSubsumption
  when (shouldLog configLvl lvl) logAction

-- | @shouldLog configLvl lvl@ returns true iff @configLvl <= lvl@. Uses
-- LogLevel's built-in ordering. The ordering is thus:
--
-- @
--   LevelTrace
--     < LevelDebug
--     < LevelInfo
--     < LevelWarn
--     < LevelError
--     < LevelFatal
--     < LevelOther \"\<any\>\"
-- @
--
-- In other words, 'LogLevel''s usual 'Ord' is respected. Note that
-- @LevelOther "custom"@ sits at the the highest level and compares via
-- Text's 'Ord', just like 'LogLevel''s usual 'Ord'.
--
-- @since 0.1
shouldLog ::
  -- | The configured log level to check against.
  LogLevel ->
  -- | Level for this log
  LogLevel ->
  -- | Whether we should log
  Bool
shouldLog = (<=)

-- | @since 0.1
_LevelTrace :: Prism' LogLevel ()
_LevelTrace =
  prism
    (const LevelTrace)
    ( \case
        LevelTrace -> Right ()
        other -> Left other
    )
{-# INLINE _LevelTrace #-}

-- | @since 0.1
_LevelDebug :: Prism' LogLevel ()
_LevelDebug =
  prism
    (const LevelDebug)
    ( \case
        LevelDebug -> Right ()
        other -> Left other
    )
{-# INLINE _LevelDebug #-}

-- | @since 0.1
_LevelInfo :: Prism' LogLevel ()
_LevelInfo =
  prism
    (const LevelInfo)
    ( \case
        LevelInfo -> Right ()
        other -> Left other
    )
{-# INLINE _LevelInfo #-}

-- | @since 0.1
_LevelWarn :: Prism' LogLevel ()
_LevelWarn =
  prism
    (const LevelWarn)
    ( \case
        LevelWarn -> Right ()
        other -> Left other
    )
{-# INLINE _LevelWarn #-}

-- | @since 0.1
_LevelError :: Prism' LogLevel ()
_LevelError =
  prism
    (const LevelError)
    ( \case
        LevelError -> Right ()
        other -> Left other
    )
{-# INLINE _LevelError #-}

-- | @since 0.1
_LevelOther :: Prism' LogLevel Text
_LevelOther =
  prism
    LevelOther
    ( \case
        LevelOther l -> Right l
        other -> Left other
    )
{-# INLINE _LevelOther #-}

-- | @since 0.1
_LevelFatal :: Prism' LogLevel ()
_LevelFatal =
  prism
    (const LevelFatal)
    ( \case
        LevelFatal -> Right ()
        other -> Left other
    )
{-# INLINE _LevelFatal #-}
