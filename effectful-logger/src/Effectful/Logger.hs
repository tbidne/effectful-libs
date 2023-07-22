{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A reimplementation of the Control.Monad.Logger API for Effectful.
--
-- @since 0.1
module Effectful.Logger
  ( -- * Effect
    LoggerEffect (..),

    -- ** Functions
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
    logDebug,
    logInfo,
    logWarn,
    logError,
    logOther,

    -- * TH logging of showable values
    logDebugSH,
    logInfoSH,
    logWarnSH,
    logErrorSH,
    logOtherSH,

    -- * TH logging with source
    logDebugS,
    logInfoS,
    logWarnS,
    logErrorS,
    logOtherS,

    -- * TH util
    liftLoc,

    -- * Non-TH logging
    logDebugN,
    logInfoN,
    logWarnN,
    logErrorN,
    logOtherN,

    -- * Non-TH logging with source
    logWithoutLoc,
    logDebugNS,
    logInfoNS,
    logWarnNS,
    logErrorNS,
    logOtherNS,

    -- * Callstack logging
    logDebugCS,
    logInfoCS,
    logWarnCS,
    logErrorCS,
    logOtherCS,

    -- * utilities for defining your own loggers
    defaultLogStr,
    Loc (..),
    defaultLoc,
    defaultOutput,
  )
where

import Control.DeepSeq (NFData)
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
import Effectful.Dispatch.Dynamic (send)
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
import System.IO (Handle)
import System.Log.FastLogger (LogStr, ToLogStr (..), fromLogStr)

-- | Logging effect.
--
-- @since 0.1
data LoggerEffect :: Effect where
  LoggerLog ::
    ( ToLogStr msg
    ) =>
    Loc ->
    LogSource ->
    LogLevel ->
    msg ->
    LoggerEffect m ()

-- | @since 0.1
type instance DispatchOf LoggerEffect = Dynamic

-- | Returns the local system time.
--
-- @since 0.1
loggerLog ::
  ( LoggerEffect :> es,
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
    LevelDebug
  | -- | @since 0.1
    LevelInfo
  | -- | @since 0.1
    LevelWarn
  | -- | @since 0.1
    LevelError
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
  lift LevelDebug = [|LevelDebug|]
  lift LevelInfo = [|LevelInfo|]
  lift LevelWarn = [|LevelWarn|]
  lift LevelError = [|LevelError|]
  lift (LevelOther x) = [|LevelOther $ pack $(lift $ unpack x)|]

  liftTyped LevelDebug = [||LevelDebug||]
  liftTyped LevelInfo = [||LevelInfo||]
  liftTyped LevelWarn = [||LevelWarn||]
  liftTyped LevelError = [||LevelError||]
  liftTyped (LevelOther x) = [||LevelOther $ pack $$(liftTyped $ unpack x)||]

-- | @since 0.1
logTH :: LogLevel -> Q Exp
logTH level =
  [|
    monadLoggerLog $(qLocation >>= liftLoc) (pack "") $(lift level)
      . (id :: Text -> Text)
    |]

-- | Generates a function that takes a 'LogLevel' and a 'Show a => a'.
--
-- @since 0.1
logTHShow :: LogLevel -> Q Exp
logTHShow level =
  [|
    monadLoggerLog $(qLocation >>= liftLoc) (pack "") $(lift level)
      . ((pack . show) :: (Show a) => a -> Text)
    |]

-- | Generates a function that takes a 'Text' and logs a 'LevelDebug' message. Usage:
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

-- | Generates a function that takes a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $(logOther "My new level") "This is a log message"
--
-- @since 0.1
logOther :: Text -> Q Exp
logOther = logTH . LevelOther

-- | Generates a function that takes a 'Show a => a' and logs a 'LevelDebug' message. Usage:
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

-- | Generates a function that takes a 'Show a => a' and logs a 'LevelOther' message. Usage:
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

-- | Generates a function that takes a 'LogSource' and 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $logDebugS "SomeSource" "This is a debug log message"
--
-- @since 0.1
logDebugS :: Q Exp
logDebugS = [|\a b -> monadLoggerLog $(qLocation >>= liftLoc) a LevelDebug (b :: Text)|]

-- | See 'logDebugS'
--
-- @since 0.1
logInfoS :: Q Exp
logInfoS = [|\a b -> monadLoggerLog $(qLocation >>= liftLoc) a LevelInfo (b :: Text)|]

-- | See 'logDebugS'
--
-- @since 0.1
logWarnS :: Q Exp
logWarnS = [|\a b -> monadLoggerLog $(qLocation >>= liftLoc) a LevelWarn (b :: Text)|]

-- | See 'logDebugS'
--
-- @since 0.1
logErrorS :: Q Exp
logErrorS = [|\a b -> monadLoggerLog $(qLocation >>= liftLoc) a LevelError (b :: Text)|]

-- | Generates a function that takes a 'LogSource', a level name and a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $logOtherS "SomeSource" "My new level" "This is a log message"
--
-- @since 0.1
logOtherS :: Q Exp
logOtherS = [|\src level msg -> monadLoggerLog $(qLocation >>= liftLoc) src (LevelOther level) (msg :: Text)|]

-- | @since 0.1
type LogLine = (Loc, LogSource, LogLevel, LogStr)

-- | A default implementation of 'monadLoggerLog' that accepts a file
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
  ( LoggerEffect :> es,
    ToLogStr msg
  ) =>
  LogSource ->
  LogLevel ->
  msg ->
  Eff es ()
logWithoutLoc = loggerLog defaultLoc

-- | @since 0.1
logDebugN :: (LoggerEffect :> es) => Text -> Eff es ()
logDebugN = logWithoutLoc "" LevelDebug

-- | @since 0.1
logInfoN :: (LoggerEffect :> es) => Text -> Eff es ()
logInfoN = logWithoutLoc "" LevelInfo

-- | @since 0.1
logWarnN :: (LoggerEffect :> es) => Text -> Eff es ()
logWarnN = logWithoutLoc "" LevelWarn

-- | @since 0.1
logErrorN :: (LoggerEffect :> es) => Text -> Eff es ()
logErrorN = logWithoutLoc "" LevelError

-- | @since 0.1
logOtherN :: (LoggerEffect :> es) => LogLevel -> Text -> Eff es ()
logOtherN = logWithoutLoc ""

-- | @since 0.1
logDebugNS :: (LoggerEffect :> es) => LogSource -> Text -> Eff es ()
logDebugNS src = logWithoutLoc src LevelDebug

-- | @since 0.1
logInfoNS :: (LoggerEffect :> es) => LogSource -> Text -> Eff es ()
logInfoNS src = logWithoutLoc src LevelInfo

-- | @since 0.1
logWarnNS :: (LoggerEffect :> es) => LogSource -> Text -> Eff es ()
logWarnNS src = logWithoutLoc src LevelWarn

-- | @since 0.1
logErrorNS :: (LoggerEffect :> es) => LogSource -> Text -> Eff es ()
logErrorNS src = logWithoutLoc src LevelError

-- | @since 0.1
logOtherNS :: (LoggerEffect :> es) => LogSource -> LogLevel -> Text -> Eff es ()
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
  (LoggerEffect :> es, ToLogStr msg) =>
  CallStack ->
  LogSource ->
  LogLevel ->
  msg ->
  Eff es ()
logCS cs = loggerLog (locFromCS cs)

-- | Logs a message with location given by 'CallStack'.
-- See 'Control.Monad.Logger.CallStack' for more convenient
-- functions for 'CallStack' based logging.
--
-- @since 0.1
logDebugCS :: (LoggerEffect :> es) => CallStack -> Text -> Eff es ()
logDebugCS cs = logCS cs "" LevelDebug

-- | See 'logDebugCS'
--
-- @since 0.1
logInfoCS :: (LoggerEffect :> es) => CallStack -> Text -> Eff es ()
logInfoCS cs = logCS cs "" LevelInfo

-- | See 'logDebugCS'
--
-- @since 0.1
logWarnCS :: (LoggerEffect :> es) => CallStack -> Text -> Eff es ()
logWarnCS cs = logCS cs "" LevelWarn

-- | See 'logDebugCS'
--
-- @since 0.1
logErrorCS :: (LoggerEffect :> es) => CallStack -> Text -> Eff es ()
logErrorCS cs = logCS cs "" LevelError

-- | See 'logDebugCS'
--
-- @since 0.1
logOtherCS :: (LoggerEffect :> es) => CallStack -> LogLevel -> Text -> Eff es ()
logOtherCS cs = logCS cs ""
