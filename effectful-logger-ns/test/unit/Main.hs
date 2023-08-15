{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
    ZonedTime (ZonedTime),
    utc,
  )
import Effectful
  ( Eff,
    runPureEff,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( interpret,
    localSeqUnlift,
    reinterpret,
  )
import Effectful.Logger.Dynamic
  ( Loc (Loc),
    LogLevel (..),
  )
import Effectful.LoggerNS.Dynamic
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter (..),
    LogStr,
    LoggerNSDynamic (..),
    Namespace,
    addNamespace,
    formatLog,
    logStrToBs,
  )
import Effectful.State.Static.Local (evalState, get, modify)
import Effectful.Time
  ( TimeDynamic (..),
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit Tests"
      [ formatBasic,
        formatNewline,
        formatTimezone,
        formatLocStable,
        formatLocPartial
      ]

formatBasic :: TestTree
formatBasic =
  testCase "Formats a basic namespaced log" $
    "[2022-02-08 10:20:05][one.two][Warn] msg" @=? fromLogStr logMsg
  where
    logMsg = runEffLoggerNamespace (formatNamespaced fmt)
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocNone,
          timezone = False
        }

formatNewline :: TestTree
formatNewline =
  testCase "Formats a log with a newline" $
    "[2022-02-08 10:20:05][one.two][Warn] msg\n" @=? fromLogStr logMsg
  where
    logMsg = runEffLoggerNamespace (formatNamespaced fmt)
    fmt =
      MkLogFormatter
        { newline = True,
          locStrategy = LocNone,
          timezone = False
        }

formatTimezone :: TestTree
formatTimezone =
  testCase "Formats a log with a timezone" $
    "[2022-02-08 10:20:05 UTC][one.two][Warn] msg" @=? fromLogStr logMsg
  where
    logMsg = runEffLoggerNamespace (formatNamespaced fmt)
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocNone,
          timezone = True
        }

formatLocStable :: TestTree
formatLocStable =
  testCase "Formats a log with stable loc" $
    "[2022-02-08 10:20:05][one.two][Warn][filename] msg" @=? fromLogStr logMsg
  where
    logMsg = runEffLoggerNamespace (formatNamespaced fmt)
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocStable loc,
          timezone = False
        }

formatLocPartial :: TestTree
formatLocPartial =
  testCase "Formats a log with partial loc" $
    "[2022-02-08 10:20:05][one.two][Warn][filename:1:2] msg" @=? fromLogStr logMsg
  where
    logMsg = runEffLoggerNamespace (formatNamespaced fmt)
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocPartial loc,
          timezone = False
        }

formatNamespaced ::
  ( LoggerNSDynamic :> es,
    TimeDynamic :> es
  ) =>
  LogFormatter ->
  Eff es LogStr
formatNamespaced fmt =
  addNamespace "one" $
    addNamespace "two" $
      formatLog @_ @Text fmt LevelWarn "msg"

fromLogStr :: LogStr -> String
fromLogStr = Char8.unpack . logStrToBs

loc :: Loc
loc = Loc "filename" "pkg" "module" (1, 2) (3, 4)

localTime :: LocalTime
localTime = LocalTime day tod
  where
    day = fromOrdinalDate 2022 39
    tod = TimeOfDay 10 20 5

zonedTime :: ZonedTime
zonedTime = ZonedTime localTime utc

runTimePure ::
  Eff (TimeDynamic : es) a ->
  Eff es a
runTimePure = interpret $ \_ -> \case
  GetSystemTime -> pure localTime
  GetSystemZonedTime -> pure zonedTime
  GetMonotonicTime -> pure 50

runLoggerNamespacePure ::
  Eff (LoggerNSDynamic : es) a ->
  Eff es a
runLoggerNamespacePure = reinterpret (evalState ([] :: Namespace)) $ \env -> \case
  GetNamespace -> get
  LocalNamespace f m -> localSeqUnlift env $ \run -> modify f *> run m

runEffLoggerNamespace :: Eff '[LoggerNSDynamic, TimeDynamic] a -> a
runEffLoggerNamespace =
  runPureEff
    . runTimePure
    . runLoggerNamespacePure
