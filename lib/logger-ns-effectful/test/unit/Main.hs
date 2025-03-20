{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Data.ByteString.Char8 qualified as Char8
#if MIN_VERSION_base(4, 18, 0)
import Data.Functor ((<&>))
#endif
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
    IOE,
    runEff,
    type (:>),
  )
import Effectful.Concurrent qualified as CC
import Effectful.Concurrent.Static (Concurrent)
import Effectful.Dispatch.Dynamic
  ( interpret,
    localSeqUnlift,
    reinterpret,
  )
import Effectful.Logger.Dynamic
  ( Loc (Loc),
    LogLevel (LevelWarn),
  )
import Effectful.LoggerNS.Dynamic
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, threadLabel, timezone),
    LogStr,
    LoggerNS (GetNamespace, LocalNamespace),
    Namespace,
    addNamespace,
    formatLog,
    logStrToBs,
  )
import Effectful.State.Static.Local (evalState, get, modify)
import Effectful.Time.Dynamic
  ( Time (GetMonotonicTime, GetSystemZonedTime),
  )
import GHC.Conc.Sync qualified as Sync
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
        formatLocPartial,
        formatThreadLabel
      ]

formatBasic :: TestTree
formatBasic = testCase "Formats a basic namespaced log" $ do
  logMsg <- runEffLoggerNamespace (formatNamespaced fmt)
  "[2022-02-08 10:20:05][one.two][Warn] msg" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocNone,
          newline = False,
          threadLabel = False,
          timezone = False
        }

formatNewline :: TestTree
formatNewline = testCase "Formats a log with a newline" $ do
  logMsg <- runEffLoggerNamespace (formatNamespaced fmt)
  "[2022-02-08 10:20:05][one.two][Warn] msg\n" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocNone,
          newline = True,
          threadLabel = False,
          timezone = False
        }

formatTimezone :: TestTree
formatTimezone = testCase "Formats a log with a timezone" $ do
  logMsg <- runEffLoggerNamespace (formatNamespaced fmt)
  "[2022-02-08 10:20:05 UTC][one.two][Warn] msg" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocNone,
          newline = False,
          threadLabel = False,
          timezone = True
        }

formatLocStable :: TestTree
formatLocStable = testCase "Formats a log with stable loc" $ do
  logMsg <- runEffLoggerNamespace (formatNamespaced fmt)
  "[2022-02-08 10:20:05][one.two][filename][Warn] msg" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocStable loc,
          newline = False,
          threadLabel = False,
          timezone = False
        }

formatLocPartial :: TestTree
formatLocPartial = testCase "Formats a log with partial loc" $ do
  logMsg <- runEffLoggerNamespace (formatNamespaced fmt)
  "[2022-02-08 10:20:05][one.two][filename:1:2][Warn] msg" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocPartial loc,
          newline = False,
          threadLabel = False,
          timezone = False
        }

{- ORMOLU_DISABLE -}

formatThreadLabel :: TestTree
formatThreadLabel = testCase "Formats a log with thread label" $ do
  logMsg <- runEffLoggerNamespace (formatNamespaced fmt)

  -- Tasty sets the thread label. We could set this here ourselves, but
  -- in the interest of not interferring with the test setup, we simply
  -- grab it here.
  lbl <- myThreadLabel
  let msg =
        mconcat
          [ "[2022-02-08 10:20:05][",
            lbl,
            "][one.two][Warn] msg"
          ]

  msg @=? fromLogStr logMsg

  where
    fmt =
      MkLogFormatter
        { locStrategy = LocNone,
          newline = False,
          threadLabel = True,
          timezone = False
        }

{- ORMOLU_ENABLE -}

formatNamespaced ::
  ( Concurrent :> es,
    LoggerNS :> es,
    Time :> es
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
  Eff (Time : es) a ->
  Eff es a
runTimePure = interpret $ \_ -> \case
  GetSystemZonedTime -> pure zonedTime
  GetMonotonicTime -> pure 50
  _ -> error "runTimePure: unimplemented"

runLoggerNamespacePure ::
  Eff (LoggerNS : es) a ->
  Eff es a
runLoggerNamespacePure = reinterpret (evalState ([] :: Namespace)) $ \env -> \case
  GetNamespace -> get
  LocalNamespace f m -> localSeqUnlift env $ \run -> modify f *> run m

runEffLoggerNamespace :: Eff '[Concurrent, LoggerNS, Time, IOE] a -> IO a
runEffLoggerNamespace =
  runEff
    . runTimePure
    . runLoggerNamespacePure
    . CC.runConcurrent

myThreadLabel :: IO String
myThreadLabel = do
#if MIN_VERSION_base(4, 18, 0)
  tid <- Sync.myThreadId
  Sync.threadLabel tid <&> \case
    Nothing -> show tid
    Just l -> l
#else
  show <$> Sync.myThreadId
#endif
