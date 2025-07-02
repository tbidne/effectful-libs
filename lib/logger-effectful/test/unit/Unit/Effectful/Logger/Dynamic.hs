{-# LANGUAGE TemplateHaskell #-}

module Unit.Effectful.Logger.Dynamic (tests) where

import Data.Text (Text, pack)
import Effectful (runEff)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Logger.Dynamic
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter
      ( MkLogFormatter,
        locStrategy,
        newline,
        threadLabel,
        timezone
      ),
    LogLevel
      ( LevelDebug,
        LevelError,
        LevelFatal,
        LevelInfo,
        LevelOther,
        LevelTrace,
        LevelWarn
      ),
    Logger (LoggerLog),
    logDebug,
    logError,
    logFatal,
    logInfo,
    logOther,
    logTrace,
    logWarn,
    shouldLog,
    _LevelOther,
  )
import Hedgehog (Gen, annotate, assert, failure, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Optics.Core (preview)
import Optics.Core.Extras (is)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)
import Unit.Prelude
  ( Env,
    format,
    fromLogStr,
    loc,
    myThreadLabel,
    runEffLoggerFormat,
  )

tests :: TestTree
tests =
  testGroup
    "Effectful.Logger.Dynamic"
    [ formatTests,
      logLevelTH,
      shouldLevelTests
    ]

formatTests :: TestTree
formatTests =
  testGroup
    "Formatting"
    [ formatBasic,
      formatNewline,
      formatTimezone,
      formatLocStable,
      formatLocPartial,
      formatThreadLabel
    ]

formatBasic :: TestTree
formatBasic = testCase "Formats a basic namespaced log" $ do
  logMsg <- runEffLoggerFormat (format @Env fmt)
  "[2022-02-08 10:20:05][Warn] msg" @=? fromLogStr logMsg
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
  logMsg <- runEffLoggerFormat (format @Env fmt)
  "[2022-02-08 10:20:05][Warn] msg\n" @=? fromLogStr logMsg
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
  logMsg <- runEffLoggerFormat (format @Env fmt)
  "[2022-02-08 10:20:05 UTC][Warn] msg" @=? fromLogStr logMsg
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
  logMsg <- runEffLoggerFormat (format @Env fmt)
  "[2022-02-08 10:20:05][filename][Warn] msg" @=? fromLogStr logMsg
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
  logMsg <- runEffLoggerFormat (format @Env fmt)
  "[2022-02-08 10:20:05][filename:1:2][Warn] msg" @=? fromLogStr logMsg
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
  logMsg <- runEffLoggerFormat (format @Env fmt)

  -- Tasty sets the thread label. We could set this here ourselves, but
  -- in the interest of not interferring with the test setup, we simply
  -- grab it here.
  lbl <- myThreadLabel
  let msg =
        mconcat
          [ "[2022-02-08 10:20:05][",
            lbl,
            "][Warn] msg"
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

logLevelTH :: TestTree
logLevelTH = testCase "TH logging compiles" $ do
  runEff . runLoggerIO $ do
    $(logTrace) "trace"
    $(logInfo) "info"
    $(logDebug) "debug"
    $(logWarn) "warn"
    $(logError) "error"
    $(logOther "custom") "other"
    $(logFatal) "fatal"
  where
    runLoggerIO = interpret $ \_ -> \case
      LoggerLog {} -> pure ()

shouldLevelTests :: TestTree
shouldLevelTests =
  testGroup
    "shouldLevel"
    [ traceProps,
      debugProps,
      infoProps,
      warnProps,
      errorProps,
      fatalProps,
      customProps,
      followsOrd,
      otherSpecs
    ]

traceProps :: TestTree
traceProps = testPropertyNamed "Trace properties" "traceProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    -- trace logs everything
    assert $ LevelTrace `shouldLog` lvl

    if lvl `shouldLog` LevelTrace
      then assert $ LevelTrace == lvl
      else
        assert $
          LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
            || LevelFatal == lvl
            || isOther lvl

debugProps :: TestTree
debugProps = testPropertyNamed "Debug properties" "debugProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    if LevelDebug `shouldLog` lvl
      then
        assert $
          LevelDebug
            == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
            || LevelFatal == lvl
            || isOther lvl
      else
        assert $
          LevelTrace == lvl

    if lvl `shouldLog` LevelDebug
      then
        assert $
          LevelTrace == lvl
            || LevelDebug == lvl
      else
        assert $
          LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
            || LevelFatal == lvl
            || isOther lvl

infoProps :: TestTree
infoProps = testPropertyNamed "Info properties" "infoProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    if LevelInfo `shouldLog` lvl
      then
        assert $
          LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
            || LevelFatal == lvl
            || isOther lvl
      else
        assert $
          LevelTrace == lvl
            || LevelDebug == lvl

    if lvl `shouldLog` LevelInfo
      then
        assert $
          LevelTrace == lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
      else
        assert $
          LevelWarn == lvl
            || LevelError == lvl
            || LevelFatal == lvl
            || isOther lvl

warnProps :: TestTree
warnProps = testPropertyNamed "Warn properties" "warnProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    if LevelWarn `shouldLog` lvl
      then
        assert $
          LevelWarn == lvl
            || LevelError == lvl
            || LevelFatal == lvl
            || isOther lvl
      else
        assert $
          LevelTrace == lvl
            || LevelDebug == lvl
            || LevelInfo == lvl

    if lvl `shouldLog` LevelWarn
      then
        assert $
          LevelTrace == lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
      else
        assert $
          LevelError == lvl
            || LevelFatal == lvl
            || isOther lvl

errorProps :: TestTree
errorProps = testPropertyNamed "Error properties" "errorProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    if LevelError `shouldLog` lvl
      then
        assert $
          LevelError == lvl
            || LevelFatal == lvl
            || isOther lvl
      else
        assert $
          LevelTrace == lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl

    if lvl `shouldLog` LevelError
      then
        assert $
          LevelTrace == lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
      else
        assert $
          LevelFatal == lvl
            || isOther lvl

fatalProps :: TestTree
fatalProps = testPropertyNamed "Fatal properties" "fatalProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    -- Fatal logs nothing except itself and non-trace custom
    if LevelFatal `shouldLog` lvl
      then
        assert $
          LevelFatal == lvl
            || isOther lvl
      else do
        assert $
          LevelTrace == lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
            || LevelFatal == lvl

    if lvl `shouldLog` LevelFatal
      then
        assert $
          LevelTrace == lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
            || LevelFatal == lvl
      else assert $ isOther lvl

customProps :: TestTree
customProps = testPropertyNamed "Custom properties" "customProps" $ do
  property $ do
    lvl <- forAll genAnyLevel
    customLvl <- forAll genText

    if LevelOther customLvl `shouldLog` lvl
      then case preview _LevelOther lvl of
        Nothing -> do
          annotate "LevelOther <custom> `shouldLog` l should imply l is also LevelOther"
          failure
        Just l -> assert $ customLvl <= l
      else case preview _LevelOther lvl of
        Nothing -> pure ()
        Just l -> assert $ customLvl > l

    if lvl `shouldLog` LevelOther customLvl
      then case preview _LevelOther lvl of
        Nothing -> pure ()
        Just l -> assert $ l <= customLvl
      else case preview _LevelOther lvl of
        Nothing -> do
          annotate "l `shouldLog` LevelOther <custom> should imply l is also LevelOther"
          failure
        Just l -> assert $ l > customLvl

followsOrd :: TestTree
followsOrd = testPropertyNamed "Follows ord" "followsOrd" $ do
  property $ do
    lvl1 <- forAll genAnyLevel
    lvl2 <- forAll genAnyLevel

    if
      | lvl1 == lvl2 -> do
          assert $ lvl1 `shouldLog` lvl2
          assert $ lvl2 `shouldLog` lvl1
      | lvl1 < lvl2 -> assert $ lvl1 `shouldLog` lvl2
      | otherwise -> assert $ lvl2 `shouldLog` lvl1

otherSpecs :: TestTree
otherSpecs = testCase "LevelOther specs" $ do
  assertBool "Debug should not log trace" $ not $ shouldLog LevelDebug LevelTrace
  assertBool "Debug should log custom" $ shouldLog LevelDebug (LevelOther "Custom")
  assertBool "Debug should log fatal" $ shouldLog LevelDebug LevelFatal

genAnyLevel :: Gen LogLevel
genAnyLevel =
  Gen.choice
    [ pure LevelTrace,
      pure LevelInfo,
      pure LevelDebug,
      pure LevelWarn,
      pure LevelError,
      pure LevelFatal,
      LevelOther <$> genText
    ]

genText :: Gen Text
genText = Gen.text (Range.linearFrom 0 0 20) Gen.unicode

isOther :: LogLevel -> Bool
isOther = is _LevelOther
