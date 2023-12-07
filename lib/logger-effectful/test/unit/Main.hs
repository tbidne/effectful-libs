{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Text (Text)
import Effectful (runEff)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Logger.Dynamic
  ( LogLevel
      ( LevelDebug,
        LevelError,
        LevelFatal,
        LevelInfo,
        LevelOther,
        LevelTrace,
        LevelWarn
      ),
    LoggerDynamic (LoggerLog),
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
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Effectful.Logger.Dynamic"
      [ logLevelTH,
        shouldLevelTests
      ]

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

{-genStdLevel :: Gen LogLevel
genStdLevel =
  Gen.element
    [ LevelTrace,
      LevelInfo,
      LevelDebug,
      LevelWarn,
      LevelError,
      LevelFatal
    ]-}

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

-- isNot :: (Is k An_AffineFold) => Optic' k is s a -> s -> Bool
-- isNot o = not . is o

{-

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
-}
