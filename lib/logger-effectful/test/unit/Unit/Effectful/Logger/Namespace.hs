{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Unit.Effectful.Logger.Namespace (tests) where

import Control.Monad (void)
import Data.Text (Text)
import Effectful (Eff, IOE, runEff, type (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Logger.Dynamic (Logger (LoggerLog))
import Effectful.Logger.Namespace
  ( HasNamespace,
    LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter
      ( MkLogFormatter,
        locStrategy,
        newline,
        threadLabel,
        timezone
      ),
    LoggerNS,
    Namespace,
    addNamespace,
  )
import Effectful.Logger.Namespace qualified as NS
import Effectful.Logger.Utils (LogLevel (LevelInfo))
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Time.Dynamic (Time, runTime)
import Optics.Core
  ( A_Getter,
    A_Lens,
    A_Setter,
    An_Iso,
    Is,
    LabelOptic (labelOptic),
    LabelOptic',
    iso,
    lensVL,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Unit.Prelude
  ( Env,
    formatNamespaced,
    fromLogStr,
    loc,
    myThreadLabel,
    runEffLoggerFormat,
  )

tests :: TestTree
tests =
  testGroup
    "Effects.Logger.Namespace"
    [ formatTests,
      miscTests
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
  logMsg <- runEffLoggerFormat (formatNamespaced @Env fmt)
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
  logMsg <- runEffLoggerFormat (formatNamespaced @Env fmt)
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
  logMsg <- runEffLoggerFormat (formatNamespaced @Env fmt)
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
  logMsg <- runEffLoggerFormat (formatNamespaced @Env fmt)
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
  logMsg <- runEffLoggerFormat (formatNamespaced @Env fmt)
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
  logMsg <- runEffLoggerFormat (formatNamespaced @Env fmt)

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

miscTests :: TestTree
miscTests =
  testGroup
    "Misc"
    [ testOpticsInference
    ]

testOpticsInference :: TestTree
testOpticsInference = testCase "Optics type inference is reliable" $ do
  runEnv (addNamespace @LensEnv "ns" $ pure ()) lensEnv
  runEnv (usesNamespace1 @LensEnv) lensEnv
  runEnv (usesNamespace2 @LensEnv) lensEnv
  runEnv (usesNamespace3 @LensEnv) lensEnv

  runEnv (void $ NS.formatLog @LensEnv defFmt LevelInfo ("" :: Text)) lensEnv
  runEnv (usesFormat1 @LensEnv) lensEnv
  runEnv (usesFormat2 @LensEnv) lensEnv
  runEnv (usesFormat3 @LensEnv) lensEnv

  runEnv (addNamespace @IsoEnv "ns" $ pure ()) isoEnv
  runEnv (usesNamespace1 @IsoEnv) isoEnv
  runEnv (usesNamespace2 @IsoEnv) isoEnv
  runEnv (usesNamespace3 @IsoEnv) isoEnv

  runEnv (void $ NS.formatLog @IsoEnv defFmt LevelInfo ("" :: Text)) isoEnv
  runEnv (usesFormat1 @IsoEnv) isoEnv
  runEnv (usesFormat2 @IsoEnv) isoEnv
  runEnv (usesFormat3 @IsoEnv) isoEnv
  where
    runEnv ::
      Eff [Time, Logger, Concurrent, Reader env, IOE] () ->
      env ->
      IO ()
    runEnv m e = do
      r <-
        runEff
          . runReader e
          . runConcurrent
          . runLogger
          . runTime
          $ m

      () @=? r

    lensEnv = MkLensEnv "lens"
    isoEnv = MkIsoEnv "iso"

runLogger :: Eff (Logger : es) a -> Eff es a
runLogger = interpret_ $ \case
  LoggerLog {} -> pure ()

usesNamespace1 ::
  forall env k es.
  ( Is k A_Setter,
    LabelOptic' "namespace" k env Namespace,
    Reader env :> es
  ) =>
  Eff es ()
usesNamespace1 = addNamespace @env "ns" $ pure ()

usesNamespace2 :: forall env k es. (HasNamespace env k es) => Eff es ()
usesNamespace2 = addNamespace @env "ns" $ pure ()

usesNamespace3 :: forall env k es. (LoggerNS env k es) => Eff es ()
usesNamespace3 = addNamespace @env "ns" $ pure ()

usesFormat1 ::
  forall env k es.
  ( Concurrent :> es,
    Is k A_Getter,
    LabelOptic' "namespace" k env Namespace,
    Reader env :> es,
    Time :> es
  ) =>
  Eff es ()
usesFormat1 = void $ NS.formatLog @env defFmt LevelInfo ("" :: Text)

usesFormat2 ::
  forall env k es.
  ( Concurrent :> es,
    HasNamespace env k es,
    Time :> es
  ) =>
  Eff es ()
usesFormat2 = void $ NS.formatLog @env defFmt LevelInfo ("" :: Text)

usesFormat3 ::
  forall env k es.
  ( Concurrent :> es,
    LoggerNS env k es,
    Time :> es
  ) =>
  Eff es ()
usesFormat3 = void $ NS.formatLog @env defFmt LevelInfo ("" :: Text)

defFmt :: LogFormatter
defFmt = MkLogFormatter LocNone False False False

newtype LensEnv = MkLensEnv Namespace

instance
  ( k ~ A_Lens,
    x ~ Namespace,
    y ~ Namespace
  ) =>
  LabelOptic "namespace" k LensEnv LensEnv x y
  where
  labelOptic =
    lensVL $ \f (MkLensEnv a1) ->
      fmap
        (\b -> MkLensEnv b)
        (f a1)

newtype IsoEnv = MkIsoEnv Namespace

instance
  ( k ~ An_Iso,
    x ~ Namespace,
    y ~ Namespace
  ) =>
  LabelOptic "namespace" k IsoEnv IsoEnv x y
  where
  labelOptic = iso (\(MkIsoEnv x) -> x) MkIsoEnv
