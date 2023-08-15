module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Fixed (Fixed (MkFixed))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay), utc)
import Effectful
  ( Eff,
    IOE,
    runEff,
  )
import Effectful.Time.Dynamic
  ( LocalTime (LocalTime),
    TimeDynamic,
    TimeSpec (MkTimeSpec),
    ZonedTime (ZonedTime),
    runTimeDynamicIO,
  )
import Effectful.Time.Dynamic qualified as TimeDynamic
import Hedgehog (Gen, annotate, annotateShow, diff, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as R
import Numeric.Natural (Natural)
import Optics.Core (view)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit Tests"
      [ classTests,
        timeSpecTests,
        localTimeTests,
        zonedTimeTests
      ]

classTests :: TestTree
classTests =
  testGroup
    "Class"
    [ getsSystemTime,
      getsSystemZonedTime,
      getsMonotonicTime
    ]

getsSystemTime :: TestTree
getsSystemTime =
  testCase "Retrieves system local time" $
    void (runEffTime TimeDynamic.getSystemTime)

getsSystemZonedTime :: TestTree
getsSystemZonedTime =
  testCase "Retrieves system zoned time" $
    void (runEffTime TimeDynamic.getSystemZonedTime)

getsMonotonicTime :: TestTree
getsMonotonicTime = testPropertyNamed desc "getsMonotonicTime" $
  property $ do
    t1 <- liftIO $ runEffTime TimeDynamic.getMonotonicTime
    t2 <- liftIO $ runEffTime TimeDynamic.getMonotonicTime
    annotateShow t1
    annotateShow t2
    diff t1 (<=) t2
  where
    desc = "getMonotonicTime is monotonic"

timeSpecTests :: TestTree
timeSpecTests =
  testGroup
    "TimeSpec"
    [ eqEquivClass,
      createFromSeconds,
      elimToSeconds,
      toFromSecondsEpsilon,
      fromToSecondsEpsilon,
      createFromNanoSeconds,
      elimToNanoseconds,
      toFromNatRoundTrip,
      fromToNatRoundTrip,
      diffsTimeSpec,
      diffTimeSpecCommutes,
      normalizesTimeSpec,
      normalizeInvariant,
      timesAction
    ]

eqEquivClass :: TestTree
eqEquivClass = testPropertyNamed desc "eqEquivClass" $
  property $ do
    ts@(MkTimeSpec s ns) <- forAll genTimeSpec
    let ts' = MkTimeSpec 0 (s * 1_000_000_000 + ns)
    ts === ts'
  where
    desc = "Eq equivalence class"

createFromSeconds :: TestTree
createFromSeconds =
  testCase "Creates TimeSpec from Double seconds" $
    expected @=? TimeDynamic.fromSeconds 10.123456789
  where
    expected = MkTimeSpec 10 123456789

createFromNanoSeconds :: TestTree
createFromNanoSeconds =
  testCase "Creates TimeSpec from Natural nanoseconds" $
    expected @=? TimeDynamic.fromNanoSeconds 10_123_456_789
  where
    expected = MkTimeSpec 10 123456789

elimToSeconds :: TestTree
elimToSeconds =
  testCase "Maps TimeSpec to seconds" $
    10.123456789 @=? TimeDynamic.toSeconds (MkTimeSpec 10 123_456_789)

toFromSecondsEpsilon :: TestTree
toFromSecondsEpsilon = testPropertyNamed desc "toFromSecondsEpsilon" $
  property $ do
    s <- forAll genDouble
    let ts = TimeDynamic.fromSeconds s
        s' = TimeDynamic.toSeconds ts
    annotateShow ts

    diff (abs (s - s')) (<) 1
  where
    desc = "(toSeconds . fromSeconds) x ~= x (up to 1 sec)"

fromToSecondsEpsilon :: TestTree
fromToSecondsEpsilon = testPropertyNamed desc "fromToSecondsEpsilon" $
  property $ do
    ts <- forAll genTimeSpec
    let s = TimeDynamic.toSeconds ts
        ts' = TimeDynamic.fromSeconds s
    annotateShow s

    toSeconds ts === toSeconds ts'
  where
    toSeconds = view #sec . TimeDynamic.normalizeTimeSpec
    desc = "(toSeconds . fromSeconds) x ~= x (up to 1 sec)"

toFromNatRoundTrip :: TestTree
toFromNatRoundTrip = testPropertyNamed desc "toFromNatRoundTrip" $
  property $ do
    ns <- forAll genNanoSeconds
    let ts = TimeDynamic.fromNanoSeconds ns
        ns' = TimeDynamic.toNanoSeconds ts
    annotateShow ts
    ns === ns'
  where
    desc = "toNanoSeconds . fromNanoSeconds == id"

fromToNatRoundTrip :: TestTree
fromToNatRoundTrip = testPropertyNamed desc "fromToNatRoundTrip" $
  property $ do
    ts <- forAll genTimeSpec
    let ns = TimeDynamic.toNanoSeconds ts
        ts' = TimeDynamic.fromNanoSeconds ns
    annotateShow ns
    ts === ts'
  where
    desc = "fromNanoSeconds . toNanoSeconds == id"

elimToNanoseconds :: TestTree
elimToNanoseconds =
  testCase "Maps TimeSpec to nanoseconds" $
    10123456789 @=? TimeDynamic.toNanoSeconds (MkTimeSpec 10 123_456_789)

diffsTimeSpec :: TestTree
diffsTimeSpec = testCase "Diffs TimeSpecs" $ do
  MkTimeSpec 10 864197532 @=? TimeDynamic.diffTimeSpec t1 t2
  MkTimeSpec 10 864197532 @=? TimeDynamic.diffTimeSpec t2 t1
  where
    t1 = MkTimeSpec 10 123_456_789
    t2 = MkTimeSpec 20 987_654_321

diffTimeSpecCommutes :: TestTree
diffTimeSpecCommutes = testPropertyNamed desc "diffsTimeSpec2" $
  property $ do
    ts1 <- forAll genTimeSpec
    ts2 <- forAll genTimeSpec
    let d1 = TimeDynamic.diffTimeSpec ts1 ts2
        d2 = TimeDynamic.diffTimeSpec ts2 ts1
    d1 === d2
  where
    desc = "diffTimeSpec is commutative"

normalizesTimeSpec :: TestTree
normalizesTimeSpec =
  testCase "Normalizes TimeSpec" $
    MkTimeSpec 55 123456789 @=? TimeDynamic.normalizeTimeSpec t
  where
    t = MkTimeSpec 10 45_123_456_789

normalizeInvariant :: TestTree
normalizeInvariant = testPropertyNamed desc "normalizeInvariant" $
  property $ do
    ts <- forAll genTimeSpec
    let ts'@(MkTimeSpec _ ns') = TimeDynamic.normalizeTimeSpec ts

    annotateShow ts'

    -- nanoseconds < 1 second
    diff ns' (<) 1_000_000_000

    -- equivalence class
    ts === ts'
  where
    desc = "Normalizes TimeSpec"

timesAction :: TestTree
timesAction = testCase "Times an action" $ do
  ts <- runEffTime $ TimeDynamic.withTiming_ (liftIO (threadDelay 1_000_000))
  assertBool (show ts <> " >= 0.8 s") $ ts >= MkTimeSpec 0 800_000_000
  assertBool (show ts <> " <= 1.2 s") $ ts <= MkTimeSpec 1 200_000_000

localTimeTests :: TestTree
localTimeTests =
  testGroup
    "LocalTime"
    [ formatsLocalTime,
      parsesLocalTime,
      formatParseLocalTimeRoundTrip,
      parseFormatLocalTimeEpsilon
    ]

zonedTimeTests :: TestTree
zonedTimeTests =
  testGroup
    "ZonedTime"
    [ formatsZonedTime,
      parsesZonedTime,
      formatParseZonedTimeRoundTrip,
      parseFormatZonedTimeEpsilon
    ]

formatsLocalTime :: TestTree
formatsLocalTime =
  testCase "Formats LocalTime" $
    "2022-02-08 10:20:05" @=? TimeDynamic.formatLocalTime localTime

parsesLocalTime :: TestTree
parsesLocalTime = testCase "Parses LocalTime" $ do
  lt <- TimeDynamic.parseLocalTime "2022-02-08 10:20:05"
  localTime @=? lt

formatParseLocalTimeRoundTrip :: TestTree
formatParseLocalTimeRoundTrip = testPropertyNamed desc "formatParseLocalTimeRoundTrip" $
  property $ do
    str <- forAll genLocalTimeString
    lt <- TimeDynamic.parseLocalTime str
    annotateShow lt

    let str' = TimeDynamic.formatLocalTime lt

    str === str'
  where
    desc = "formatLocalTime . parseLocalTime == id"

parseFormatLocalTimeEpsilon :: TestTree
parseFormatLocalTimeEpsilon = testPropertyNamed desc "parseFormatLocalTimeEpsilon" $
  property $ do
    lt <- forAll genLocalTime
    let str = TimeDynamic.formatLocalTime lt
    annotate str

    lt' <- TimeDynamic.parseLocalTime str

    diff lt eqLocalTimeEpsilon lt'
  where
    desc = "(parseLocalTime . formatLocalTime) x ~= x (up to < 1 second)"

formatsZonedTime :: TestTree
formatsZonedTime =
  testCase "Formats ZonedTime" $
    "2022-02-08 10:20:05 UTC" @=? TimeDynamic.formatZonedTime zonedTime

parsesZonedTime :: TestTree
parsesZonedTime = testCase "Parses ZonedTime" $ do
  ZonedTime lt tz <- TimeDynamic.parseZonedTime "2022-02-08 10:20:05 UTC"
  let ZonedTime expectedLt expectedTz = zonedTime
  expectedLt @=? lt
  expectedTz @=? tz

formatParseZonedTimeRoundTrip :: TestTree
formatParseZonedTimeRoundTrip = testPropertyNamed desc "formatParseZonedTimeRoundTrip" $
  property $ do
    str <- forAll genZonedTimeString
    lt <- TimeDynamic.parseZonedTime str
    annotateShow lt

    let str' = TimeDynamic.formatZonedTime lt

    str === str'
  where
    desc = "formatZonedTime . parseZonedTime == id"

parseFormatZonedTimeEpsilon :: TestTree
parseFormatZonedTimeEpsilon = testPropertyNamed desc "parseFormatZonedTimeEpsilon" $
  property $ do
    zt <- forAll genZonedTime
    let str = TimeDynamic.formatZonedTime zt
    annotate str

    zt' <- TimeDynamic.parseZonedTime str

    diff zt eqZonedTimeEpsilon zt'
  where
    desc = "(parseZonedTime . formatZonedTime) x ~= x (up to < 1 second)"

localTime :: LocalTime
localTime = LocalTime day tod
  where
    day = fromOrdinalDate 2022 39
    tod = TimeOfDay 10 20 5

zonedTime :: ZonedTime
zonedTime = ZonedTime localTime utc

eqLocalTimeEpsilon :: LocalTime -> LocalTime -> Bool
eqLocalTimeEpsilon
  (LocalTime d1 (TimeOfDay h1 m1 (MkFixed s1)))
  (LocalTime d2 (TimeOfDay h2 m2 (MkFixed s2))) =
    d1 == d2
      && h1 == h2
      && m1 == m2
      -- Second difference must be less than 1
      -- (== < 1_000_000_000_000 picoseconds)
      && abs (s1 - s2) < 1_000_000_000_000

eqZonedTimeEpsilon :: ZonedTime -> ZonedTime -> Bool
eqZonedTimeEpsilon (ZonedTime t1 z1) (ZonedTime t2 z2) =
  eqLocalTimeEpsilon t1 t2
    && z1 == z2

genLocalTime :: Gen LocalTime
genLocalTime = LocalTime <$> genDay <*> genTimeOfDay
  where
    genDay = fromOrdinalDate <$> genYear <*> genDayOfYear
    genYear = Gen.integral (R.linear 0 2022)
    genDayOfYear = Gen.integral (R.linear 0 364)

    genTimeOfDay = TimeOfDay <$> genHour <*> genMin <*> genSec
    genHour = Gen.integral (R.linear 0 23)
    genMin = Gen.integral (R.linear 0 59)
    -- Fixed precision pico seconds 1e-12
    genSec = MkFixed <$> Gen.integral (R.linear 0 59_000_000_000_000)

genZonedTime :: Gen ZonedTime
genZonedTime = ZonedTime <$> genLocalTime <*> genTz
  where
    genTz = pure utc

genLocalTimeString :: Gen String
genLocalTimeString =
  toDate
    <$> genYear
    <*> genMonth
    <*> genDay
    <*> genHour
    <*> genMin
    <*> genSec
  where
    toDate y m d h mn s =
      mconcat
        [ y,
          "-",
          m,
          "-",
          d,
          " ",
          h,
          ":",
          mn,
          ":",
          s
        ]
    genYear = Gen.list (R.singleton 4) Gen.digit
    genMonth = Gen.element $ fmap (pad2 . show @Int) [1 .. 12]
    genDay = Gen.element $ fmap (pad2 . show @Int) [1 .. 28]
    genHour = Gen.element $ fmap (pad2 . show @Int) [1 .. 23]
    genMin = Gen.element $ fmap (pad2 . show @Int) [0 .. 59]
    genSec = genMin
    pad2 s
      | length s == 1 = '0' : s
      | otherwise = s

genZonedTimeString :: Gen String
genZonedTimeString =
  (<>)
    <$> genLocalTimeString
    <*> Gen.element
      [ " UTC",
        " UT",
        " GMT",
        " EST",
        " EDT",
        " CST",
        " CDT",
        " MST",
        " MDT",
        " PST",
        " PDT",
        " +1300"
      ]

genNanoSeconds :: Gen Natural
genNanoSeconds = Gen.integral (R.linearFrom 1_000_000_000 0 100_000_000_000)

genDouble :: Gen Double
genDouble = Gen.double (R.linearFracFrom 1 0 100)

genTimeSpec :: Gen TimeSpec
genTimeSpec = MkTimeSpec <$> genSec <*> genNSec
  where
    genSec = Gen.integral (R.linearFrom 5 0 10)
    genNSec = Gen.integral (R.linearFrom 0 0 10_000_000_000)

runEffTime :: Eff '[TimeDynamic, IOE] a -> IO a
runEffTime = runEff . runTimeDynamicIO
