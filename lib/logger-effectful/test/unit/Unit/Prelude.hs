{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Unit.Prelude
  ( -- * Runners
    Env (..),
    runEffLoggerFormat,

    -- * Formatters
    format,
    formatNamespaced,

    -- * Utils
    fromLogStr,
    loc,
    myThreadLabel,
  )
where

#if MIN_VERSION_base(4, 18, 0)
import Data.Functor ((<&>))
#endif
import Data.ByteString.Char8 qualified as C8
import Data.Text (Text)
import Data.Time (TimeOfDay (TimeOfDay), utc)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Effectful (Eff, IOE, runEff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as CC
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Logger.Dynamic (Loc (Loc), LogLevel (LevelWarn), LogStr)
import Effectful.Logger.Dynamic qualified as Logger
import Effectful.Logger.Namespace (LogFormatter, Namespace, addNamespace, logStrToBs)
import Effectful.Logger.Namespace qualified as Namespace
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Time.Dynamic (LocalTime (LocalTime), Time (GetMonotonicTime, GetSystemZonedTime), ZonedTime (ZonedTime))
import GHC.Conc.Sync qualified as Sync
import Optics.Core (A_Lens, LabelOptic (labelOptic), LabelOptic', lens)

newtype Env = MkEnv {namespace :: Namespace}

instance
  (k ~ A_Lens, a ~ Namespace, b ~ Namespace) =>
  LabelOptic "namespace" k Env Env a b
  where
  labelOptic =
    lens
      (\(MkEnv l) -> l)
      (\(MkEnv _) l -> MkEnv l)

runEffLoggerFormat :: Eff '[Concurrent, Reader Env, Time, IOE] a -> IO a
runEffLoggerFormat =
  runEff
    . runTimePure
    . runReader (MkEnv mempty)
    . CC.runConcurrent

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

-- runLoggerNamespacePure ::
--  Eff (LoggerNS : es) a ->
--  Eff es a
-- runLoggerNamespacePure = reinterpret (evalState ([] :: Namespace)) $ \env -> \case
--  GetNamespace -> get
-- LocalNamespace f m -> localSeqUnlift env $ \run -> modify f *> run m

format ::
  forall env k es.
  ( Concurrent :> es,
    k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    Reader env :> es,
    Time :> es
  ) =>
  Namespace.LogFormatter ->
  Eff es LogStr
format = format' @env Logger.formatLog

formatNamespaced ::
  forall env k es.
  ( Concurrent :> es,
    k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    Reader env :> es,
    Time :> es
  ) =>
  Namespace.LogFormatter ->
  Eff es LogStr
formatNamespaced = format' @env (Namespace.formatLog @env)

format' ::
  forall env k es.
  ( k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    Reader env :> es
  ) =>
  (LogFormatter -> LogLevel -> Text -> Eff es LogStr) ->
  Namespace.LogFormatter ->
  Eff es LogStr
format' logFn fmt =
  addNamespace @env "one" $
    addNamespace @env "two" $
      logFn fmt LevelWarn "msg"

loc :: Loc
loc = Loc "filename" "pkg" "module" (1, 2) (3, 4)

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

fromLogStr :: LogStr -> String
fromLogStr = C8.unpack . logStrToBs
