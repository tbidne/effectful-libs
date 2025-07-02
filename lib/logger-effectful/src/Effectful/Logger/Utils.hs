{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Logger.Utils
  ( -- * Namespace
    Namespace (..),
    displayNamespace,

    -- * Levels
    LogLevel (..),

    -- * Formatter
    LogFormatter (..),
    defaultLogFormatter,
    LocStrategy (..),
    formatLog,

    -- * Misc
    logStrToText,
    logStrToBs,
  )
where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Foldable (Foldable (foldMap'))
#if MIN_VERSION_base(4, 18, 0)
import Data.Functor ((<&>))
#endif
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as CC
#if MIN_VERSION_base(4, 18, 0)
import Effectful.Concurrent.Static qualified as Thread
#endif
import Effectful.Time.Dynamic (Time)
import Effectful.Time.Dynamic qualified as Time
import GHC.Generics (Generic)
#if MIN_VERSION_base(4, 17, 0)
import GHC.IsList (IsList (Item, fromList, toList))
#else
import GHC.Exts (IsList (Item, fromList, toList))
#endif
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (Loc (loc_filename, loc_start))
import Language.Haskell.TH.Syntax (Lift (lift, liftTyped))
import Optics.Core
  ( A_Lens,
    An_Iso,
    LabelOptic (labelOptic),
    iso,
    lensVL,
    view,
    (%),
    (^.),
    _1,
    _2,
  )
import System.Log.FastLogger (LogStr, ToLogStr (toLogStr))
import System.Log.FastLogger qualified as FL

-- | Logging namespace.
--
-- @since 0.1
newtype Namespace = MkNamespace
  { -- | @since 0.1
    unNamespace :: Seq Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Monoid,
      -- | @since 0.1
      Semigroup
    )
    via (Seq Text)
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance
  (k ~ An_Iso, a ~ Seq Text, b ~ Seq Text) =>
  LabelOptic "unNamespace" k Namespace Namespace a b
  where
  labelOptic = iso (\(MkNamespace ns) -> ns) MkNamespace
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance IsString Namespace where
  fromString = MkNamespace . Seq.singleton . T.pack

-- | @since 0.1
instance IsList Namespace where
  type Item Namespace = Text
  fromList = MkNamespace . fromList
  toList = toList . unNamespace

displayNamespace :: Namespace -> Text
displayNamespace =
  foldMap' id
    . Seq.intersperse "."
    . (.unNamespace)

-- | Determines how we log location data.
--
-- @since 0.1
data LocStrategy
  = -- | Logs the location with filename, line, col.
    --
    -- @since 0.1
    LocPartial !Loc
  | -- | Logs the location with filename.
    --
    -- @since 0.1
    LocStable !Loc
  | -- | No location logging.
    --
    -- @since 0.1
    LocNone
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | Formatter for logs.
--
-- @since 0.1
data LogFormatter = MkLogFormatter
  { -- | How to log the code location.
    --
    -- @since 0.1
    locStrategy :: !LocStrategy,
    -- | If true, append a newline.
    --
    -- @since 0.1
    newline :: !Bool,
    -- | Whether to include the thread's label set by 'Thread.labelThread'.
    -- Falls back to the thread's 'Thread.ThreadId' when the label has not been set.
    --
    -- @since 0.1
    threadLabel :: !Bool,
    -- | Whether to include the timezone in the timestamp.
    --
    -- @since 0.1
    timezone :: !Bool
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ LocStrategy, b ~ LocStrategy) =>
  LabelOptic "locStrategy" k LogFormatter LogFormatter a b
  where
  labelOptic = lensVL $ \f (MkLogFormatter a1 a2 a3 a4) ->
    fmap
      (\b -> MkLogFormatter b a2 a3 a4)
      (f a1)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "newline" k LogFormatter LogFormatter a b
  where
  labelOptic = lensVL $ \f (MkLogFormatter a1 a2 a3 a4) ->
    fmap
      (\b -> MkLogFormatter a1 b a3 a4)
      (f a2)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "threadLabel" k LogFormatter LogFormatter a b
  where
  labelOptic = lensVL $ \f (MkLogFormatter a1 a2 a3 a4) ->
    fmap
      (\b -> MkLogFormatter a1 a2 b a4)
      (f a3)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "timezone" k LogFormatter LogFormatter a b
  where
  labelOptic = lensVL $ \f (MkLogFormatter a1 a2 a3 a4) ->
    fmap
      (\b -> MkLogFormatter a1 a2 a3 b)
      (f a4)
  {-# INLINE labelOptic #-}

-- | 'LogFormatter' with:
--
-- @
-- 'locStrategy' = 'LocPartial' loc
-- 'newline' = 'True'
-- 'threadLabel' = 'False'
-- 'timezone' = 'False'
-- @
--
-- @since 0.1
defaultLogFormatter :: Loc -> LogFormatter
defaultLogFormatter loc =
  MkLogFormatter
    { locStrategy = LocPartial loc,
      newline = True,
      threadLabel = False,
      timezone = False
    }

-- | Produces a formatted 'LogStr'.
--
-- @since 0.1
formatLog ::
  ( Concurrent :> es,
    HasCallStack,
    Time :> es,
    ToLogStr msg
  ) =>
  Maybe Namespace ->
  LogFormatter ->
  LogLevel ->
  msg ->
  Eff es LogStr
formatLog mNamespace formatter lvl msg = do
  timestampTxt <- timeFn
  let namespaceTxt = case mNamespace of
        Nothing -> ""
        Just namespace -> brackets $ toLogStr $ displayNamespace namespace
  threadLbl <-
    if formatter ^. #threadLabel
      then getThreadLabel
      else pure ""
  let locTxt = case formatter ^. #locStrategy of
        LocPartial loc -> (brackets . toLogStr . partialLoc) loc
        LocStable loc -> (brackets . toLogStr . stableLoc) loc
        LocNone -> ""
      lvlTxt = toLogStr $ showLevel lvl
      msgTxt = toLogStr msg
      newline'
        | formatter ^. #newline = "\n"
        | otherwise = ""
      formatted =
        mconcat
          [ brackets timestampTxt,
            threadLbl,
            namespaceTxt,
            locTxt,
            brackets lvlTxt,
            " ",
            msgTxt,
            newline'
          ]
  pure formatted
  where
    timeFn
      | formatter ^. #timezone =
          toLogStr <$> Time.getSystemZonedTimeString
      | otherwise =
          toLogStr <$> Time.getSystemTimeString

{- ORMOLU_DISABLE -}

-- | Retrieves the thread label or thread id, if the former has not been set.
getThreadLabel :: (Concurrent :> es, HasCallStack) => Eff es LogStr
getThreadLabel = do
#if MIN_VERSION_base(4, 18, 0)
  tid <- CC.myThreadId
  Thread.threadLabel tid <&> \case
    Just label -> bracketsLogStr label
    Nothing -> bracketsLogStr $ show tid
#else
  bracketsLogStr . show <$> CC.myThreadId
#endif
  where
    bracketsLogStr = brackets . toLogStr

{- ORMOLU_ENABLE -}

partialLoc :: Loc -> Builder
partialLoc loc =
  mconcat
    [ fromString $ view #loc_filename loc,
      ":" <> mkLine loc,
      ":" <> mkChar loc
    ]
  where
    mkLine = fromString . show . view (#loc_start % _1)
    mkChar = fromString . show . view (#loc_start % _2)

stableLoc :: Loc -> Builder
stableLoc loc = fromString $ view #loc_filename loc

showLevel :: LogLevel -> Text
showLevel LevelTrace = "Trace"
showLevel LevelDebug = "Debug"
showLevel LevelInfo = "Info"
showLevel LevelWarn = "Warn"
showLevel LevelError = "Error"
showLevel LevelFatal = "Fatal"
showLevel (LevelOther txt) = txt

-- LogStr uses ByteString's Builder internally, so we might as well use it
-- for constants.
brackets :: LogStr -> LogStr
brackets m = cLogStr "[" <> m <> cLogStr "]"

cLogStr :: Builder -> LogStr
cLogStr = toLogStr @Builder

-- | @since 0.1
logStrToBs :: LogStr -> ByteString
logStrToBs = FL.fromLogStr

-- | @since 0.1
logStrToText :: LogStr -> Text
logStrToText = TEnc.decodeUtf8With TEncError.lenientDecode . FL.fromLogStr

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
instance Lift LogLevel where
  lift LevelTrace = [|LevelTrace|]
  lift LevelDebug = [|LevelDebug|]
  lift LevelInfo = [|LevelInfo|]
  lift LevelWarn = [|LevelWarn|]
  lift LevelError = [|LevelError|]
  lift LevelFatal = [|LevelFatal|]
  lift (LevelOther x) = [|LevelOther $ pack $(lift $ T.unpack x)|]

  liftTyped LevelTrace = [||LevelTrace||]
  liftTyped LevelDebug = [||LevelDebug||]
  liftTyped LevelInfo = [||LevelInfo||]
  liftTyped LevelWarn = [||LevelWarn||]
  liftTyped LevelError = [||LevelError||]
  liftTyped LevelFatal = [||LevelFatal||]
  liftTyped (LevelOther x) = [||LevelOther $ T.pack $$(liftTyped $ T.unpack x)||]
