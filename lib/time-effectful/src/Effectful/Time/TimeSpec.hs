{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'TimeSpec' type and algebra re-exports.
--
-- @since 0.1
module Effectful.Time.TimeSpec
  ( -- * Type
    TimeSpec (..),

    -- * Creation
    fromSeconds,
    fromNanoSeconds,

    -- * Elimination
    toSeconds,
    toNanoSeconds,

    -- * Operations
    diffTimeSpec,
    normalizeTimeSpec,

    -- * Algebra
    ASemigroup (..),
    AMonoid (..),
    MSemiSpace (..),
    MSpace (..),
    Semimodule,
    SemivectorSpace,
    Normed (..),
    LowerBounded (..),
    UpperBoundless,
  )
where

import Control.DeepSeq (NFData)
import Data.Bounds (LowerBounded (lowerBound), UpperBoundless)
#if MIN_VERSION_base(4,17,0)
import GHC.Float (properFractionDouble)
#endif
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Numeric.Algebra
  ( AMonoid (zero),
    ASemigroup ((.+.)),
    MSemiSpace ((.*)),
    MSpace ((.%)),
    Normed (norm),
    Semimodule,
    SemivectorSpace,
  )
import Optics.Core (A_Lens, LabelOptic (labelOptic), lensVL)

-- | Structure for holding time data. 'Eq' and 'Ord' are defined in terms of
-- an equivalence class e.g.
--
-- @
-- MkTimeSpec s n === MkTimeSpec 0 (s * 1_000_000_000 + n)
-- @
--
-- @since 0.1
data TimeSpec = MkTimeSpec
  { -- | Seconds.
    --
    -- @since 0.1
    sec :: !Natural,
    -- | Nanoseconds.
    --
    -- @since 0.1
    nsec :: !Natural
  }
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Natural, b ~ Natural) =>
  LabelOptic "sec" k TimeSpec TimeSpec a b
  where
  labelOptic = lensVL $ \f (MkTimeSpec _sec _nsec) ->
    fmap (`MkTimeSpec` _nsec) (f _sec)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Natural, b ~ Natural) =>
  LabelOptic "nsec" k TimeSpec TimeSpec a b
  where
  labelOptic = lensVL $ \f (MkTimeSpec _sec _nsec) ->
    fmap (MkTimeSpec _sec) (f _nsec)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance Eq TimeSpec where
  l == r = toNanoSeconds l == toNanoSeconds r

-- | @since 0.1
instance Ord TimeSpec where
  l <= r = toNanoSeconds l <= toNanoSeconds r

-- | @since 0.1
instance LowerBounded TimeSpec where
  lowerBound = MkTimeSpec 0 0

-- | @since 0.1
instance UpperBoundless TimeSpec

-- | @since 0.1
instance ASemigroup TimeSpec where
  MkTimeSpec ls ln .+. MkTimeSpec rs rn = MkTimeSpec (ls + rs) (ln + rn)

-- | @since 0.1
instance AMonoid TimeSpec where
  zero = MkTimeSpec 0 0

-- | @since 0.1
instance MSemiSpace TimeSpec Natural where
  MkTimeSpec s n .* k = MkTimeSpec (s * k) (n * k)

-- | @since 0.1
instance MSpace TimeSpec Natural where
  ts .% k = fromSeconds (toSeconds ts / fromIntegral k)

-- | @since 0.1
instance Semimodule TimeSpec Natural

-- | @since 0.1
instance SemivectorSpace TimeSpec Natural

-- | @since 0.1
instance Normed TimeSpec where
  norm = id

-- | Converts a 'Double' to a 'TimeSpec'.
--
-- @since 0.1
fromSeconds :: Double -> TimeSpec
fromSeconds d =
  MkTimeSpec
    { sec = seconds,
      nsec = nanoseconds
    }
  where
    (seconds, remainder) = properFractionDouble d
    nanoseconds = floor $ remainder * 1_000_000_000

-- | Converts 'Natural' nanoseconds to a 'TimeSpec'.
--
-- @since 0.1
fromNanoSeconds :: Natural -> TimeSpec
fromNanoSeconds nanoseconds = MkTimeSpec s ns
  where
    (s, ns) = quotRem nanoseconds 1_000_000_000

-- | Converts a 'TimeSpec' to a 'Double'.
--
-- @since 0.1
toSeconds :: TimeSpec -> Double
toSeconds (MkTimeSpec s n) =
  fromIntegral s + (fromIntegral n / 1_000_000_000)

-- | Converts a 'TimeSpec' into 'Natural' nanoseconds.
--
-- @since 0.1
toNanoSeconds :: TimeSpec -> Natural
toNanoSeconds (MkTimeSpec s n) = (s * 1_000_000_000) + n

-- | Returns the absolute difference of two 'TimeSpec's.
--
-- @since 0.1
diffTimeSpec :: TimeSpec -> TimeSpec -> TimeSpec
diffTimeSpec t1 t2
  | t1' >= t2' = fromNanoSeconds (t1' - t2')
  | otherwise = fromNanoSeconds (t2' - t1')
  where
    t1' = toNanoSeconds t1
    t2' = toNanoSeconds t2

-- | Normalizes nanoseconds < 1 second.
--
-- @since 0.1
normalizeTimeSpec :: TimeSpec -> TimeSpec
normalizeTimeSpec = fromNanoSeconds . toNanoSeconds

#if !MIN_VERSION_base(4,17,0)
properFractionDouble :: Integral b => Double -> (b, Double)
{-# NOINLINE [1] properFractionDouble #-}
properFractionDouble x =
  case decodeFloat x of
    (m, n) ->
      if n >= 0
        then (fromInteger m * 2 ^ n, 0.0)
        else case quotRem m (2 ^ negate n) of
          (w, r) ->
            (fromInteger w, encodeFloat r n)
#endif
