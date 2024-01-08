-- | @since 0.1
module Effectful.Environment.Utils
  ( QueryExePath (..),
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | Result of querying for the executable path.
--
-- @since 0.1
data QueryExePath
  = -- | If the system does not provide a reliable way to determine the
    -- current executable.
    --
    -- @since 0.1
    NoQuery
  | -- | The result of querying the executable name.
    --
    -- @since 0.1
    QueryResult (Maybe FilePath)
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )
