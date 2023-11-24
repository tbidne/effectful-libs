-- | @since 0.1
module Effectful.Environment.Utils
  ( QueryExePath (..),
  )
where

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
  deriving stock (Eq, Show)
