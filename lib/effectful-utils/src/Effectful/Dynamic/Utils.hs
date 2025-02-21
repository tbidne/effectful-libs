-- | @since 0.1
module Effectful.Dynamic.Utils
  ( ShowEffect (..),
  )
where

import Data.Kind (Constraint, Type)

-- | Class for "showing" a dynamic effect. This class is intended for
-- debugging or error handling.
type ShowEffect :: ((Type -> Type) -> Type -> Type) -> Constraint
class ShowEffect p where
  -- | Shows an effect constructor. Useful for stubbing mock effects e.g.
  --
  -- @
  --   runEffect = interpret_ $ \case
  --     Cons1 x -> ...
  --     Cons2 x y -> ...
  --     other -> error $ (showEffectCons other) ++ ": unimplemented"
  -- @
  --
  -- We may not want to provide an implementation for all members of a
  -- mock effect e.g. because we want to verify a specific member is _not_
  -- used, or implementing everything is unnecessarily cumbersome.
  --
  -- Erroring out is a reasonable choice, though it can lead to cryptic errors
  -- if such a member _is_ (unexpectedly) used and subsequently fails.
  -- Thus this show instance can be used to provide a better error message.
  showEffectCons :: p m a -> String
