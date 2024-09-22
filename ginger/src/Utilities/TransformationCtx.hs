module Utilities.TransformationCtx where

-- | TransformCtx describes values that wrap source and object values
-- throughout the transformation process from the source to the object.
class TransformCtx m where
  source :: m a b -> a
  updateSource :: m a b -> c -> m c b
  object :: m a b -> b
  updateObject :: m a b -> c -> m a c

-- | Update source datum.
(>:) :: TransformCtx m => c -> m a b -> m c b
a >: ctx = updateSource ctx a

-- | Update object datum.
(<:) :: TransformCtx m => m a b -> c -> m a c
ctx <: a = updateObject ctx a

-- | Mark transformation as finished. Replace context source with ().
done :: Monad m => TransformCtx c => c a b -> m (c () b)
done ctx = return $ updateSource ctx ()
