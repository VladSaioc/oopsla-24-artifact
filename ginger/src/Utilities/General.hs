module Utilities.General (results, foldMonad, binaryCons, unaryCons, fix) where

import Control.Monad
import Data.Functor ( (<&>) )

-- | Converts a list of monad values, into a monad wrapping a list.
results :: Monad m => [m a] -> m [a]
results [] = return []
results (r : rs) = do
  rv <- r
  rs' <- results rs
  return (rv : rs')

-- | Fold over a foldable structure to construct a monadic value.
foldMonad :: (Foldable t1, Monad m) => (t2 -> m t3) -> t4 -> (t4 -> t3 -> t4) -> t1 t2 -> m t4
foldMonad f start combine = Control.Monad.foldM (\b a -> f a <&> combine b) start

-- | Lifts a binary operation to a monad.
binaryCons :: Monad m => (a -> m b) -> (b -> b -> c) -> a -> a -> m c
binaryCons p cons e1 e2 = do
  e1' <- p e1
  e2' <- p e2
  return (cons e1' e2')

-- | Lifts a unary operation to a monad.
unaryCons :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
unaryCons p cons e = do
  e' <- p e
  return (cons e')

-- | Fixed point combinator.
fix :: Eq a => (a -> a) -> a -> a
fix f a = let a' = f a in if a' == a then a else fix f a'
