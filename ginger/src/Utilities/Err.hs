module Utilities.Err where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), liftM)
import Data.Map qualified as M
import Data.Maybe qualified as Mb

-- | Error monad with a string message.
data Err a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Monad Err where
  return = pure
  Ok a >>= f = f a
  Bad s >>= _ = Bad s

instance MonadFail Err where
  fail = Bad

instance Applicative Err where
  pure = Ok
  (Bad s) <*> _ = Bad s
  (Ok f) <*> o = fmap f o

instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad "Error message"
  mplus (Bad s) _ = Bad s
  mplus _ (Bad s) = Bad s
  mplus x _ = x

instance Alternative Err where
  empty = mzero
  (<|>) = mplus

-- | Checks a sequence of guards and returns the first error message if any guard is false.
multiGuard :: [(Bool, String)] -> Err ()
multiGuard = \case
  [] -> return ()
  (g, msg) : guards ->
    if not g
      then multiGuard guards
      else Bad msg

-- | Look up key in map and fail with a message if key is not found.
mlookup :: (MonadFail m, Ord p) => String -> p -> M.Map p a -> m a
mlookup errmsg k m = do
  let v = M.lookup k m
  Mb.maybe (fail errmsg) pure v
