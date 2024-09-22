module Backend.Utilities where

import Data.List qualified as L
import Data.Monoid qualified as Mo

import Backend.Ast
import Utilities.Collection

-- | An alias for variable names (as strings) to clarify type definitions
type 𝑋 = String

-- | The type of type environments
type 𝛤 = 𝑋 ↦ Type

{- | Composes lists of expressions under arbitrary binary
constructors, ⊕, with a user-provided zero element 𝟎.
Depends on: E = [e₁, ..., eₙ]

Produces:
  if |E| = 0. 𝟎
  if |E| > 0. ⨁ eᵢ ≠ 𝟎. eᵢ
-}
(⨁) :: Exp -> (Exp -> Exp -> Exp) -> [Exp] -> Exp
(⨁) zero' cons = \case
  [] -> zero'
  e : es ->
    if e == zero'
      then (zero' ⨁ cons) es
      else cons e ((zero' ⨁ cons) es)

-- | Compose lists of expressions under syntactic conjunction (&&).
(...⋀) :: [Exp] -> Exp
(...⋀) = (True ?) ⨁ (:&&)

-- | Compose lists of expressions under syntactic disjunction (||).
(...⋁) :: [Exp] -> Exp
(...⋁) = (False ?) ⨁ (:||)

-- | Compose lists of expressions under binary arithmetic.
arithmCompose :: (Exp -> Exp -> Exp) -> [Exp] -> Exp
arithmCompose = (⨁) (0 #)

-- | Compose lists of expressions under syntactic addition.
(...+) :: [Exp] -> Exp
(...+) = arithmCompose (:+)

-- | Construct the syntactical expression equivalent of an integer.
(#) :: Int -> Exp
(#) = ECon . CNum

-- | Construct the syntactical expression equivalent of a boolean.
(?) :: Bool -> Exp
(?) True = ECon CTrue
(?) False = ECon CFalse

-- | Construct the syntactical expression equivalent of a variable with the name as the given string.
(@) :: String -> Exp
(@) = EVar

-- | Check whether an expression contains any conditional statements
containsConditionals :: Exp -> Bool
containsConditionals e =
  let Mo.Any b = aggregateExpression (\case
        IfElse {} -> Mo.Any True
        Match {} -> Mo.Any True
        _ -> Mo.Any False) e
   in b

-- | Get the set of free variables in an expression.
aggregateExpression :: Monoid m => (Exp -> m) -> Exp -> m
aggregateExpression f e0 =
  let fold = aggregateExpression f
      variadic = foldl (<>) mempty . L.map fold
      bin e1 e2 = fold e1 <> fold e2
   in f e0 <> case e0 of
    Match e cs -> f e <> variadic (L.map snd cs)
    IfElse e1 e2 e3 -> variadic [e1, e2, e3]
    Exists _ e -> variadic [e]
    Forall _ e -> variadic [e]
    In e1 e2 -> bin e1 e2
    ESet es -> variadic es
    e1 :<==> e2 -> bin e1 e2
    e1 :==> e2 -> bin e1 e2
    e1 :&& e2 -> bin e1 e2
    e1 :|| e2 -> bin e1 e2
    e1 :== e2 -> bin e1 e2
    e1 :!= e2 -> bin e1 e2
    e1 :>= e2 -> bin e1 e2
    e1 :> e2 -> bin e1 e2
    e1 :<= e2 -> bin e1 e2
    e1 :< e2 -> bin e1 e2
    e1 :+ e2 -> bin e1 e2
    e1 :- e2 -> bin e1 e2
    e1 :* e2 -> bin e1 e2
    e1 :/ e2 -> bin e1 e2
    e1 :% e2 -> bin e1 e2
    Not e -> variadic [e]
    ETuple es -> variadic es
    Call _ es -> variadic es
    _ -> mempty
