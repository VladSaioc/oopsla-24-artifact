module Pipeline.VIRGoTranslation.Clauses.CapPrecondition (capPreconditions) where

import Data.List qualified as L
import Data.Map qualified as M

import Backend.Ast
import Backend.Utilities
import Pipeline.VIRGoTranslation.Summary.Chan

-- | Aggregate all capacity preconditions.
capPreconditions :: 𝛫 -> [Exp]
capPreconditions = L.map capPrecondition . M.elems

{- | Constructs a precondition guaranteeing that the channel capacity
is valid i.e., its expression evaluates to a positive integer.
Depends on: 𝜅, c

Produces:

> 𝜅(c) ≥ 0
-}
capPrecondition :: Exp -> Exp
capPrecondition = ((0 #) :<=)
