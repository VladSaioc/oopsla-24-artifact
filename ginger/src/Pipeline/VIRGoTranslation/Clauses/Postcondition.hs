module Pipeline.VIRGoTranslation.Clauses.Postcondition (postcondition) where

import Backend.Ast
import Backend.Utilities
import Pipeline.VIRGoTranslation.Summary.Go
import Pipeline.VIRGoTranslation.Utilities

{- | Constructs postconditions for all the processes.
Depends on: 𝜓, 𝛯, 𝒢

Produces:

> 𝜋(0) = 𝜏(0) ∧ ⋀ ∀ (p', p, 𝑛) ∈ 𝒢. 𝜋(p) ≠ -1 ⟹ 𝜋(p) = 𝜏(p)
-}
postcondition :: [𝒢] -> Exp
postcondition gs = (((𝜋 0 :== 𝜏 0) : map goPostcondition gs)  ...⋀)

{- | Constructs a postcondition from a given go instruction.
Depends on: 𝜓, 𝛯, (p₀, p, 𝑛).

Produces:

> 𝜋(p) ≠ -1 ⟹ 𝜋(p) == 𝜏(p)
-}
goPostcondition :: 𝒢 -> Exp
goPostcondition 𝒢 { gP' = p' } = (𝜋 p' :!= ((-1) #)) :==> (𝜋 p' :== 𝜏 p')
