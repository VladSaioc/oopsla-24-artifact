module Pipeline.VIRGoTranslation.Invariant.ChannelBound (channelBounds) where

import Backend.Ast
import Backend.Utilities
import Pipeline.VIRGoTranslation.Summary.Chan

{- | Composes all channel bound invariants.
Depends on: 𝜅, 𝒞

Produces:

> ∀ c ∈ 𝒞. channelBound(c)
-}
channelBounds :: [𝒞] -> [Exp]
channelBounds = map channelBound

{- | Constructs a channel bound invariant.
Depends on: 𝜓, (𝑛, p, c, e)

Produces:

> 𝜅(c) ≥ 0 ⟹ if 𝜅(c) > 0 then 0 ≤ c ∧ c ≤ 𝜅(c) else c in {1, 0, -1}
-}
channelBound :: 𝒞 -> Exp
channelBound 𝒞{c𝐶 = c, cCap = k} =
  let -- 0 ≤ c ∧ c ≤ 𝜅(c)
      asyncBound = (0 #) :<= (c @) :&& ((c @) :<= k)
      -- c ∈ {1, 0, -1}
      syncBound = In (c @) (ESet [(1 #), (0 #), ((-1) #)])
   in -- 𝜅(c) ≥ 0 ⟹ if 𝜅(c) > 0 then 0 ≤ c ∧ c ≤ 𝜅(c) else c in {1, 0, -1}
      (k :>= (0 #)) :==> IfElse (k :> (0 #)) asyncBound syncBound
