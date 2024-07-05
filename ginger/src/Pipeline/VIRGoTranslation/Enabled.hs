module Pipeline.VIRGoTranslation.Enabled (enabledExp) where

import Data.Map qualified as M
import Data.Maybe qualified as Mb

import Backend.Ast
import Backend.Utilities
import VIRGo.Utilities
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Summary.CommOp
import Pipeline.VIRGoTranslation.Summary.WgOp
import Pipeline.VIRGoTranslation.Utilities

{- | Composes the enabled predicates for all processes
under disjunction.
Depends on: 𝜅, 𝛯

Produces:

> ⋁ (p, 𝜙) ∈ 𝛯. enabled(𝜅, p, 𝜙)
-}
enabledExp :: 𝛫 -> 𝛯 -> Exp
enabledExp 𝜅 = (...⋁) . M.elems . M.mapWithKey (enabled 𝜅)

{- | Computes an enabled predicate for a given process.
Depends on: 𝜅, p, 𝜙

Let the following:

> C! = ⋃ ∀ (c, !, 𝑛) ∈ chanOps(𝜙). [
>    𝑛 ↦ if 0 < 𝜅(c) then c < 𝜅(c) else c == 0,
>    (𝑛 + 1) ↦ c == -1
>  ]
> C? = ⋃ ∀ (c, !, 𝑛) ∈ chanOps(𝜙). [
>    𝑛 ↦ if 0 < 𝜅(c) then c > 0 else c == 1
>  ]
> Wait(W) = ⋃ ∀ (w, Wait, 𝑛) ∈ wgOps(𝜙). [
>    𝑛 ↦ w == 0
>  ]

Produces:

> match 𝜋(p) {
> ∀ (𝑛, e) ∈ C! ∪ C?. case 𝑛 => e
> case _ => -1 < 𝜋(p) < (max ∘ dom)(𝜙)
> }
-}
enabled :: 𝛫 -> P -> 𝛷 -> Exp
enabled 𝜅 p 𝜙 =
  let -- Process id variable
      pc = 𝜋 p
      -- Construct match over process id
      match cs = Match pc (cs ++ [(Wildcard, ((-1) #) :< pc :< 𝜏 p)])
      chsops = processChanOps p 𝜙
      wgops = processWgOps p 𝜙
      -- Process has not reached termination point
      subExpCh 𝒪 {o𝐶 = cn, o𝑛 = 𝑛, oDir = d} =
        let k = Mb.fromJust (M.lookup cn 𝜅)
            c = (cn @)

            -- If the process is at instruction 𝑛', check e
            -- case 𝑛' => e
            executing 𝑛' e' = (PCon (CNum 𝑛'), e')
            -- Check for the buffered case for capacity k:
            -- if 0 < k then e1 else e2
            bufCase = IfElse ((0 #) :< k)

            opEnabled = case d of
              -- Send operations are enabled if:
              -- 1. Buffered case: the channel is not full
              -- 2. Unbuffered case:
              -- -- The channel is available to write to.
              -- -- Rendezvous has been established
              S ->
                [ executing 𝑛 $ bufCase (c :< k) (c :== (0 #)),
                  executing (𝑛 + 1) (c :== ((-1) #))
                ]
              -- Receive operations are enabled if:
              -- 1. Buffered case: the channel is not empty
              -- 2. Unbuffered case: the channel is available to read from.
              R ->
                [ executing 𝑛 $ bufCase (c :> (0 #)) (c :== (1 #))
                ]
         in opEnabled
      -- Process has not reached termination point
      subExpWg 𝒲 {w𝐶 = cn, w𝑛 = 𝑛, wDir = d} =
        let c = (cn @)
            -- If the process is at instruction 𝑛', check e
            -- case 𝑛' => e
            executing 𝑛' e' = (PCon (CNum 𝑛'), e')
            opEnabled = case d of
              -- Wait operations are enabled if the WaitGroup counter is 0.
              W -> [executing 𝑛 (c :== (0 #))]
              _ -> []
         in opEnabled
   in match $ concatMap subExpCh chsops ++ concatMap subExpWg wgops
