module Pipeline.VIRGoTranslation.Invariant.RendezvousMutex where

import Data.Map qualified as M

import Backend.Ast
import Backend.Utilities
import VIRGo.Utilities
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Summary.CommOp
import Pipeline.VIRGoTranslation.Utilities

-- | Aggregates all rendezvous mutexes.
rendezvousMutexes :: 𝛯 -> [Exp]
rendezvousMutexes ps =
  let flattenMap2 = concatMap (concat . M.elems)
      opsMap = chanOpsMap ps
      allOps = flattenMap2 opsMap
      makeInv p c = [rendezvousMutex p c allOps]
      mes = M.mapWithKey (\p cs -> map (makeInv p) (M.keys cs)) opsMap
   in concatMap concat $ M.elems mes

{- | For the given channel and process, ensures that no other process may
simultaneously sit on the rendezvous point as long as the current process
also visits a rendezvous point.
Depends on: p, c, 𝒪

Let:

> E = ⋁ ∀ c ∈ { 𝜋(p) == 𝓃 + 1 | ∀ (p, c, !, 𝓃) ∈ 𝒪 }
> E' = ⋀ ∀ c ∈ { 𝜋(p')) != 𝓃 + 1 | ∀ (p', c, !, 𝓃) ∈ 𝒪, p' ≠ p }

Produces:

> E ==> E'
-}
rendezvousMutex :: P -> 𝐶 -> [𝒪] -> Exp
rendezvousMutex p c os =
  let pc = 𝜋 p
      -- Rendezvous points in this process
      thisProc 𝒪 {oP = p', o𝐶 = c', oDir = S, o𝑛 = 𝑛} =
        -- If the process and channel are the same
        if p' == p && c == c'
          then -- Model that the process is at the rendezvous point
            pc :== ((𝑛 + 1) #)
          else -- The operation is irrelevant
            (False ?)
      -- Other types of operations are irrelevant
      thisProc _ = (False ?)

      -- Rendezvous points in other processes
      otherProc 𝒪 {oP = p', o𝐶 = c', oDir = S, o𝑛 = 𝑛} =
        -- The processes are different, but the channel is the same
        if p' /= p && c == c'
          then 𝜋 p' :!= ((𝑛 + 1) #)
          else (True ?)
      -- Other types of operations are irrelevant
      otherProc _ = (True ?)
   in (map thisProc os ...⋁) :==> (map otherProc os ...⋀)
