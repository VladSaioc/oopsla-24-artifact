module Pipeline.VIRGoTranslation.Context.Reachability (reachability) where

import Data.Map qualified as M

import Backend.Ast qualified as T
import Backend.Utilities
import VIRGo.Ast
import Pipeline.VIRGoTranslation.Exps
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

{- | Computes the reachability conditions forr every instruction point
in every process.

For individual statements, it produces a binding from program points
to reachability conditions, given an instruction point.
Its return context also includes the instruction point offset
for possible continuations, and the aggregated condition leading to a return
statement.

Rules:

> [CHAN]:   ⟨e, 𝑛 : c = [e']⟩ -> ⟨𝑛 + 1, e : [𝑛 ↦ e]⟩

> [RETURN]: ⟨e, 𝑛 : return⟩ -> ⟨𝑛 + 1, e : [𝑛 ↦ e]⟩

> [SKIP]:   ⟨_, 𝑛 : skip⟩ -> ⟨𝑛, false : []⟩

> [FOR]:    ⟨_, 𝑛 : for _ _ os⟩ -> ⟨𝑛ₙ + 1, false : [𝑛 ↦ e]⟩
>           |- 𝑛₀ = 𝑛 + 1
>           |- ∀ 0 ≤ i < n, n = |os|, oᵢ ∈ os, 𝜓ᵢ = [𝑛ᵢ ↦ e].
>               ⟨e, 𝑛ᵢ : oᵢ⟩ -> ⟨𝑛ᵢ₊₁, false : 𝜓ᵢ⟩
>           |- 𝜓 = ⋃ ∀ 0 ≤ i < n. 𝜓ᵢ

> [GO]:     ⟨e, 𝑛 : go { S }⟩ -> ⟨𝑛 + 1, e : [𝑛 ↦ e]𝜓₁⟩
>           |- ⟨e, 0 : S⟩ -> ⟨𝑛₁, e₁ : 𝜓₁⟩

> [SEND]:   ⟨e, 𝑛 : c!⟩ -> ⟨𝑛 + 2, false : [𝑛 ↦ e]⟩

> [RECV]:   ⟨e, 𝑛 : c?⟩ -> ⟨𝑛 + 1, false : [𝑛 ↦ e]⟩

> [ADD]:    ⟨e, 𝑛 : w.Add(e)⟩ -> ⟨𝑛 + 1, false : [𝑛 ↦ e]⟩

> [WAIT]:   ⟨e, 𝑛 : w.Wait()⟩ -> ⟨𝑛 + 1, false : [𝑛 ↦ e]⟩

> [SEQ]:    ⟨e, 𝑛 : S₁; S₂⟩ -> ⟨𝑛₂, e₁ || e₂ : 𝜓₁ ∪ 𝜓₂⟩
>           |- ⟨e, 𝑛, S₁⟩ -> ⟨𝑛₁, e₁ : 𝜓₁⟩
>           |- ⟨!e₁ && e, 𝑛₁, S₂⟩ -> ⟨𝑛₂, e₂ : 𝜓₂⟩

> [IF]:     ⟨e, 𝑛 : if e' then S₁ else S₂⟩ -> ⟨𝑛₂, e₁ || e₂ : 𝜓₁ ∪ 𝜓₂⟩
>           |- ⟨e && e', 𝑛 : S₁⟩ -> ⟨𝑛₁, e₁ : 𝜓₁⟩
>           |- ⟨e && !e', S₂⟩ -> ⟨𝑛₂, e₂ : 𝜓₂⟩
-}
reachability :: 𝑆 -> 𝛹
reachability s₀=
  let (⊎) = M.unionWith M.union
      stmtReachability' e 𝜆@𝛬 { 𝑛 = 𝑛₀, p = p₀ } s =
        let 𝜆' = 𝜆 { 𝑛 = 𝑛 𝜆 + ppOffset s }
         in case s of
            -- Channel definition statements increment program counter,
            -- and do not result in an early return condition.
            Def {} -> (M.empty ⊔ (p 𝜆, 𝑛 𝜆, e), 𝜆', (False ?))
            -- Skip statements do not increment the program counter,
            -- and do not result in an early return condition.
            Skip -> (M.empty, 𝜆', (False ?))
            -- Close statements do not increment the program counter,
            -- and do not result in an early return condition.
            Close _ -> (M.empty, 𝜆', (False ?))
            -- Return statements are conditional based on the previous statement.
            -- They also stipulate an early return condition if their reachability
            -- condition is satisfied.
            Return -> (M.empty ⊔ (p 𝜆, 𝑛 𝜆, e), 𝜆', e)
            Seq s₁ s₂ ->
              let (𝜓₁, 𝜆₁, e₁) = stmtReachability' e 𝜆 s₁
                  (𝜓₂, 𝜆₂, e₂) = stmtReachability' (T.Not e₁ T.:&& e) 𝜆₁ s₂
               in (𝜓₁ ⊎ 𝜓₂, 𝜆₂, e₁ T.:|| e₂)
            For _ _ _ os ->
               let addOp (𝑛ᵢ, 𝜓ᵢ) o = (𝑛ᵢ + ppOffset o, 𝜓ᵢ ⇒ (𝑛ᵢ, e))
                   (𝑛', 𝜓ₚ) = foldl addOp (𝑛₀ + 1, M.empty ⇒ (𝑛₀, e)) os
               in  (M.fromList [(p₀, 𝜓ₚ)], 𝜆 { 𝑛 = 𝑛' + 1 }, (False ?))
            Atomic {} -> (M.empty ⊔ (p₀, 𝑛₀, e), 𝜆', (False ?))
            -- If statements may add additional reachability conditions to
            -- possibile return statements encountered along the branches.
            If e0 s₁ s₂ ->
              let e' = parseExp e0
                  -- The 'then' branch extends reachability with the guard condition.
                  𝜆₀ = 𝜆 { 𝑛 = 𝑛₀ + 1 }
                  (𝜓₁, 𝜆₁, e₁) = stmtReachability' (e T.:&& e') 𝜆₀ s₁
                  -- The 'else' branch extends reachability with the negated guard condition.
                  𝜆₁' = 𝜆₁ { 𝑛 = 𝑛 𝜆₁ + 1 }
                  (𝜓₂, 𝜆₂, e₂) = stmtReachability' (e T.:&& T.Not e') 𝜆₁' s₂
               in -- Program point reachability maps are joined.
                  -- The if exit point is the continuation point of the else branch.
                  -- Return conditions are a disjunction between return conditions
                  -- of the branches.
                  (𝜓₁ ⊎ 𝜓₂, 𝜆₂, e₁ T.:|| e₂)
            Go s₁ ->
              let -- Get next fresh process ID.
                  p₁ = nextp 𝜆
                  -- Create a new traversal context for goroutine body.
                  𝜆₀ = 𝛬 { 𝑛 = 0, p = p₁, nextp = p₁ + 1 }
                  -- Compute reachability for instructions inside goroutine.
                  (𝜓₁, 𝜆₁, _) = stmtReachability' e 𝜆₀ s₁
                  -- Add reachability of 'go' instruction itself.
                  𝜓₁' = 𝜓₁ ⊔ (p₀, 𝑛₀, e)
               in -- 'go' instruction does not result in an early return condition.
                  -- Traversal context for continuation only inherits the next
                  -- available goroutine name.
                  (𝜓₁', 𝜆' { nextp = nextp 𝜆₁ }, (False ?))
      (𝜓, _, _) = stmtReachability' (True ?) 𝛬 { 𝑛 = 0, p = 0, nextp = 1} s₀
  in 𝜓
