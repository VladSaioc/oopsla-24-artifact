module Pipeline.VIRGoTranslation.Invariant.Loop (loopMonitors) where

import Data.Map qualified as M

import Backend.Ast
import Backend.Utilities
import Pipeline.VIRGoTranslation.Summary.Loop
import Pipeline.VIRGoTranslation.Utilities

{- | Get all loop monitors for every loop.
-}
loopMonitors :: 𝛹 -> [ℒ] -> [Exp]
loopMonitors 𝜓 = map (loopMonitor 𝜓)

{- | Constructs a loop monitor invariant.
Depends on:

I. Reachability conditions for all processes:
    𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]

II. ℓ = (p, x, 𝑛, 𝑛', e₁, e₂) with the following properties:

1. p is the process id of the loop
2. x is the loop index variable
3. e₁ is the lower bound expression
4. e₂ is the upper bound expression
5. 𝑛 is the guard point
6. 𝑛' is the exit point

Produces:

> if 𝜓(p)(𝑛) && e₁ ≤ e₂ then
>     e₁ ≤ x ≤ e₂
>   ∧ 𝜋(p) < 𝑛 => x = e₁ ∧
>   ∧ 𝑛 < 𝜋(p) < 𝑛' => x < e₂ ∧
>   ∧ 𝑛' ≤ 𝜋(p) => x = e₂
> else x = e₁ ∧ ¬(𝑛 < 𝜋(p) < 𝑛')
-}
loopMonitor :: 𝛹 -> ℒ -> Exp
loopMonitor 𝜓 (ℒ {l𝑋 = var, lP = p, l𝑛 = 𝑛, lExit = 𝑛', lower, upper}) =
  let b = 𝜓 M.! p M.! 𝑛
      -- Short-hand for lower bound
      lo = lower
      -- Short-hand for upper bound
      hi = upper
      -- Loop variable as a back-end variable
      x = (var @)
      -- Program counter as a back-end variable
      pc = 𝜋 p
      -- Loop guard point as a fixed program point
      guard = (𝑛 #)
      -- Loop exit point as a fixed program point
      exit = (𝑛' #)
      counterInLoop = (guard :< pc) :&& (pc :< exit)
      -- Initial guard checks whether the loop will be entered at all.
      -- If the lower bound is already strictly higher than the upper bound,
      -- or the loop is unreachable due to path conditions, then no iterations
      -- are performed.
      initGuard = b :&& (upper :>= lower)
      -- The clauses modeling loop behaviour when it has 0 or more
      -- iterations.
      hasIter =
        let -- Loop counter is bounded: lo ≤ x ≤ hi
            bounded = (lo :<= x) :&& (x :<= hi)
            -- Counter value before guard is reached: 𝜋(p) < n => x = lo
            before = (pc :< guard) :==> (x :== lo)
            -- Counter value while loop is executing: n < 𝜋(p) < n' => x < hi
            during = counterInLoop :==> (x :< hi)
            -- Counter value after loop has executed: n' ≤ 𝜋(p) => x = hi
            after = (exit :<= pc) :==> (x :>= hi)
         in ([bounded, before, during, after] ...⋀)
      -- The clause modeling loop behaviour if the loop never enters.
      noIter =
        let -- The incrementing variable will remain frozen to the lower bound.
            bounded = x :== lower
         in -- The loop counter will never reach loop body program points in these scenarios.
            bounded :&& Not counterInLoop
   in IfElse initGuard hasIter noIter
