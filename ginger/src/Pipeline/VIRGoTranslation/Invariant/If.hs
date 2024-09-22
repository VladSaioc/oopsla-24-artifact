module Pipeline.VIRGoTranslation.Invariant.If (ifMonitors) where

import Backend.Ast
import Backend.Utilities
import Pipeline.VIRGoTranslation.Summary.If
import Pipeline.VIRGoTranslation.Utilities

{- | Get all if monitor expressions.
-}
ifMonitors :: [ℐ] -> [Exp]
ifMonitors = map ifMonitor

{- | Constructs an if monitor invariant.
Depends on: 𝒾 = (p, e, 𝑛₁, 𝑛₂, 𝑛₃)

Produces:

> if e then ¬(𝑛₂ ≤ 𝜋(p) < 𝑛₃) else ¬(𝑛₁ < 𝜋(p) < 𝑛₂)
-}
ifMonitor :: ℐ -> Exp
ifMonitor (ℐ {iP = p, iGuard = b, i𝑛 = 𝑛₁, iElse = 𝑛₂, iExit = 𝑛₃}) =
  let -- Program counter as a back-end variable
      pc = 𝜋 p
      -- If guard point as a fixed program point
      guard = (𝑛₁ #)
      -- If else point as a fixed program point
      els = (𝑛₂ #)
      -- If exit point as a fixed program point
      exit = (𝑛₃ #)
      counterInThen = (guard :< pc) :&& (pc :< els)
      counterInElse = (els :<= pc) :&& (pc :< exit)
   in IfElse b (Not counterInElse) (Not counterInThen)
