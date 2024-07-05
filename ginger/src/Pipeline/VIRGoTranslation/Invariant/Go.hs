module Pipeline.VIRGoTranslation.Invariant.Go (goMonitors) where

import Data.Map qualified as M

import Backend.Ast
import Backend.Utilities
import Pipeline.VIRGoTranslation.Summary.Go
import Pipeline.VIRGoTranslation.Utilities

{- | Get all go monitor expressions.
-}
goMonitors :: 𝛹 -> [𝒢] -> [Exp]
goMonitors 𝜓 = map (goMonitor 𝜓)

{- | Constructs a 'go' monitor invariant.
It states that, if the reachability conditions for the go instruction
are met, and after the go instruction has been executed by the parent process,
the child process is no longer waiting to start (at -1).
If the reachability conditions are not met, the process will remainin
in waiting to start mode until the end of the program.
Depends on: 𝜓, g = (p, p', 𝑛)

Produces:

> 𝜓(p)(𝑛) ∧ 𝑛 < 𝜋(p) <==> -1 < 𝜋(p')
-}
goMonitor :: 𝛹 -> 𝒢 -> Exp
goMonitor 𝜓 (𝒢 { gP = p, gP' = p', g𝑛 = 𝑛 }) = ((𝜓 M.! p M.! 𝑛) :&& (𝜋 p :> (𝑛 #))) :<==> (((-1) #) :< 𝜋 p')
