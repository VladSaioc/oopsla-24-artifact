module Pipeline.VIRGoTranslation.Invariant.ChannelAsyncMonitor (asyncChannelMonitors) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe

import Backend.Ast
import Backend.Utilities
import VIRGo.Utilities
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Summary.CommOp
import Pipeline.VIRGoTranslation.Summary.Loop
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

{- | Retrieves all asynchronous channel monitor expressions by analyzing
all loop and non-loop channel oeprations. The produced expressions
represent a relationship between process progress and their impact
on the size of the channel buffer.

Depends on:
1. Reachability conditions for all processes:
    𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]
2. All program loops: ℒ
3. All non-loop operations:
    O = {(p, 𝑛, o) | (𝑛, o) ∉ op(ℓ), ℓ ∈ ℒ, (𝑛, o) ∈ 𝜙, (p, 𝜙) ∈ 𝛯 }

Produces:

> [ c ↦ e1 - e2 | ∀ c. (𝑛, cd) ∈ 𝜙, (p, 𝜙) ∈ 𝛯,
>    e1 =  𝛴 ∀ ℓ, (c, [! ↦ e']) ∈ loopMonitor(ℓ). e'
>        + 𝛴 (p, 𝑛, !) ∈ O, e' = noloopMonitor(p, 𝑛). e',
>    e2 =  𝛴 ∀ ℓ, (c, [? ↦ e']) ∈ loopMonitor(ℓ). e'
>        + 𝛴 (p, 𝑛, ?) ∈ O, e' = noloopMonitor(p, 𝑛). e' ]
-}
asyncChannelMonitors :: 𝛹 -> P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> 𝐶 ↦ Exp
asyncChannelMonitors 𝜓 os ls =
  let noloopSubexps = L.map snd (M.toList (M.map (noloopMonitors 𝜓) os))
      loopSubexps = L.map (loopMonitor 𝜓) ls
      subexps = M.unionsWith (M.unionWith (:+)) (noloopSubexps ++ loopSubexps)
      chanMonitor dir =
        let sendops = fromMaybe (0 #) (M.lookup S dir)
            recvops = fromMaybe (0 #) (M.lookup R dir)
         in sendops :- recvops
   in M.map chanMonitor subexps

{- | Monitor asynchronous channel buffer length by analyzing the operations in a loop.
It returns an expression representing the resource contribution of each channel
operated on in a loop.
Depends on:

I. Reachability conditions for all processes:
    𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]

II. ℓ = (p, O, x, e₁, e₂, 𝑛₀, 𝑛'), with the following properties:
1. p is the process id of the loop
2. O = {(𝑛₁, c₁{!,?}), ..., (𝑛ₘ, cₘ{!,?})} are loop channel operations.
3. x is the loop index variable
4. e₁ is the lower bound expression
5. e₂ is the upper bound expression
6. 𝑛₀ is the guard point
7. 𝑛' is the exit point

Produces:

> [ c ↦ [
>   ! ↦ if 𝜓(p)(𝑛₀) then
>           (x - e₁) * |{ c! | (𝑛, c!) ∈ O }|
>         + (𝛴 ∀(𝑛, c!) ∈ O.
>             if 𝑛 < 𝜋(p) < 𝑛' then 1 else 0)
>       else 0,
>   ? ↦ if 𝜓(p)(𝑛₀) then
>           (x - e₁) * |{ c? | (𝑛, c?) ∈ O }|
>         + (𝛴 ∀(𝑛, c?) ∈ O.
>             if 𝑛 < 𝜋(p) < 𝑛' then 1 else 0)
>       else 0 ]
>   | ∀ c, (𝑛, c{!,?}) ∈ O ]
-}
loopMonitor :: 𝛹 -> ℒ -> 𝐶 ↦ (CommOpType ↦ Exp)
loopMonitor 𝜓 ℒ {lP = p, l𝑋 = var, lower, l𝑛 = 𝑛, lExit = 𝑛', l𝒪s = chans} =
  let b = 𝜓 M.! p M.! 𝑛
      pc = 𝜋 p
      x = (var @)
      singleOp ch =
        let 𝒪 {o𝑛 = 𝑛ᵢ} = ch
            hasPassedOp = ((𝑛ᵢ #) :< pc) :&& (pc :< (𝑛' #))
         in IfElse hasPassedOp (1 #) (0 #)
      chanSubexp ops =
        let iterations = (x :- lower) :* (length ops #)
            ops' = L.map singleOp ops
         in IfElse b (iterations :+ (ops' ...+)) (0 #)
   in M.map (M.map chanSubexp) chans

{- | Organize and compose under addition all non-loop monitor
sub-expressions for every asynchronous channel for a given process.
Depends on: p, 𝜙

Produces:

> ⋃ ∀ c.
>   [c ↦ [
>     ! ↦ {if 𝑛 < 𝜋(p) then 1 else 0) | ∀(𝑛, c!) ∈ 𝜙(p) },
>     ? ↦ {if 𝑛 < 𝜋(p) then 1 else 0) | ∀(𝑛, c!) ∈ 𝜙(p) }
>   ]]
-}
noloopMonitors :: 𝛹 -> 𝐶 ↦ 𝒪s -> 𝐶 ↦ (CommOpType ↦ Exp)
noloopMonitors 𝜓 =
  let subexps = L.map (noloopMonitor 𝜓)
      setTransform = (...+) . subexps
   in M.map (M.map setTransform)

{- | Monitor sub-expression for a non-loop single asynchronous channel operation.
If the operation has occurred, its resource contribution is 1.
Depends on: p, 𝑛, where n ∈ dom(𝛯(p)), b (reachability condition)

Produces:

> if 𝜓(p)(𝑛) ∧ 𝑛 < 𝜋(p) then 1 else 0
-}
noloopMonitor :: 𝛹 -> 𝒪 -> Exp
noloopMonitor 𝜓 𝒪 {oP = p, o𝑛 = 𝑛} =
  let b = 𝜓 M.! p M.! 𝑛
      pc = 𝜋 p
      passed = (𝑛 #) :< pc
   in IfElse (b :&& passed) (1 #) (0 #)
