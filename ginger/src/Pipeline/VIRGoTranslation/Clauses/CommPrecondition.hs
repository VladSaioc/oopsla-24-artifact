module Pipeline.VIRGoTranslation.Clauses.CommPrecondition (projectedCommunication) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb

import Backend.Ast
import Backend.Utilities
import VIRGo.Utilities
import Pipeline.VIRGoTranslation.Clauses.Utilities
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Summary.CommOp
import Pipeline.VIRGoTranslation.Summary.Loop
import Pipeline.VIRGoTranslation.Summary.Summary
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

-- | Creates a binding from channels to expressions projecting the number of channel operations,
-- from which channel preconditions may be constructed.
projectedCommunication :: 𝛹 -> ℳ -> 𝐶 ↦ (CommOpType ↦ Exp)
projectedCommunication 𝜓 ℳ { ls, os } =
  let
      -- Gather precondition contributions for every channel for
      -- operations in loop statements.
      lR = M.unionsWith (M.unionWith (:+)) (L.map (loopToPre 𝜓) ls)
      -- Gather precondition contributions for every channel for
      -- operations outside loops.
      nR = noloopOpToPre 𝜓 os
      -- Combine the sets of channel names.
      cs = M.keys lR `L.union` M.keys nR
      -- Construct a precondition for channel c.
      prc c =
        let -- Get channel precondition sub-expressions from map.
            cR r = Mb.fromMaybe M.empty (M.lookup c r)
            -- Get precondition sub-expressions for channel operation orientation.
            cdR d r = Mb.fromMaybe (0 #) (M.lookup d r)
            -- Get loop and non-loop precondition sub-expressions.
            (clR, cnR) = (cR lR, cR nR)
            -- Get send sub-expressions.
            sends = cdR S clR :+ cdR S cnR
            -- Get receive sub-expressions.
            recvs = cdR R clR :+ cdR R cnR
         in -- Bind the channel to the number of receive and send operations it
            -- is projected to compute.
            (c, M.fromList [(R, recvs), (S, sends)])
   in M.fromList $ L.map prc cs

{- | Constructs the communication operation projection resulting from
loop channel operations.
Depends on: ℓ = (x, e₁, e₂, [𝒪])

Reachability conditions for all processes:

>  𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]

Produces:

> [ c ↦ [
>   ! ↦ |𝒪! ⊆ 𝒪| * iterations(e₁, e₂)
>   ? ↦ |𝒪? ⊆ 𝒪| * iterations(e₁, e₂)
> ] | c ∈ chans(𝒪)]
-}
loopToPre :: 𝛹 -> ℒ -> 𝐶 ↦ (CommOpType ↦ Exp)
loopToPre 𝜓 (ℒ {lP = p, l𝑛 = 𝑛, lower, upper, l𝒪s = os}) =
  let iter ops =
        let b = 𝜓 M.! p M.! 𝑛
            e = case length ops of
              0 -> (0 #)
              1 -> iterations lower upper
              n -> (n #) :* iterations lower upper
         in IfElse b e (0 #)
   in M.map (M.map iter) os

{- | Constructs the communication operation projection resulting from
non-loop channel operations.
Depends on: O

Reachability conditions for all processes:

>  𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]

Produces:

> [ c ↦ [
>   ! ↦ (𝛴 (p, 𝜙) ∈ dom(𝛯). ∀ 𝑛, (𝑛, c!) ∈ 𝜙. if 𝜓(p)(𝑛) then 1 else 0)
>   ? ↦ (𝛴 (p, 𝜙) ∈ dom(𝛯). ∀ 𝑛, (𝑛, c?) ∈ 𝜙. if 𝜓(p)(𝑛) then 1 else 0)
> ] | c ∈ chans(O)]
-}
noloopOpToPre :: 𝛹 -> P ↦ (𝐶 ↦ 𝒪s) -> 𝐶 ↦ (CommOpType ↦ Exp)
noloopOpToPre 𝜓 pis =
  let chOp 𝒪 {oP = p, o𝑛 = 𝑛} =
        let b = 𝜓 M.! p M.! 𝑛
         in IfElse b (1 #) (0 #)
      pis' = (M.elems . M.map (M.map (M.map ((...+) . map chOp)))) pis
   in M.unionsWith (M.unionWith (:+)) pis'
