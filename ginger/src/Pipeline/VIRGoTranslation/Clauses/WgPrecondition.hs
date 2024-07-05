module Pipeline.VIRGoTranslation.Clauses.WgPrecondition (projectedConcurrency) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb

import Backend.Ast
import Backend.Utilities
import VIRGo.Utilities
import Pipeline.VIRGoTranslation.Clauses.Utilities
import Pipeline.VIRGoTranslation.Summary.Loop
import Pipeline.VIRGoTranslation.Summary.Summary
import Pipeline.VIRGoTranslation.Summary.WgOp
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

-- | Creates a binding from WaitGroups to expressions projecting the number of WaitGroup operations,
-- from which WaitGroup preconditions may be constructed.
projectedConcurrency :: 𝛹 -> ℳ -> 𝑋 ↦ (WgOpType ↦ Exp)
projectedConcurrency 𝜓 ℳ { ls, ws } =
  let
      -- Gather precondition contributions for every WaitGroup for
      -- operations in loop statements.
      lR = M.unionsWith (M.unionWith (:+)) (L.map (loopToPre 𝜓) ls)
      -- Gather precondition contributions for every channel for
      -- operations outside loops.
      nR = noloopOpToPre 𝜓 ws
      -- Combine the sets of channel names.
      cs = M.keys lR `L.union` M.keys nR
      -- Construct a precondition for WaitGroup w.
      prc w =
        let -- Get channel precondition sub-expressions from map.
            cR r = Mb.fromMaybe M.empty (M.lookup w r)
            -- Get precondition sub-expressions for channel operation orientation.
            cdR d r = Mb.fromMaybe (0 #) (M.lookup d r)
            -- Get loop and non-loop precondition sub-expressions.
            (clR, cnR) = (cR lR, cR nR)

            -- Get send sub-expressions.
            adds = cdR A clR :+ cdR A cnR
         in -- Bind the WaitGroup to the number of add operations it is projected to compute.
            (w, M.fromList [(A, adds)])
   in M.fromList $ L.map prc cs

{- | Constructs the concurrency operation projection resulting from loop WaitGroup operations.
Depends on: ℓ = (x, e₁, e₂, O)

Reachability conditions for all processes:

>  𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]

Produces:

> [ c ↦ [
>   Add ↦ (∀ w.Add(e') ∈ O. 𝛴 e') * iterations(e₁, e₂)
> ] | c ∈ chans(O)]
-}
loopToPre :: 𝛹 -> ℒ -> 𝑋 ↦ (WgOpType ↦ Exp)
loopToPre 𝜓 (ℒ {lP = p, l𝑛 = 𝑛, lower, upper, l𝒲s = os}) =
  let iter A ops =
        let b = 𝜓 M.! p M.! 𝑛
            loopLoad = (map wE ops ...+) :* iterations lower upper
         in IfElse b loopLoad (0 #)
      iter _ _ = (0 #)
   in M.map (M.mapWithKey iter) os

{- | Constructs the concurrency operation projection resulting from non-loop WaitGroup operations.
Depends on: O

Reachability conditions for all processes:

>  𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]

Produces:

> [ w ↦ [
>   Add ↦ (𝛴 (p, 𝜙) ∈ dom(𝛯). ∀ 𝑛, (𝑛, w.Add(e')) ∈ 𝜙. if 𝜓(p)(𝑛) then e' else 0)
> ] | c ∈ wgs(O)]
-}
noloopOpToPre :: 𝛹 -> P ↦ (𝑋 ↦ 𝒲s) -> 𝑋 ↦ (WgOpType ↦ Exp)
noloopOpToPre 𝜓 pis =
  let op 𝒲 {wP = p, w𝑛 = 𝑛, wDir = d, wE = e} =
        let b = 𝜓 M.! p M.! 𝑛
         in case d of
            A -> IfElse b e (0 #)
            _ -> (0 #)
      pis' = (M.elems . M.map (M.map (M.map ((...+) . map op)))) pis
   in M.unionsWith (M.unionWith (:+)) pis'
