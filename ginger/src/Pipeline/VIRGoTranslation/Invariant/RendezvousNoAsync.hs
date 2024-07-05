module Pipeline.VIRGoTranslation.Invariant.RendezvousNoAsync (noAsyncRendezvous) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb

import Backend.Ast
import Backend.Utilities
import VIRGo.Utilities
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Summary.CommOp
import Pipeline.VIRGoTranslation.Summary.Loop
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

{- | For every channel send operation of every process, adds
an invariant clause stipulating that the process never reaches
the rendezvous instruction if the channel is buffered.
-}
noAsyncRendezvous :: 𝛫 -> P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> [Exp]
noAsyncRendezvous 𝜅 os ls =
  let atomicRvC = M.map (chanopsToRendezvous 𝜅)
      atomicRvs = concat $ concatMap M.elems (M.map atomicRvC os)
      loopRvs = concatMap (loopToNoRendezvous 𝜅) ls
   in atomicRvs ++ loopRvs

-- | Collects all rendezvous negations for operations in loops.
loopToNoRendezvous :: 𝛫 -> ℒ -> [Exp]
loopToNoRendezvous 𝜅 ℒ {l𝒪s} =
  let invs = M.map (chanopsToRendezvous 𝜅) l𝒪s
   in concat $ M.elems invs

-- | Collects all rendezvous negations for operations outside loops.
chanopsToRendezvous :: 𝛫 -> 𝒪s -> [Exp]
chanopsToRendezvous 𝜅 =
  let sends = Mb.fromMaybe [] . M.lookup S
   in L.map (sendToNoRendezvous 𝜅) . sends

{- | Creates an invariant sub-expression stipulating that the program
counter will never reach rendezvous points if the channel is buffered.
Depends on: 𝜅, 𝜙, p

Produces:

> 0 < 𝜅(c) => 𝜋(p) != n + 1
-}
sendToNoRendezvous :: 𝛫 -> 𝒪 -> Exp
sendToNoRendezvous 𝜅 𝒪 {oP = p, o𝐶 = c, o𝑛 = 𝑛} =
  let pc = 𝜋 p
      k = Mb.fromMaybe (0 #) (M.lookup c 𝜅)
   in ((0 #) :< k) :==> (pc :!= ((𝑛 + 1) #))
