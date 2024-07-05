module Pipeline.VIRGoTranslation.Invariant.ChannelMonitor (channelMonitors) where

import Data.Map qualified as M
import Data.Maybe qualified as Mb

import Backend.Ast
import Backend.Utilities
import Pipeline.VIRGoTranslation.Invariant.ChannelAsyncMonitor (asyncChannelMonitors)
import Pipeline.VIRGoTranslation.Invariant.ChannelSyncMonitor (syncChannelMonitors)
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Summary.CommOp
import Pipeline.VIRGoTranslation.Summary.Loop
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

{- | Produces channel monitors by composing both buffered and
unbuffered channel behaviour under an expression conditional
over the capacity expression.

Depends on:

> 1. Reachability conditions for all processes:
>     𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]
> 2. All program loops: ℒ
> 3. All non-loop operations:
>     O = {(p, 𝑛, o) | (𝑛, o) ∉ op(ℓ), ℓ ∈ ℒ, (𝑛, o) ∈ 𝜙, (p, 𝜙) ∈ 𝛯 }
> 4. Channel capacity environments: 𝜅
>
> ∀ c, e1 = syncChannelMonitor(𝜓, O, ℒ)(c),
>      e2 = asyncChannelMonitor(𝜓, O, ℒ)(c).
>     c = if 𝜅(c) > 0 then e2 else e1
-}
channelMonitors :: 𝛹 -> 𝛫 -> P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> [Exp]
channelMonitors 𝜓 𝜅 os ls =
  let syncMs = syncChannelMonitors 𝜓 os ls
      asyncMs = asyncChannelMonitors 𝜓 os ls
      combineMonitors c s a =
        let cap = Mb.fromMaybe (0 #) (M.lookup c 𝜅)
            isAsync = cap :> (0 #)
         in (c @) :== IfElse isAsync a s
   in M.elems $ M.unionWithKey combineMonitors syncMs asyncMs
