module Pipeline.Verification.Dafny (encodingToDafny, iterationsFunc) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S

import Backend.Ast
import Backend.Utilities
import Backend.Simplifier
import Pipeline.VIRGoTranslation.Clauses.Utilities
import Pipeline.VIRGoTranslation.Enabled (enabledExp)
import Pipeline.VIRGoTranslation.Encoding
import Pipeline.VIRGoTranslation.Invariant.ChannelBound (channelBounds)
import Pipeline.VIRGoTranslation.Invariant.ChannelDef (channelDefs)
import Pipeline.VIRGoTranslation.Invariant.ChannelMonitor (channelMonitors)
import Pipeline.VIRGoTranslation.Invariant.CounterBound (counterInvariants)
import Pipeline.VIRGoTranslation.Invariant.If (ifMonitors)
import Pipeline.VIRGoTranslation.Invariant.Loop (loopMonitors)
import Pipeline.VIRGoTranslation.Invariant.Go (goMonitors)
import Pipeline.VIRGoTranslation.Invariant.RendezvousMutex (rendezvousMutexes)
import Pipeline.VIRGoTranslation.Invariant.RendezvousNoAsync (noAsyncRendezvous)
import Pipeline.VIRGoTranslation.Invariant.Return (returnMonitors)
import Pipeline.VIRGoTranslation.Invariant.WgBound (wgBounds)
import Pipeline.VIRGoTranslation.Invariant.WgMonitor (wgMonitors)
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Summary.Loop
import Pipeline.VIRGoTranslation.Summary.Summary
import Pipeline.VIRGoTranslation.Summary.WgOp
import Pipeline.VIRGoTranslation.Utilities
import Pipeline.Verification.Strategy

{- | A function for computing the number of iterations that
may be performed in a loop.

Produces:

> ghost function iterations(lo, hi) : int {
>   if lo <= hi then hi - lo else 0
> }
-}
iterationsFunc :: Function
iterationsFunc =
  let lo = ("lo" @)
      hi = ("hi" @)
   in Function
        { yields = TInt,
          funcBody = IfElse (lo :<= hi) (hi :- lo) (0 #),
          funcHoare =
            HoareWrap
              { ghost = True,
                name = 𝑥iter,
                types = [],
                params = [("lo", TInt), ("hi", TInt)],
                requires = [],
                ensures = [],
                decreases = []
              }
        }

{- | A predicate on schedules that ensures all schedule steps
are bound to valid process IDs.
Depends on: 𝛯

Produces:

> ghost function isSchedule(S : nat -> nat) {
>   forall n :: s <= |dom(𝛯)|
> }
-}
isScheduleFunc :: 𝛯 -> Function
isScheduleFunc 𝜉 =
  let n = "n"
      domPi = ((M.size 𝜉 - 1) #)
      callS = Call "S" [(n @)]
   in Function
        { yields = TBool,
          funcBody = Forall [(n, Nothing)] (callS :<= domPi),
          funcHoare =
            HoareWrap
              { ghost = True,
                name = "isSchedule",
                types = [],
                params = [("S", TNat :-> TNat)],
                requires = [],
                ensures = [],
                decreases = []
              }
        }

{- | Case analysis of a single process over its program points.
Depends on: p, 𝜙

Produces:

> switch 𝜋(p) {
>   ∀ 𝑛 ∈ 𝜙. case n => 𝜙(𝑛)
> }
-}
processSwitch :: P -> 𝛷 -> Stmt
processSwitch pid =
  let pc = 𝜋 pid
      iguard c = PCon (CNum c)
      cases = M.toList . M.mapKeys iguard
   in MatchStmt pc . cases

{- | Case analysis for scheduled process at the given step
over process ids.
Depends on: 𝛯

Produces:

> switch S(step) {
>   ∀ (p, 𝜙) ∈ dom(𝛯). case p => processSwitch(p, 𝜙)
> }
-}
scheduleSwitch :: 𝛯 -> Stmt
scheduleSwitch =
  let iguard pid = PCon (CNum pid)
      cases = M.toList . M.mapKeys iguard . M.mapWithKey processSwitch
      step = Call "S" [(𝑥step @)]
   in MatchStmt step . cases

{- | Constructs the central loop which emulates the execution
of the concurrent program.
Depends on: 𝜓, 𝜅, 𝛯, nonloop(P), loop(P)

Produces:

> while enabled(𝜅, 𝛯)
> ∀ e ∈ goMonitor(𝜓, 𝒢). invariant e
> ∀ e ∈ ifMonitor(ℐ). invariant e
> ∀ e ∈ loopMonitor(𝜓, ℒ). invariant e
> ∀ e ∈ returnMonitor(𝜓, ℛ). invariant e
> ∀ e ∈ channelBound(𝜅). invariant e
> ∀ e ∈ wgBound(𝑊). invariant e
> ∀ e ∈ noAsyncRendezvous(𝜅, 𝒪, ℒ). invariant e
> ∀ e ∈ rendezvousMutex(𝛯). invariant e
> ∀ e ∈ counterInvariant(𝛯). invariant e
> ∀ e ∈ channelMonitors(𝜓, 𝜅, 𝒪, ℒ). invariant e
> ∀ e ∈ wgMonitors(𝜓, 𝒲𝒜, ℒ). invariant e
> {
>   scheduleSwitch(𝛯)
>   step := step + 1
> }
-}
centralLoop :: Encoding -> Stmt
centralLoop Encoding {
  conditions = 𝜓,
  capacities = 𝜅,
  processes = 𝜉,
  waitgroups = wgs,
  summaries = ℳ { cs, os, gs, is, ls, rs, ws }} =
  let -- Go statement invariants
      g = goMonitors 𝜓 gs
      -- If statement invariants
      i = ifMonitors is
      -- Process loop invariants
      l = loopMonitors 𝜓 ls
      -- Return statement invariants
      r = returnMonitors 𝜓 rs
      -- Channel definition invariants
      def = channelDefs 𝜓 cs
      -- Channel bound invariants
      k = channelBounds cs
      -- WaitGroup bound invariants
      w = wgBounds wgs
      -- Absence of rendezvous for buffered channels invariants
      rv = noAsyncRendezvous 𝜅 os ls
      -- Mutual exclusion between rendezvous points of different process
      -- on the same channel
      rvm = rendezvousMutexes 𝜉
      -- Process counter invariants
      pc = counterInvariants 𝜉
      -- Channel buffer size invariants
      m = channelMonitors 𝜓 𝜅 os ls
      -- WaitGroup counter size invariants
      wg = wgMonitors 𝜓 ws ls
      -- Condition under which progress is enabled:
      -- Has fuel + process operation disjunctions
      enabled = ((𝑥step @) :< (𝑥fuel @)) :&& enabledExp 𝜅 𝜉
   in While
        enabled
        (concat [k, w, def, pc, rv, rvm, g, i, l, r, m, wg])
        ( Block
            [ -- Central loop case analysis
              scheduleSwitch 𝜉,
              -- Increment steps
              Assign 𝑥step ((𝑥step @) :+ (1 #))
            ]
        )

{- | Constructs an initial assignment for all program counters.
Depends on: 𝛯

Produces:

> var 𝜋(p₁) = 0;
> ...
> var 𝜋(pₙ) = 0;
-}
counterDef :: 𝛯 -> [Stmt]
counterDef =
  let def p = Assign (p ⊲) ((if p == 0 then 0 else -1) #)
   in L.map def . M.keys

{- | Constructs an initial assignment for all loop variables.
Depends on: ℒ

Produces:

> var x₁ := lo₁;
> ...
> var xₙ := loₙ;
-}
loopVarDef :: [ℒ] -> [Stmt]
loopVarDef =
  let def (ℒ {l𝑋 = x, lower}) = VarDef False x Nothing lower
   in  L.map def

{- | Constructs an assignment for the process termination variables.
The variables are assigned the process termination point.
Other expressions may indirectly reference process termination
by proxy of these variables.
Depends on: 𝛯

Produces:

> ∀ (p, 𝜙) ∈ 𝛯. const 𝜏(p) := (max ∘ dom)(𝜙)
-}
terminationVars :: 𝛯 -> [Decl]
terminationVars 𝜉 =
  let def p 𝜙 = CDecl (p ▽) (𝜙 -|)
   in M.elems . M.mapWithKey def $ 𝜉

{- | Constructs an initial assignment for all channel variables.
Depends on: 𝜅

Produces:

> ∀ c ∈ dom(𝜅). var c := 0;
-}
chanDef :: 𝛫 -> [Stmt]
chanDef =
  let def c = VarDef False c Nothing (0 #)
   in L.map def . M.keys

{- | Constructs an initial assignment for all WaitGroup variables.
Depends on: ws

Produces:

> ∀ w ∈ ws. var w = 0
-}
wgDef :: 𝑊 -> [Stmt]
wgDef =
  let def w = VarDef False w Nothing (0 #)
   in L.map def . S.elems

{- | Construcs the "isSchedule(S)" precondition.
-}
isSchedule :: Exp
isSchedule = Call "isSchedule" [("S" @)]

{- | Constructs the main program encoding according to the given strategy.
Depends on: Strat, 𝜓, 𝜅, 𝑊, 𝛯, ℳ

The overall program structure is as follows:

> lemma Program(S : nat -> nat, ∀ (x, t) ∈ fv(P). x : int)
> returns (∀ p ∈ dom(𝛯). 𝜋(p) : int)
> requires isSchedule(S)
>
> requires Strat.makePrecondition(𝜓, 𝜅, 𝑊, 𝛯, ℳ)
> ensures Strat.makePostcondition(𝜓, 𝜅, 𝑊, 𝛯, ℳ)
> {
>   counterDef(𝛯);
>   chanDef(𝜅);
>   wgDef(𝑊);
>   loopVarDef(ℒ);
>   step := 0;
>   centralLoop(𝜅, 𝑊, 𝛯, ℳ)
> }
-}
progEncoding :: Strategy -> Encoding -> Method
progEncoding Strategy { makePrecondition, makePostcondition } encoding@Encoding {
  typeenv = 𝛾,
  typevars = ts,
  waitgroups = ws,
  capacities = 𝜅,
  processes = 𝜉,
  summaries = ℳ { ls } } =
    Method
        { methodReturns = (𝑥step, TNat) : (L.map ((,TInt) . (⊲)) . M.keys) 𝜉,
          methodHoare =
            HoareWrap
              { ghost = True,
                name = "Program",
                types = ts,
                params = (𝑥fuel, TNat) : ("S", TNat :-> TNat) : M.toList 𝛾,
                ensures =
                  [ ((𝑥step @) :< (𝑥fuel @)) :==> makePostcondition encoding
                  ],
                decreases = [],
                requires = [isSchedule, makePrecondition encoding]
              },
          methodBody =
            Block $ concat
              [ counterDef 𝜉,
                chanDef 𝜅,
                wgDef ws,
                loopVarDef ls,
                [Assign 𝑥step (0 #)],
                [centralLoop encoding]
              ]
        }

{- | Constructs the complete CoreDafny program, by emitting
all the necessary functions, and the program encoding.
-}
encodingToDafny :: Strategy -> Encoding -> Program
encodingToDafny strat encoding@Encoding { processes = 𝜉 } =
  simplify $ Program (terminationVars 𝜉 ++ [
      FDecl iterationsFunc,
      FDecl (isScheduleFunc 𝜉),
      MDecl (progEncoding strat encoding)
    ])
