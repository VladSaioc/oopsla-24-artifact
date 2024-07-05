module Pipeline.VIRGoTranslation.Processes (procs) where

import Data.Map qualified as M

import Backend.Ast qualified as T
import Backend.Utilities
import VIRGo.Ast
import VIRGo.Utilities
import Pipeline.VIRGoTranslation.Exps
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

{- | Transforms a VIRGo program into a map from process ids to program points.
Depends on: 𝜅, 𝑆

Produces:

> 𝛯 = [ pᵢ ↦ 𝜙ᵢ | 𝜙ᵢ = stmtsToPoints(𝜅, pᵢ, ⟨0, []⟩, 𝑆ᵢ) ]
-}
procs :: 𝛫 -> 𝑆 -> 𝛯
procs 𝜅 s =
  let -- Convert all the statements to program points.
      (𝜆, 𝜉) = stmtToPoints 𝜅 (𝛬 { p = 0, nextp = 1, 𝑛 = 0 }, M.empty) s
      -- Add a termination program point for the entry process.
   in 𝜉 ⊔ (0, 𝑛 𝜆, T.Block [])

{- | Transform an VIRGo statement into a map of program points.
Depends on: 𝜅, p, next(p), S

Produces, based on S:

> [SKIP]:     ⟨p, 𝑛, 𝛯, skip⟩ -> ⟨p, 𝑛, 𝛯⟩
> [RETURN]:   ⟨p, 𝑛, 𝛯, return⟩ -> ⟨p, 𝑛 + 1, 𝛯[p ↦ 𝜙]⟩
>             |- 𝜙 = 𝛯(p)[𝑛 ↦ { 𝜋(p) := Tp }]
> [CHAN]:     ⟨p, 𝑛, 𝛯, c = [e]⟩ -> ⟨p, 𝑛 + 1, 𝛯[p ↦ 𝜙]⟩
>             |- 𝜙 = 𝛯(p)[𝑛 ↦ { 𝜋(p) := 𝑛 + 1; if e < 0 { return; } }]
> [COMM]:     ⟨p, 𝑛, 𝛯, c{!,?}⟩ -> opToPoints(𝜅, p, ⟨𝑛, 𝛯⟩, c{!,?})
> [SEQ]:      ⟨p, 𝑛, 𝛯, 𝑆₁; 𝑆₂⟩ -> ⟨p, 𝑛', 𝛯'⟩
>             |- ⟨p, 𝑛, 𝛯, 𝑆₁⟩ -> ⟨p, 𝑛'', 𝛯''⟩
>             |- ⟨p, 𝑛'', 𝛯'', 𝑆₂⟩ -> ⟨p, 𝑛', 𝛯'⟩
> [GO]:       ⟨p, 𝑛, 𝛯, go { 𝑆 }⟩ -> ⟨p, 𝑛 + 1, 𝛯'[p ↦ 𝜙, p' ↦ 𝜙']⟩
>             |- p' = next(𝛯)
>             |- ⟨p', 0, 𝛯, go { 𝑆 }⟩ -> ⟨p', 𝑛', 𝛯'⟩
>             |- 𝜙 = 𝛯'(p)[𝑛 ↦ { 𝜋(p) := 𝑛; 𝜋(p') := 0 }]
>             |- 𝜙' = 𝛯'(p')[-1 ↦ {}, 𝑛' ↦ {}]
> [IF]:       ⟨p, 𝑛, 𝛯, if e { 𝑆₁ } else { 𝑆₂ }⟩ -> ⟨p, 𝑛' + 1, 𝛯[p ↦ 𝜙]⟩
>             |- ⟨𝑛 + 1, 𝛯, 𝑆₁⟩ -> ⟨𝑛₁, 𝛯₁⟩
>             |- ⟨𝑛₁ + 1, 𝜙₁, 𝑆₁⟩ -> ⟨𝑛', 𝛯₂⟩
>             |- 𝜙 = 𝛯₂(p)[
>               𝑛 ↦ if x < e₂ {
>                   𝜋(p) := 𝑛 + 1
>                 } else {
>                   𝜋(p) := 𝑛₁ + 1
>                 },
>               𝑛₁ ↦ { pc := 𝑛' }]
> [FOR]:      ⟨p, 𝑛, 𝛯, for (i : e₁ .. e₂) { s }⟩ -> ⟨p, 𝑛' + 1, 𝛯'[p ↦ 𝜙]⟩
>             |- ⟨p, 𝑛', 𝛯'⟩ = opToPoints(𝜅, p, ⟨𝑛 + 1, 𝛯⟩, s)
>             |- 𝜙 = 𝛯'(p)[
>               𝑛 ↦ if x < e₂ {
>                   𝜋(p) := 𝑛 + 1
>                 } else {
>                   𝜋(p) := 𝑛' + 1
>                 },
>               𝑛' ↦ {
>                 x := x + 1;
>                 𝜋(p) := 𝑛;
>               }]
-}
stmtToPoints :: 𝛫 -> (𝛬, 𝛯) -> 𝑆 -> (𝛬, 𝛯)
stmtToPoints 𝜅 (𝜆@𝛬 { 𝑛 = 𝑛₀, p = p₀ }, 𝜉) s =
  let -- The next program location afther the current statement.
      𝜆' = 𝜆 { 𝑛 = 𝑛₀ + ppOffset s }
      -- if e { s }
      ifNoElse e s' = T.If e (T.Block s') Nothing
      -- Goto
      goto 𝜆₀ is =
        let p' = p 𝜆₀
            𝑛' = 𝑛 𝜆₀
         in T.Block [T.Block is, T.Assign (p' ⊲) (𝑛' #)]
      pgoto 𝑛' = goto 𝜆 { 𝑛 = 𝑛' } []
      p'goto p' 𝑛' = T.Assign (p' ⊲) (𝑛' #)
   in case s of
        Skip -> (𝜆, 𝜉)
        Close _ -> (𝜆, 𝜉)
        Def (Wg {}) -> (𝜆, 𝜉)
        Def (Chan _ e) ->
          -- Ensure that the channel is capacity-safe
          let e' = parseExp e
              -- if e' < 0 { return }
              check = ifNoElse (e' T.:< (0 #)) [T.Return []]
              -- 𝜙 = 𝜉(p)[𝑛₀ ↦ { 𝜋(p) := 𝑛₀ + 1; if e' < 0 { return } }]
           in (𝜆', 𝜉 ⊔ (p₀, 𝑛₀, goto 𝜆' [check]))
        Return ->
          let exit = T.Block [T.Assign (p₀ ⊲) (𝜏 p₀)]
           in (𝜆', 𝜉 ⊔ (p₀, 𝑛₀, exit))
        Atomic op -> opToPoint 𝜅 (𝜆, 𝜉) op
        Seq s1 s2 ->
          let (𝜆₁, 𝜉') = stmtToPoints 𝜅 (𝜆, 𝜉) s1
           in stmtToPoints 𝜅 (𝜆₁, 𝜉') s2
        If e s1 s2 ->
          let -- Translate guard expression
              e' = parseExp e
              -- Translate then branch
              (𝜆₁@𝛬 { 𝑛 = 𝑛₁ }, 𝜉₁) = stmtToPoints 𝜅 (𝜆 { 𝑛 = 𝑛₀ + 1 }, 𝜉) s1
              -- Translate else branch
              (𝜆₂@𝛬 { 𝑛 = 𝑛₂ }, 𝜉₂) = stmtToPoints 𝜅 (𝜆₁ { 𝑛 = 𝑛₁ + 1 }, 𝜉₁) s2
              -- if e' { 𝜋(p) := 𝑛₀ + 1 } else { pc := 𝑛₁ }
              guard = T.If e' (pgoto (𝑛₀ + 1)) (return $ pgoto (𝑛₁ + 1))
              -- 𝜙 = 𝜉₂(p)[
              --  𝑛₀ ↦ if x < e { 𝜋(p) := 𝑛₀ + 1; } else { 𝜋(p) := 𝑛₁ + 1 },
              --  𝑛₁ ↦ { 𝜋(p) := 𝑛₂ }
              -- ]
              𝜙₀ = [(𝑛₀, guard), (𝑛₁, pgoto 𝑛₂)]
           in (𝜆₂, 𝜉₂ ⨆ (p₀, 𝜙₀))
        For x _ e ops ->
          let -- Construct loop variable name in back-end.
              x' = p₀ % x
              -- Translate expression to back-end.
              e' = parseExp e
              -- Translate loop body to program points.
              (𝜆₁@𝛬{ 𝑛 = 𝑛' }, 𝜉₁) = opsToPoints 𝜅 (𝜆 { 𝑛 = 𝑛₀ + 1 }, 𝜉) ops

              -- if x < e { pc := 𝑛₀ + 1; } else { pc := 𝑛' + 1 }
              ifs = T.If ((x' @) T.:< e') (pgoto (𝑛₀ + 1)) (return $ pgoto $ 𝑛' + 1)
              -- { x := x + 1; pc := 𝑛 }
              iter = goto 𝜆 [T.Assign x' ((x' @) T.:+ (1 #))]

              -- 𝜙₂ = 𝜙[
              --  𝑛₀ ↦ if x < e { 𝜋(p) := 𝑛₀ + 1; } else { 𝜋(p) := 𝑛' + 1 },
              --  𝑛' ↦ { x := x + 1; 𝜋(p) := 𝑛 }
              -- ]
              𝜙₀ = [(𝑛₀, ifs), (𝑛', iter)]
           in (𝜆₁ { 𝑛 = 𝑛' + 1 }, 𝜉₁ ⨆ (p₀, 𝜙₀))
        Go s1 ->
          let -- Get next process ID
              p₁ = nextp 𝜆
              -- Construct new traversal context and translate goroutine body
              -- into a binding of program points.
              (𝜆₁, 𝜉₁) = stmtToPoints 𝜅 (𝛬 { 𝑛 = 0, p = p₁, nextp = p₁ + 1 }, 𝜉) s1
              -- Spawn goroutine instruction:
              -- Add go instruction to parent goroutine:
              -- 𝜙₀ = 𝜉₁(p₀)[
              --    𝑛₀ ↦ { 𝜋(p) := 𝑛; 𝜋(p₁) := 0 }
              --  ]
              𝜙₀ = (𝑛₀, goto 𝜆' [p'goto p₁ 0])
              -- Add "not-started" and terminated program points:
              -- 𝜙₁ = 𝜉₁(p₁)[
              --    -1 ↦ {},
              --    T(p₁) ↦ {}
              -- ]
              𝜙₁ = [(_UNSPAWNED, T.Block []), (𝑛 𝜆₁, T.Block [])]
           in -- Updated both processes with new program points.
              (𝜆' { nextp = nextp 𝜆₁ }, 𝜉₁ ⩏ [(p₀, [𝜙₀]), (p₁, 𝜙₁)])

{- Updates a program point set with the translations of
  the operation in the provided sequence.
-}
opsToPoints :: 𝛫 -> (𝛬, 𝛯) -> [Op] -> (𝛬, 𝛯)
opsToPoints 𝜅 (𝜆, 𝜉) = Prelude.foldl (opToPoint 𝜅) (𝜆, 𝜉)

{- Appends a set of program points with a new program point,
based on the next available instruction.
Depends on: 𝜅, p, ⟨n, 𝜙⟩, o

Produces:

1. If o = c!, then:

>   ⟨n + 2, 𝜙 = [
>     n ↦ if 0 < 𝜅(c) {
>         if c < 𝜅(c) {
>           c := c + 1;
>           𝜋(p) := n + 2;
>         }
>       } else {
>         if c == 0 {
>           c := 1;
>           𝜋(p) := n + 1;
>         }
>       }
>     (n + 1) ↦ if c == 1 {
>         c := -1;
>         𝜋(p) := n + 2;
>       }
>   ]⟩

2. If o = c?, then:

>  ⟨n + 1, 𝜙 = [
>    n ↦ if 0 < 𝜅(c) {
>        if c > 0 {
>          c := c - 1;
>          𝜋(p) := n + 1;
>        }
>      } else {
>        if c == 1 {
>          c := -1;
>          𝜋(p) := n + 1;
>        }
>      }
>  ]⟩

3. If o = w.Wait() then:

>  ⟨n + 1, 𝜙 = [
>    if w == 0 {
>      𝜋(p) := n + 1;
>    }
>  ]⟩

4. If o = w.Add(e) then:

>  ⟨n + 1, 𝜙 = [
>    if w + e >= 0 {
>      w := w + e
>      𝜋(p) := n + 1;
>    }
>  ]⟩
-}
opToPoint :: 𝛫 -> (𝛬, 𝛯) -> Op -> (𝛬, 𝛯)
opToPoint 𝜅 (𝜆@𝛬 { 𝑛 = 𝑛₀, p }, 𝜉) op =
  let -- Get concurrency primitive name for the operation.
      c = primName op
      -- 𝜋(p) := 𝑛'
      nextInstruction 𝑛' = T.Assign (p ⊲) (𝑛' #)
      -- if e { s }
      ifNoElse e s = T.If e (T.Block s) Nothing
      -- 𝜅(c)
      k = 𝜅 M.! c
      -- if 0 < 𝜅(c) { s1 } else { s2 }
      syncPoint s1 s2 =
        let wrap = T.If ((0 #) T.:< k)
         in -- Ensure statements are wrapped in blocks
            case (s1, s2) of
              (T.Block _, T.Block _) -> wrap s1 (return s2)
              (T.Block _, _) -> wrap s1 (return $ T.Block [s2])
              (_, T.Block _) -> wrap (T.Block [s1]) (return s2)
              (_, _) -> wrap (T.Block [s1]) (return $ T.Block [s2])
      -- c := e
      assignChan = T.Assign c
      -- Guarded channel operation for unbuffered communication
      sync 𝑛' current new =
        let body = [assignChan (new #), nextInstruction 𝑛']
         in ifNoElse ((c @) T.:== (current #)) body
      -- Guarded channel operation for buffered communication
      async 𝑛' guard inc =
        let body =
              [ -- c := c {+,-} 1
                assignChan $ inc (c @) (1 #),
                -- p := n + 1
                nextInstruction 𝑛'
              ]
         in ifNoElse guard body
   in case op of
        Send _ ->
          let -- c < 𝜅(c)
              guard = (c @) T.:< k
              -- if c < 𝜅(c) { c := c + 1; 𝜋(p) := 𝑛 + 2 }
              asyncCase = async (𝑛₀ + 2) guard (T.:+)
              -- if c == 0 { c := 1; 𝜋(p) := 𝑛 + 1 }
              syncCase = sync (𝑛₀ + 1) 0 1
              -- if 0 < 𝜅(c) { <async case> } else { <sync case> }
              opPoint = syncPoint asyncCase syncCase
              -- if c == -1 { c := 0; 𝜋(p) := 𝑛 + 2 }
              rendezvousPoint = sync (𝑛₀ + 2) (-1) 0
              -- Insert send operation at program point 𝑛.
              -- Insert rendezvous at program point 𝑛+1.
              𝜙₁ = [(𝑛₀, opPoint), (𝑛₀ + 1, rendezvousPoint)]
           in -- Return program points and next available instruction
              -- point 𝑛+2.
              (𝜆 { 𝑛 = 𝑛₀ + 2 }, 𝜉 ⨆ (p, 𝜙₁))
        Recv _ ->
          let -- c > 0
              guard = (c @) T.:> (0 #)
              -- if c > 0 { c := c - 1; 𝜋(p) := 𝑛 + 1 }
              asyncCase = async (𝑛₀ + 1) guard (T.:-)
              -- if c == 1 { c := -1; 𝜋(p) := 𝑛 + 1 }
              syncCase = sync (𝑛₀ + 1) 1 (-1)
              -- if 0 < 𝜅(c) { <async case> } else { <sync case> }
              opPoint = syncPoint asyncCase syncCase
              -- Insert receive operation at program point 𝑛
           in -- Return program points and next available instruction
              -- point 𝑛+1
              (𝜆 { 𝑛 = 𝑛₀ + 1 }, 𝜉 ⊔ (p, 𝑛₀, opPoint))
        Wait _ ->
          let -- w == 0
              guard = (c @) T.:== (0 #)
              -- if w == 0 { 𝜋(p) := 𝑛 + 1 }
              opPoint = ifNoElse guard [nextInstruction (𝑛₀ + 1)]
            in -- Return program points and next available instruction
               -- point 𝑛+1
               (𝜆 { 𝑛 = 𝑛₀ + 1 }, 𝜉 ⊔ (p, 𝑛₀, opPoint))
        Add _ e ->
          let e' = parseExp e
              -- w + e >= 0
              guard = (c @) T.:< (((-1) #) T.:* e')
              -- { if w < -e { return; }; w := w + e; 𝜋(p) := 𝑛 + 1; }
              opPoint = T.Block [ifNoElse guard [T.Return []], assignChan ((c @) T.:+ e'), nextInstruction (𝑛₀ + 1)]
            in -- Return program points and next available instruction
               -- point 𝑛+1
               (𝜆 { 𝑛 = 𝑛₀ + 1 }, 𝜉 ⊔ (p, 𝑛₀, opPoint))
