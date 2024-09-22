module VIRGo.Simplifier (simplify) where

import Data.List qualified as L

import VIRGo.Ast
import Utilities.General

(#) :: Int -> 𝐸
(#) = Const

-- | Simplify VIRGo statements.
simplify :: 𝑆 -> 𝑆
simplify = fix (stripOuterPaths . stripReturns True . sSimplify)

-- | Simplify VIRGo channel definitions.
cSimplify :: 𝐷 -> 𝐷
cSimplify = \case
  Chan c e -> Chan c (eSimplify e)
  d -> d

-- | Simplify VIRGo statements.
sSimplify :: 𝑆 -> 𝑆
sSimplify s =
  let bin c s1 s2 = c (sSimplify s1) (sSimplify s2)
  in case s of
    Def d -> Def $ cSimplify d
    -- wg.Add(0) ==> skip
    Atomic (Add _ (Const 0)) -> Skip
    Atomic o -> Atomic (oSimplify o)
    -- return; S ==> return
    Seq Return _ -> Return
    -- skip; S ==> S
    Seq Skip s2 -> sSimplify s2
    -- S; skip ==> S
    Seq s1 Skip -> sSimplify s1
    -- if S'i then return else skip; if S'j then return else skip ==> if S'i then return else skip
    Seq (If (Var x) Return Skip) (If (Var y) Return Skip) | all ("S'" `L.isPrefixOf`) [x, y] -> If (Var x) Return Skip
    -- if S'i then return else skip; if S'j then return else skip ==> if S'i then return else skip
    Seq (If (Var x) Return Skip) s1 | "S'" `L.isPrefixOf` x -> If (Var x) Return (sSimplify s1)
    Seq s1 s2 -> Seq (sSimplify s1) (sSimplify s2)
    -- for (x : e1 .. e2) {} ==> skip
    For _ _ _ [] -> Skip
    For x e1 e2 os -> For x (eSimplify e1) (eSimplify e2) (map oSimplify os)
    -- if _ then skip else skip ==> skip
    If _ Skip Skip -> Skip
    -- if true then S1 else S2 ==> S1
    If BTrue s1 _ -> sSimplify s1
    -- if false then S1 else S2 ==> S2
    If BFalse _ s2 -> sSimplify s2
    -- if S'i then (if S'j then S1 else return) else return ==> if S'i then S1 else return
    If (Var x) (If (Var y) s1 Return) Return | all ("S'" `L.isPrefixOf`) [x, y] -> If (Var y) (sSimplify s1) Return
    -- if S'i then (if S'j then S1 else return) else return ==> if S'i then S1 else return
    If (Var x) (If (Var y) Return s1) Return | all ("S'" `L.isPrefixOf`) [x, y] -> If (Var x) (sSimplify s1) Return
    -- if S'i then (if S'j then S1 else return) else return ==> if S'i then S1 else return
    If (Var x) Return (If (Var y) s1 Return) | all ("S'" `L.isPrefixOf`) [x, y] -> If (Var x) (sSimplify s1) Return
    -- if S'i then (if S'j then S1 else return) else return ==> if S'i then S1 else return
    If (Var x) Return (If (Var y) Return s1) | all ("S'" `L.isPrefixOf`) [x, y] -> If (Var y) Return (sSimplify s1)
    -- if S'i then skip else (if S'j then S1 else S2) ==> if S'j then S1 else S2
    If (Var x) Skip (If (Var y) s1 s2) | all ("S'" `L.isPrefixOf`) [x, y] -> bin (If (Var y)) s1 s2
    -- if S'i then (if S'j then S1 else S2) else skip ==> if S'i then S1 else S2
    If (Var x) (If (Var y) s1 s2) Skip | all ("S'" `L.isPrefixOf`) [x, y] -> bin (If (Var x)) s1 s2
    -- if S'i then skip else (if S'j then S1 else S2) ==> if S'j then S1 else S2
    If (Var x) Skip (If (Var y) s1 s2) | all ("S'" `L.isPrefixOf`) [x, y] -> bin (If (Var y)) s1 s2
    -- if S'i then (if S'j then return else S1) else S2 ==> if S'j then return else (if S'i then S1 else S2)
    If (Var x) (If (Var y) Return s1) s2 | all ("S'" `L.isPrefixOf`) [x, y] -> If (Var y) Return (bin (If (Var x)) s1 s2)
    -- if S'i then (if S'j then return else S1) else S2 ==> if S'j then return else (if S'i then S1 else S2)
    If (Var x) (If (Var y) s1 Return) s2 | all ("S'" `L.isPrefixOf`) [x, y] -> If (Var y) Return (bin (If (Var x)) s1 s2)
    If e s1 s2 ->
      let e' = eSimplify e
       in if e' /= e
        then sSimplify (If e' s1 s2)
        else bin (If e') s1 s2
    -- go { skip } ==> skip
    Go Skip -> Skip
    Go s1 -> Go (sSimplify s1)
    _ -> s

-- | Simplify VIRGo expressions.
eSimplify :: 𝐸 -> 𝐸
eSimplify pe =
  let bin c e1 e2 = c (eSimplify e1) (eSimplify e2)
      e' = case pe of
        -- n1 + n2 ==> n
        Const n1 :+ Const n2 -> ((n1 + n2) #)
        -- e + e' - e' ==> e
        -- e + e' - e ==> e'
        (e :+ e1) :- e2 ->
          if e1 == e2 then eSimplify e
          else if e == e2 then eSimplify e1
            else bin (:-) (bin (:+) e e1) e2
        e :+ (e1 :- e2) -> bin (:-) (bin (:+) e e1) e2
        -- e + 0 ==> e
        e :+ Const 0 -> eSimplify e
        -- 0 + e ==> e
        Const 0 :+ e -> eSimplify e
        -- e1 + (e2 + e3) ==> (e1 + e2) + e3
        e1 :+ (e2 :+ e3) -> eSimplify ((e1 :+ e2) :+ e3)
        -- e1 + e2 ==> e2 + e1, if e2 < e1 (move constants)
        e1 :+ e2 -> uncurry (bin (:+)) (if e1 < e2 then (e1, e2) else (e2, e1))
        -- n1 - n2 ==> n
        Const n1 :- Const n2 -> ((n1 - n2) #)
        -- e - 0 ==> e
        e :- Const 0 -> eSimplify e
        -- e - e ==> 0
        e1 :- e2 -> if e1 == e2 then (0 #) else bin (:-) e1 e2
        -- n1 * n2 ==> n
        Const n1 :* Const n2 -> ((n1 * n2) #)
        -- 1 * e ==> e
        Const 1 :* e -> eSimplify e
        -- e * 1 ==> e
        e :* Const 1 -> eSimplify e
        -- e * 0 ==> 0
        _ :* Const 0 -> (0 #)
        -- 0 * e ==> e
        Const 0 :* _ -> (0 #)
        e1 :* e2 -> bin (:*) e1 e2
        -- e / 1 ==> e
        e :/ Const 1 -> e
        e1 :/ e2 -> bin (:/) e1 e2
        e -> e
   in if pe == e'
        then e'
        else eSimplify e'

-- | Simplify concurrent operations
oSimplify :: Op -> Op
oSimplify = \case
  Add w e -> Add w (eSimplify e)
  s -> s

-- | Strip returns from statements in tail position.
stripReturns :: Bool -> 𝑆 -> 𝑆
stripReturns tailPos = \case
  Seq s1 s2 -> Seq (stripReturns False s1) (stripReturns tailPos s2)
  If e s1 s2 -> If e (stripReturns tailPos s1) (stripReturns tailPos s2)
  Go s -> Go (stripReturns True s)
  Return -> if tailPos then Skip else Return
  s -> s

stripOuterPaths :: 𝑆 -> 𝑆
stripOuterPaths = \case
  If (Var _) Skip s1 -> stripOuterPaths s1
  If (Var _) s1 Skip -> stripOuterPaths s1
  s -> s
