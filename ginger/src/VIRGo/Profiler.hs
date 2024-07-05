module VIRGo.Profiler (
  countVirgoFVOccurrences,
  countVirgoFVs,
  getVirgoParametricity,
  countVirgoFVOccurrencesLoop,
  countVirgoFVOccurrencesCaps,
  countVirgoFVOccurrencesAdd,
  countVirgoFVOccurrencesIf,
) where

import Data.List qualified as L
import Data.Monoid
import Data.Set qualified as S

import VIRGo.Ast

data Parametricity = ChLooping | WgLooping | WgAdd | Capacity
  deriving (Read)

instance Show Parametricity where
  show = \case
    ChLooping -> "chan-loop"
    WgLooping -> "wg-loop"
    WgAdd -> "wg-add"
    Capacity -> "capacity"

getVirgoParametricity :: 𝑆 -> String
getVirgoParametricity s =
  let parametricity = getParametricity s
   in if null parametricity then "not parametric."
      else L.intercalate "; " (map show parametricity)

getParametricity :: 𝑆 -> [Parametricity]
getParametricity p =
  let chlooping =  ([ChLooping | Any True == chLoopParametric p])
      wglooping =  ([WgLooping | Any True == wgLoopParametric p])
      add =  ([WgAdd | Any True == wgAddParametric p])
      cap = ([Capacity | Any True == capParametric p])
    in chlooping ++ wglooping ++ add ++ cap

chLoopParametric :: 𝑆 -> Any
chLoopParametric s =
  let makecontext looping = \case
        For _ e1 e2 _ ->
          let param = parametricExp (Any True)
           in param e1 <> param e2
        _ -> looping
      makeresult looping = \case
        Atomic (Send _) -> looping
        Atomic (Recv _) -> looping
        _ -> Any False
    in traverseStmt makecontext makeresult (Any False) s

wgLoopParametric :: 𝑆 -> Any
wgLoopParametric s =
  let makecontext looping = \case
        For _ e1 e2 _ ->
          let param = parametricExp (Any True)
           in param e1 <> param e2
        _ -> looping
      makeresult looping = \case
        Atomic (Add _ _) -> looping
        Atomic (Wait _) -> looping
        _ -> Any False
    in traverseStmt makecontext makeresult (Any False) s

wgAddParametric :: 𝑆 -> Any
wgAddParametric s =
  let makecontext _ _ = Any False
      makeresult _ = \case
        Atomic (Add _ e) -> parametricExp (Any True) e
        _ -> Any False
    in traverseStmt makecontext makeresult (Any False) s

capParametric :: 𝑆 -> Any
capParametric s =
  let makecontext _ _ = Any False
      makeresult _ = \case
        Def (Chan _ e) -> parametricExp (Any True) e
        _ -> Any False
   in traverseStmt makecontext makeresult (Any False) s

traverseStmt :: Monoid a => Monoid b => (a -> 𝑆 -> a) -> (a -> 𝑆 -> b) -> a -> 𝑆 -> b
traverseStmt makecontext makeresult ctx s =
  let tr = traverseStmt makecontext makeresult (makecontext ctx s)
      res = makeresult ctx
  in case s of
    Seq s1 s2 -> res s <> tr s1 <> tr s2
    Go s' -> res s <> tr s'
    For _ _ _ os ->
      let mkres c = makeresult c . Atomic
       in res s <> mconcat (map (mkres (makecontext ctx s)) os)
    If _ ss1 ss2 -> res s <> tr ss1 <> tr ss2
    _ -> res s

parametricExp :: Monoid a => a -> 𝐸 -> a
parametricExp unit =
  let param = parametricExp unit
      bin e1 e2 = param e1 <> param e2
  in \case
    Const _ -> mempty
    BFalse -> mempty
    BTrue -> mempty
    e1 :& e2 -> bin e1 e2
    e1 :| e2 -> bin e1 e2
    e1 :== e2 -> bin e1 e2
    e1 :!= e2 -> bin e1 e2
    e1 :< e2 -> bin e1 e2
    e1 :<= e2 -> bin e1 e2
    e1 :> e2 -> bin e1 e2
    e1 :>= e2 -> bin e1 e2
    Not e -> param e
    e1 :+ e2 -> bin e1 e2
    e1 :- e2 -> bin e1 e2
    e1 :* e2 -> bin e1 e2
    e1 :/ e2 -> bin e1 e2
    Var x -> if "S'" `L.isPrefixOf` x then mempty else unit

-- | Count the number of free variable occurrences.
countVirgoFVOccurrences :: 𝑆 -> Int
countVirgoFVOccurrences s = getSum $ mconcat $ map Sum
  [ countVirgoFVOccurrencesLoop s
  , countVirgoFVOccurrencesCaps s
  , countVirgoFVOccurrencesAdd s
  , countVirgoFVOccurrencesIf s
  ]

-- | Count the number of free variable occurrences in channel capacities.
countVirgoFVOccurrencesStmt :: (𝑆 -> Sum Int) -> 𝑆 -> Int
countVirgoFVOccurrencesStmt match =
  let makecontext _ = const 0
      makeresult  _ = match
   in getSum . traverseStmt makecontext makeresult (Sum 0)

-- | Count the number of free variable occurrences in for loop bounds.
countVirgoFVOccurrencesLoop :: 𝑆 -> Int
countVirgoFVOccurrencesLoop =
  let countExp :: 𝐸 -> Sum Int
      countExp = parametricExp (Sum 1)
      makeresult  = \case
        For _ e1 e2 _ -> countExp e1 <> countExp e2
        _ -> Sum 0
   in countVirgoFVOccurrencesStmt makeresult

-- | Count the number of free variable occurrences in channel capacities.
countVirgoFVOccurrencesCaps :: 𝑆 -> Int
countVirgoFVOccurrencesCaps =
  let countExp :: 𝐸 -> Sum Int
      countExp = parametricExp (Sum 1)
      makeresult  = \case
        Def (Chan _ e) -> countExp e
        _ -> Sum 0
   in countVirgoFVOccurrencesStmt makeresult

-- | Count the number of free variable occurrences in WaitGroup add expressions.
countVirgoFVOccurrencesAdd :: 𝑆 -> Int
countVirgoFVOccurrencesAdd =
  let countExp :: 𝐸 -> Sum Int
      countExp = parametricExp (Sum 1)
      makeresult  = \case
        Atomic (Add _ e) -> countExp e
        _ -> Sum 0
   in countVirgoFVOccurrencesStmt makeresult

-- | Count the number of free variable occurrences in conditional statement guards.
countVirgoFVOccurrencesIf :: 𝑆 -> Int
countVirgoFVOccurrencesIf =
  let countExp :: 𝐸 -> Sum Int
      countExp = parametricExp (Sum 1)
      makeresult  = \case
        If e _ _ -> countExp e
        _ -> Sum 0
   in countVirgoFVOccurrencesStmt makeresult

-- | Count the number of unique free variables.
countVirgoFVs :: 𝑆 -> Int
countVirgoFVs =
  let makecontext :: S.Set String -> 𝑆 -> S.Set String
      makecontext _ = const S.empty
      makeresult _ = \case
        Def (Chan _ e) -> freeVars e
        For _ e1 e2 _ -> freeVars e1 <> freeVars e2
        Atomic (Add _ e) -> freeVars e
        If e _ _ -> freeVars e
        _ -> S.empty
   in S.size . traverseStmt makecontext makeresult mempty

-- | Get a set of free variables in an expression
freeVars :: 𝐸 -> S.Set String
freeVars = \case
  Const _ -> S.empty
  BFalse -> S.empty
  BTrue -> S.empty
  Var x -> if "S'" `L.isPrefixOf` x then S.empty else S.singleton x
  Not e -> freeVars e
  e1 :& e2 -> freeVars e1 <> freeVars e2
  e1 :| e2 -> freeVars e1 <> freeVars e2
  e1 :== e2 -> freeVars e1 <> freeVars e2
  e1 :!= e2 -> freeVars e1 <> freeVars e2
  e1 :< e2 -> freeVars e1 <> freeVars e2
  e1 :<= e2 -> freeVars e1 <> freeVars e2
  e1 :> e2 -> freeVars e1 <> freeVars e2
  e1 :>= e2 -> freeVars e1 <> freeVars e2
  e1 :+ e2 -> freeVars e1 <> freeVars e2
  e1 :- e2 -> freeVars e1 <> freeVars e2
  e1 :* e2 -> freeVars e1 <> freeVars e2
  e1 :/ e2 -> freeVars e1 <> freeVars e2
