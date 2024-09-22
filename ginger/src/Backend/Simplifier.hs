module Backend.Simplifier (simplify, clausesSimplify, eSimplify) where

import Data.Maybe qualified as Mb

import Backend.Ast
import Backend.Utilities

-- | Takes a back-end program and applies heuristics to simplify it.
simplify :: Program -> Program
simplify (Program ds) = Program $ map decSimplify ds

-- | Simplification of a declaration. Only applies to constant, method and
-- function declaration bodies. Acts as identity for type declarations.
decSimplify :: Decl -> Decl
decSimplify = \case
  FDecl f -> FDecl $ fSimplify f
  MDecl m -> MDecl $ mSimplify m
  CDecl x e -> CDecl x $ eSimplify e
  d -> d

-- | Simplify function declaration. Affects Hoare clauses and the function body.
fSimplify :: Function -> Function
fSimplify (Function {yields, funcHoare, funcBody}) =
  Function
    { yields = yields,
      funcHoare = hSimplify funcHoare,
      funcBody = eSimplify funcBody
    }

-- | Simplify method declaration. Affects Hoare clauses and the method body.
mSimplify :: Method -> Method
mSimplify (Method {methodReturns, methodHoare, methodBody}) =
  Method
    { methodReturns = methodReturns,
      methodHoare = hSimplify methodHoare,
      methodBody = sSimplify methodBody
    }

-- | Simplify clauses in Hoare triples. The process eliminates redundant clauses
-- i.e., `requires/ensures true`
hSimplify :: HoareWrap -> HoareWrap
hSimplify (HoareWrap {ghost, name, types, params, decreases, requires, ensures}) =
  HoareWrap
    { ghost = ghost,
      name = name,
      types = types,
      params = params,
      decreases = clausesSimplify decreases,
      requires = clausesSimplify requires,
      ensures = clausesSimplify ensures
    }

-- | Simplify lists of clauses in Hoare triples, by simplifying each
-- underlying expression, and eliminating redundant clauses i.e.,
-- trivially true clauses.
clausesSimplify :: [Exp] -> [Exp]
clausesSimplify = filter (ECon CTrue /=) . map eSimplify

-- | Simplify expressions via several heuristics. Applies constant folding.
-- Simplification is carried out repeatedly until a fixpoint is reached.
eSimplify :: Exp -> Exp
eSimplify pe =
  let -- Short-hands for simplification of unary, binary and ternary constructors.
      bin c e1 e2 = c (eSimplify e1) (eSimplify e2)
      un c e = c (eSimplify e)
      tri c e1 e2 e3 = c (eSimplify e1) (eSimplify e2) (eSimplify e3)
      -- Simplified expression.
      e' = case pe of
        -- Quantifiers bodies are simplified
        Exists xs e -> un (Exists xs) e
        Forall xs e -> un (Forall xs) e
        In e1 e2 -> bin In e1 e2
        -- Set members are point-wise simplified
        ESet es -> ESet $ map eSimplify es
        -- Tuple components are point-wise simplified
        ETuple es -> ETuple $ map eSimplify es
        -- Implications from false are always true
        -- false => e ==> true
        ECon CFalse :==> _ -> (True ?)
        -- Everything implies true
        -- e => true ==> true
        _ :==> ECon CTrue -> (True ?)
        -- Implications from true are equivalent to the right-hand side
        -- true => e ==> e
        ECon CTrue :==> e -> eSimplify e
        -- Implying false is equivalent to negation
        -- e => false ==> !e
        e :==> ECon CFalse -> eSimplify (Not e)
        e1 :==> e2 -> bin (:==>) e1 e2
        -- True and false are trivially not equivalent
        -- a. true <=> false ==> false
        ECon CTrue :<==> ECon CFalse -> (False ?)
        -- b. false <=> true ==> false
        ECon CFalse :<==> ECon CTrue -> (False ?)
        -- True is neutral in equivalence
        -- a. true <=> e ==> e
        ECon CTrue :<==> e -> eSimplify e
        -- b. e <=> true ==> e
        e :<==> ECon CTrue -> eSimplify e
        -- Equivalence with false implies the negation of identity
        -- a. false <=> e ==> !e
        ECon CFalse :<==> e -> eSimplify (Not e)
        -- b. e <=> false ==> !e
        e :<==> ECon CFalse -> eSimplify (Not e)
        -- Syntactic equivalence implies tautology
        -- e <=> e ==> true
        e1 :<==> e2 -> if e1 == e2 then (True ?) else bin (:<==>) e1 e2
        -- Conjunction with false is trivially absurd
        -- a. e && false ==> false
        _ :&& ECon CFalse -> (False ?)
        -- b. false && e ==> false
        ECon CFalse :&& _ -> (False ?)
        -- True is neutral in conjunction
        -- a. e && true ==> e
        e :&& ECon CTrue -> eSimplify e
        -- b. true && e ==> e
        ECon CTrue :&& e -> eSimplify e
        -- e && e ==> e
        e1 :&& e2 -> if e1 == e2 then e1 else bin (:&&) e1 e2
        -- Disjunction with true is trivially tautological
        -- a. e || true ==> true
        _ :|| ECon CTrue -> (True ?)
        -- b. true || e ==> true
        ECon CTrue :|| _ -> (True ?)
        -- False is neutral in disjunction
        -- a. e || false ==> e
        e :|| ECon CFalse -> eSimplify e
        -- b. false || e ==> e
        ECon CFalse :|| e -> eSimplify e
        -- e || e ==> e
        e1 :|| e2 -> if e1 == e2 then e1 else bin (:||) e1 e2
        -- Constant folding for negation
        -- !true ==> false
        Not (ECon CTrue) -> (False ?)
        -- !false ==> true
        Not (ECon CFalse) -> (True ?)
        -- Double negation elimination
        -- !!e ==> e
        Not (Not e) -> eSimplify e
        -- Comparison flipping
        -- !(e1 < e2) ==> e1 >= e2
        Not (e1 :== e2) -> eSimplify $ e1 :!= e2
        -- !(e1 <= e2) ==> e1 > e2
        Not (e1 :!= e2) -> eSimplify $ e1 :== e2
        -- !(e1 < e2) ==> e1 >= e2
        Not (e1 :< e2) -> eSimplify $ e1 :>= e2
        -- !(e1 <= e2) ==> e1 > e2
        Not (e1 :<= e2) -> eSimplify $ e1 :> e2
        -- !(e1 > e2) ==> e1 <= e2
        Not (e1 :> e2) -> eSimplify $ e1 :<= e2
        -- !(e1 >= e2) ==> e1 < e2
        Not (e1 :>= e2) -> eSimplify $ e1 :< e2
        Not e -> un Not e
        -- Constant folding for equality
        -- c == c ==> true, c1 != c2 ==> false
        ECon c1 :== ECon c2 ->
          if c1 == c2 then (True ?) else (False ?)
        -- Syntactic equality implies semantic equality
        -- e == e ==> true
        e1 :== e2 -> if e1 == e2 then (True ?) else bin (:==) e1 e2
        -- Constant folding for inequality
        -- c != c ==> false, c1 != c2 ==> true
        ECon c1 :!= ECon c2 ->
          if c1 == c2 then (False ?) else (True ?)
        -- Syntactic equality implies semantic inequality is false
        -- e != e ==> false
        e1 :!= e2 -> if e1 == e2 then (False ?) else bin (:!=) e1 e2
        -- Constant folding for lesser-than-or-equal numeric comparison
        -- n1 <= n2
        ECon (CNum n1) :<= ECon (CNum n2) -> ((n1 <= n2) ?)
        -- (if e then e1 else e2) <= (if e then e1' else e2') ==> if e then e1 <= e1' else e2 <= e2'
        IfElse e1 e2 e3 :<= IfElse e1' e2' e3' ->
          if e1 == e1'
            then tri IfElse e1 (e2 :<= e2') (e3 :<= e3')
            else bin (:<=) (IfElse e1 e2 e3) (IfElse e1' e2' e3')
        -- Syntactic equality implies lesser-than-or-equal comparison is trivially true
        -- e <= e ==> true
        e1 :<= e2 -> if e1 == e2 then (True ?) else bin (:<=) e1 e2
        -- Constant folding for strictly-lesser-than numeric comparison
        -- n1 < n2
        ECon (CNum n1) :< ECon (CNum n2) -> ((n1 < n2) ?)
        -- Syntactic equality implies strictly-lesser-than comparison is trivially false
        -- e < e ==> false
        e1 :< e2 -> if e1 == e2 then (False ?) else bin (:<) e1 e2
        -- Constant folding for greater-than-or-equal numeric comparison
        -- n1 >= n2
        ECon (CNum n1) :>= ECon (CNum n2) -> ((n1 >= n2) ?)
        -- Syntactic equality implies greater-than-or-equal comparison is trivially true
        -- e >= e ==> true
        e1 :>= e2 -> if e1 == e2 then (True ?) else bin (:>=) e1 e2
        -- Constant folding for strictly-greater-than numeric comparison
        -- n1 > n2
        ECon (CNum n1) :> (ECon (CNum n2)) -> ((n1 > n2) ?)
        -- Syntactic equality implies strictly-greater-than comparison is trivially false
        -- e > e ==> false
        e1 :> e2 -> if e1 == e2 then (False ?) else bin (:>) e1 e2
        -- Constant folding for addition
        -- n1 + n2 ==> n
        ECon (CNum n1) :+ ECon (CNum n2) -> ((n1 + n2) #)
        -- Zero is neutral for addition
        -- a. e + 0 ==> e
        e :+ ECon (CNum 0) -> eSimplify e
        -- b. 0 + e ==> e
        ECon (CNum 0) :+ e -> eSimplify e
        -- Flip negation to subtraction
        -- a. e + -e' ==> e - e'
        e1 :+ Neg e2 -> bin (:-) e1 e2
        -- b. -e + e' ==> e' - e
        Neg e1 :+ e2 -> bin (:-) e2 e1
        -- Unite if-else branches with common guard
        -- (if e then e1 else e2) + (if e then e1' else e2') ==> if e then e1 + e1' else e2 + e2'
        IfElse e1 e2 e3 :+ IfElse e1' e2' e3' ->
          if e1 == e1'
            then tri IfElse e1 (e2 :+ e2') (e3 :+ e3')
            else bin (:+) (IfElse e1 e2 e3) (IfElse e1' e2' e3')
        -- if e1 then e2 else true
        e1 :+ e2 -> bin (:+) e1 e2
        -- Constant folding for subtraction
        -- n1 - n2 ==> n
        ECon (CNum n1) :- ECon (CNum n2) -> ((n1 - n2) #)
        -- Zero is right-hand-side neutral for subtraction
        -- e - 0 ==> e
        e :- ECon (CNum 0) -> eSimplify e
        -- Subtraction of syntactically equal expressions is 0
        -- e - e ==> 0
        e1 :- e2 -> if e1 == e2 then (0 #) else bin (:-) e1 e2
        -- Constant folding for multiplication
        -- n1 * n2 ==> n
        ECon (CNum n1) :* ECon (CNum n2) -> ((n1 * n2) #)
        -- One is neutral for multiplication
        -- a. 1 * e ==> e
        ECon (CNum 1) :* e -> eSimplify e
        -- b. e * 1 ==> e
        e :* ECon (CNum 1) -> eSimplify e
        -- Zero is absorbing for multiplication
        -- a. e * 0 ==> 0
        _ :* ECon (CNum 0) -> (0 #)
        -- b. 0 * e ==> e
        ECon (CNum 0) :* _ -> (0 #)
        -- Multiplication with -1 is equivalent to negation
        -- a. -1 * e ==> -e
        ECon (CNum (-1)) :* e -> eSimplify (Neg e)
        -- b. e * -1 ==> -e
        e :* ECon (CNum (-1)) -> eSimplify (Neg e)
        e1 :* e2 -> bin (:*) e1 e2
        -- One is right-hand-side neutral for division
        -- e / 1 ==> e
        e :/ ECon (CNum 1) -> e
        -- Zero is left-hand-side absorbing for division
        -- 0 / e ==> 0
        ECon (CNum 0) :/ _ -> (0 #)
        e1 :/ e2 -> bin (:/) e1 e2
        -- Double negation elimination
        -- -(-e) ==> e
        Neg (Neg e) -> eSimplify e
        Neg (ECon (CNum n)) -> ((-n) #)
        Neg e -> un Neg e
        -- Reduction of statically determinable conditional statements
        -- to corresponding branch.
        -- a. if true then e1 else e2 ==> e1
        IfElse (ECon CTrue) e _ -> eSimplify e
        -- b. if false then e1 else e2 ==> e2
        IfElse (ECon CFalse) _ e -> eSimplify e
        -- c. if e1 then e2 else true
        IfElse e1 e2 (ECon CTrue) -> bin (:==>) e1 e2
        -- Reduce to one branch if both branches are equal.
        IfElse e1 e2 e3 -> if e2 == e3 then eSimplify e2 else tri IfElse e1 e2 e3
        -- Match expressions are point-wise simplified per-case
        Match e cs ->
          let ps = map fst cs
              es = map (eSimplify . snd) cs
           in Match (eSimplify e) $ zip ps es
        -- Call expression arguments are point-wise simplified
        Call f es -> Call f $ map eSimplify es
        -- Modulo 1 is trivially 0
        -- e % 1 ==> 0
        _ :% ECon (CNum 1) -> (0 #)
        e1 :% e2 -> bin (:%) e1 e2
        -- Terminal expressions
        ECon c -> ECon c
        EVar x -> (x @)
   in -- Check if simplification reached a fixpoint
      if pe == e'
        then -- If a fixpoint has been reached, return the expression
          e'
        else -- Otherwise, do another simplification pass
          eSimplify e'

-- | Applies heuristics to simplify statements.
sSimplify :: Stmt -> Stmt
sSimplify = \case
  -- Assignment right-hand-sides are point-wise simplified
  Assign x e -> Assign x $ eSimplify e
  -- Trivially nested blocks may be simplified by removing the outer block.
  Block [Block ss] -> sSimplify (Block ss)
  -- Blocks have statement sequence simplification applied to their body
  Block ss -> Block $ ssSimplify ss
  -- Variable definition right-hand-sides are point-wise simplified
  VarDef b x t e -> VarDef b x t $ eSimplify e
  -- If statements are point-wise simplified w.r.t. the guard and branches.
  If e s1 ms ->
    let e' = eSimplify e
        s1' = sSimplify s1
        ms' = fmap sSimplify ms
     in case e' of
          -- Statically true guards reduce if statements to the true branch
          -- if true S1 [else S2] ==> S1
          ECon CTrue -> s1'
          -- Statically false guards reduce if statements to the false branch.
          -- If no false branch is present, an empty block is produced instead.
          -- a. if false S1 else S2 ==> S2
          -- b. if false S1 ==> {}
          ECon CFalse -> Mb.fromMaybe (Block []) ms'
          _ -> If e' s1' ms'
  -- Match statement expression and branches are point-wise reduced.
  MatchStmt e cs ->
    let ps = map fst cs
        ss = map (sSimplify . snd) cs
     in MatchStmt (eSimplify e) $ zip ps ss
  -- While statements guards and bodies are simplified
  While e es s ->
    let e' = eSimplify e
     in case e' of
          -- Statically false while statements are reduced to an empty block.
          -- while false S ==> {}
          ECon CFalse -> Block []
          _ -> While e' (clausesSimplify es) (sSimplify s)
  Return es -> Return $ map eSimplify es

-- | Statement sequence simplification point-wise simplifies statement in lists
-- and afterwards eliminates "dead" statements, identified as empty blocks.
ssSimplify :: [Stmt] -> [Stmt]
ssSimplify = filter (Block [] /=) . map sSimplify
