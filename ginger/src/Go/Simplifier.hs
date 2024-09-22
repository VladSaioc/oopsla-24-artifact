module Go.Simplifier (simplify) where

import Data.Bifunctor

import Go.Ast
import Go.Utilities
import Utilities.General
import Utilities.Position

-- | Simplifies a program by applying semantics-preserving reductions to statements,
-- where applicable e.g., by removing redundant statements or constant folding.
simplify :: Prog -> Prog
simplify p@(Prog ss) =
  let simpl = Prog $ simplifyStatements ss
   in if simpl == p
        then simpl
        else simplify simpl

-- | Simplifies a list of statements by applying simplifications to each statement.
simplifyStatements :: [Pos Stmt] -> [Pos Stmt]
simplifyStatements = \case
  [] -> []
  Pos p s : ss ->
    let pos = Pos p
        expr = fix simplifyExpression
        un c ss' = c (simplifyStatements ss')
        bin c ss1 ss2 = c (simplifyStatements ss1) (simplifyStatements ss2)
     in case s of
          -- Close statement: No change.
          Close c -> pos (Close c) : simplifyStatements ss
          -- As statement: No change.
          As x e -> pos (As x (expr e)) : simplifyStatements ss
          -- Declaration statement: No change.
          Decl x e -> pos (Decl x (expr e)) : simplifyStatements ss
          -- Channel declaration statement: No change.
          Chan c e -> pos (Chan c (expr e)) : simplifyStatements ss
          -- WaitGroup declaration statement: No change.
          Wgdef w -> pos (Wgdef w) : simplifyStatements ss
          -- WaitGroup add: No change.
          Add e w -> pos (Add (expr e) w) : simplifyStatements ss
          -- WaitGroup wait: No change.
          Wait w -> pos (Wait w) : simplifyStatements ss
          -- Channel operation statement: No change.
          Atomic o -> pos (Atomic o) : simplifyStatements ss
          -- Skip statement: Remove redundant Skip.
          Skip -> simplifyStatements ss
          -- Return statement: Simplify to just Return.
          Return -> [Pos p Return]
          -- Break statement: Simplify to just Break.
          Break -> [Pos p Break]
          -- Continue statement: Simplify to just Continue.
          Continue -> [Pos p Continue]
          -- Block statement: Flatten nested blocks.
          Block ss' -> simplifyStatements $ ss' ++ ss
          -- If-else statement:
          -- - Remove empty if-else.
          -- - Simplify if with true condition.
          -- - Simplify if with false condition.
          If _ [] [] -> simplifyStatements ss
          If CTrue ss' _ -> simplifyStatements $ ss' ++ ss
          If CFalse _ ss' -> simplifyStatements $ ss' ++ ss
          If e ss1 ss2 -> pos (bin (If (expr e)) ss1 ss2) : simplifyStatements ss
          -- Select statement:
          -- - Reduce empty select to itself. No continuation is possible.
          -- - Remove redundant empty non-blocking select.
          Select [] Nothing -> [Pos p $ Select [] Nothing]
          -- Inline blocking single send case select statements
          Select [(Pos p' (Send c), ss')] Nothing -> simplifyStatements (Pos p' (Atomic (Send c)) : ss' ++ ss)
          -- Inline blocking single receive case select statements
          Select [(Pos p' (Recv c), ss')] Nothing -> simplifyStatements (Pos p' (Atomic (Recv c)) : ss' ++ ss)
          -- Inline blocking single star case select statements
          Select [(Pos _ Star, ss')] Nothing -> simplifyStatements $ ss' ++ ss
          -- Inline default statement.
          Select [] (Just ss') -> simplifyStatements $ ss' ++ ss
          Select cs ds ->
            if not (relevantSelect s) && all (null . snd) cs && maybe True null ds
            -- Remove select statement if nothing interesting happens
            then simplifyStatements ss
            else
                let cs' = map (second simplifyStatements) cs
                    ds' = fmap simplifyStatements ds
                in pos (Select cs' ds') : simplifyStatements ss
          -- Go statement:
          -- - Remove redundant empty go.
          Go [] -> simplifyStatements ss
          Go ss' -> pos (un Go ss') : simplifyStatements ss
          -- For loop:
          -- - Remove redundant empty for loop.
          For _ _ _ _ [] -> simplifyStatements ss
          For x e1 e2 d ss' -> pos (un (For x e1 e2 d) ss') : simplifyStatements ss
          -- While loop:
          -- - Remove redundant empty while loop.
          While _ [] -> simplifyStatements ss
          While e ss' -> pos (un (While (expr e)) ss') : simplifyStatements ss

simplifyExpression :: Exp -> Exp
simplifyExpression =
  let simp = fix simplifyExpression
      bin c e1 e2 = c (simp e1) (simp e2)
    in \case
        -- n1 + n2 ==> n
        Plus (CNum n1) (CNum n2) -> (CNum (n1 + n2))
        -- (e + e') - e' ==> e
        -- (e + e') - e ==> e'
        Minus (Plus e e1) e2 ->
          if e1 == e2 then simp e
          else if e == e2 then simp e1
            else bin Minus (bin Plus e1 e2) e2
        -- e + (e' - e) ==> e
        Plus e (Minus e1 e2) -> bin Minus (bin Plus e e1) e2
        -- e + 0 ==> e
        Plus e (CNum 0) -> simp e
        -- 0 + e ==> e
        Plus (CNum 0) e -> simp e
        -- e1 + (e2 + e3) ==> (e1 + e2) + e3
        Plus e1 (Plus e2 e3) -> simp (Plus (Plus e1 e2) e3)
        -- e1 + e2 ==> e2 + e1, if e2 < e1 (move constants)
        Plus e1 e2 -> uncurry (bin Plus) (if e1 < e2 then (e1, e2) else (e2, e1))
        -- n1 - n2 ==> n
        Minus (CNum n1) (CNum n2) -> (CNum (n1 - n2))
        -- e - 0 ==> e
        Minus e (CNum 0) -> simp e
        -- e - e ==> 0
        Minus e1 e2 -> if e1 == e2 then CNum 0 else bin Minus e1 e2
        -- n1 * n2 ==> n
        Mult (CNum n1) (CNum n2) -> CNum (n1 * n2)
        -- 1 * e ==> e
        Mult (CNum 1) e -> simp e
        -- e * 1 ==> e
        Mult e (CNum 1) -> simp e
        -- -1 * e ==> -e
        Mult (CNum (-1)) e -> simp (Neg e)
        -- e * -1 ==> -e
        Mult e (CNum (-1)) -> simp (Neg e)
        -- e * 0 ==> 0
        Mult _ (CNum 0) -> CNum 0
        -- 0 * e ==> e
        Mult (CNum 0) _ -> CNum 0
        Mult e1 e2 -> bin Mult e1 e2
        -- e / 1 ==> e
        Div e (CNum 1) -> e
        Div e1 e2 -> bin Div e1 e2
        e -> e
