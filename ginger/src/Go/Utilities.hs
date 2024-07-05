module Go.Utilities where

import Data.Bifunctor

import Go.Ast
import Utilities.Position

-- | Takes `if-else` statement chains and flips them such that `else` statement
-- of the last `if` statement becomes the `else` statement of the first `if` statement.
-- This inverts the order in which the `if` statement guards are evaluated.
-- Turns:
--
-- > if e₁ s₁
-- > else if e₂ s₂
-- > ...
-- > else if eₙ sₙ
-- > else { ... }
--
-- Into:
--
-- > if eₙ sₙ
-- > ...
-- > else if e₂ s₂
-- > else if e₁ s₁
-- > else { ... }
flipIfs :: Pos Stmt -> Pos Stmt
flipIfs s =
  let collectBranches = \case
        Pos p (If e s' [els]) -> Pos p (e, s') : collectBranches els
        _ -> []
      getElse = \case
        Pos _ (If _ _ [els]) -> getElse els
        Pos p (If _ _ els) -> Pos p $ Block els
        ps' -> ps'
      branches = collectBranches s
   in foldl (\(Pos p els) (Pos p' (e, s')) -> Pos p' $ If e s' [Pos p els]) (getElse s) branches

-- Reverses all the statements in a Go program.
reverseProg :: Prog -> Prog
reverseProg (Prog ss) = Prog $ reverseStmts ss

reverseStmts :: [Pos Stmt] -> [Pos Stmt]
reverseStmts = reverse . map reverseStmt

reverseStmt :: Pos Stmt -> Pos Stmt
reverseStmt (Pos p s) =
  let bin c ss1 ss2 = c (reverseStmts ss1) (reverseStmts ss2)
      un c ss1 = c (reverseStmts ss1)
   in Pos p $ case s of
        If e ss1 ss2 -> bin (If e) ss1 ss2
        While e ss' -> un (While e) ss'
        For x e1 e2 d ss' -> un (For x e1 e2 d) ss'
        Select cs d ->
          let d' = fmap reverseStmts d
              cs' = map (second reverseStmts) cs
           in Select cs' d'
        Go ss' -> un Go ss'
        Block ss' -> un Block ss'
        _ -> s

relevantSelect :: Stmt -> Bool
relevantSelect = \case
  Select cs _ ->
    let starCase o = case (o @^) of { Star -> True; _ -> False }
     in not (all (starCase . fst) cs)
  _ -> True

interesting :: Prog -> Bool
interesting (Prog ss) = any interestingStmt ss

interestingStmt :: Pos Stmt -> Bool
interestingStmt (Pos _ s) =
  let
    bin s1 s2 = any interestingStmt s1 || any interestingStmt s2
  in case s of
    Atomic Star -> False
    Atomic _ -> True
    If _ ss1 ss2 -> bin ss1 ss2
    While _ ss -> any interestingStmt ss
    For _ _ _ _ ss -> any interestingStmt ss
    Go s1 -> any interestingStmt s1
    Close _ -> True
    Block ss -> any interestingStmt ss
    Select cs def ->
      let
        caseOp o = case (o @^) of { Star -> False; _ -> True }
        def' = maybe False (any interestingStmt) def
        cs' = any (any interestingStmt . snd) cs
      in any (caseOp . fst) cs || def' || cs'
    _ -> False
