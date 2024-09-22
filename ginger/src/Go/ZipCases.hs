module Go.ZipCases where

import Data.Bifunctor

import Go.Ast
import Go.Cyclomatic
import Utilities.Position

{- | Zip cases by computing a fixpoint.
-}
zipCases :: Prog -> Prog
zipCases (Prog s) =
  let s' = zipCasesStmts $ s ++ [Pos NoPos Return]
   in if s' == s then Prog s'
      else zipCases (Prog s')

-- | Zip together the branches of cases in statements, if
-- all side-effect operations observe the same sequence.
zipCasesStmts :: [Pos Stmt] -> [Pos Stmt]
zipCasesStmts = \case
  [] -> []
  s0@(Pos p _) : ss ->
    let pos = Pos p
        ss' = zipCasesStmts ss
        un c ss1 = c $ zipCasesStmts ss1
        -- Combine two branches of cases while handling potential terminal branches
        -- i. e., branches terminating with return/break/continue.
        twoBranches ss1 ss2 =
          let (ss1', ss2', cont) = case (terminal ss1, terminal ss2) of
                (True, True) -> (zipCasesStmts ss1, zipCasesStmts ss2, [])
                (True, False) -> (zipCasesStmts ss1, zipCasesStmts (ss2 ++ ss), [])
                (False, True) -> (zipCasesStmts (ss1 ++ ss), zipCasesStmts ss2, [])
                (False, False) -> (zipCasesStmts ss1, zipCasesStmts ss2, ss')
              -- Combine two lists of statements by hoisting common statements.
              hoist c1 c2 = case (c1, c2) of
                ([], _) -> (c1, c2, [])
                (_, []) -> (c1, c2, [])
                (Pos p1 s1 : thn, Pos _ s2 : els) ->
                  let (thn', els', shd) = hoist thn els
                   in if s1 == s2
                        then (thn', els', Pos p1 s1 : shd)
                        else (c1, c2, [])
           in
              -- Combine branches while hoisting common statements.
              let (thn, els, pref) = hoist ss1' ss2'
                   -- Strip return statements to prevent duplication.
                  (thn', els') =
                    if null pref
                      then (thn, els)
                      else (stripReturns thn, stripReturns els)
               in (thn', els', stripReturns pref, cont)
     in case (s0 @^) of
        -- Skip statement: No change.
        Skip -> ss'
        -- As, Chan, Decl, and Atomic statements: No change.
        As {} -> s0 : ss'
        Chan {} -> s0 : ss'
        Decl {} -> s0 : ss'
        Wgdef {} -> s0 : ss'
        Add {} -> s0 : ss'
        Wait {} -> s0 : ss'
        Atomic {} -> s0 : ss'
        -- Close statement: No change.
        Close {} -> s0 : ss'
        -- Break statement: Simplify to just Break.
        Break -> [s0]
        -- Return statement: Simplify to just Return.
        Return -> [s0]
        -- Continue statement: Simplify to just Continue.
        Continue -> [s0]
        -- Block statement: Zip branches of nested blocks and flatten them.
        Block ss1 -> zipCasesStmts ss1 ++ ss'
        -- Go statement: Zip branches of Go.
        Go ss1 -> pos (un Go ss1) : ss'
        -- For loop: Zip branches of For loop.
        For x e1 e2 inc ss1 -> pos (un (For x e1 e2 inc) ss1) : ss'
        -- While loop: Zip branches of While loop.
        While e ss1 -> pos (un (While e) ss1) : ss'
        -- If-else statement: Zip branches of If-else.
        If e ss1 ss2 ->
          let (thn, els, pref, cont) = twoBranches ss1 ss2
           in pref ++ pos (If e thn els) : cont
        -- Select statement: Zip cases of Select.
        Select [(o1, ss1), (o2, ss2)] Nothing ->
          let (thn, els, pref, cont) = twoBranches ss1 ss2
           in pref ++ pos (Select [(o1, thn), (o2, els)] Nothing) : cont
        Select cs def ->
          let cs' = map (second zipCasesStmts) cs
              def' = fmap zipCasesStmts def
           in pos (Select cs' def') : ss'

-- let caseBodies = map snd cs
--     hoistStmts = fmap def
--     checkTerminal () c =
--         if terminal c
--           then
--           else
--     (ss1', ss2', cont) = case (terminal ss1, terminal ss2) of
--       (True, True) -> (zipCasesStmts ss1, zipCasesStmts ss2, [])
--       (True, False) -> (zipCasesStmts ss1, zipCasesStmts (ss2 ++ ss), [])
--       (False, True) -> (zipCasesStmts (ss1 ++ ss), zipCasesStmts ss2, [])
--       (False, False) -> (zipCasesStmts ss2, zipCasesStmts ss2, ss')
--     hoist c1 c2 = case (c1, c2) of
--       ([], _) -> (c1, c2, [])
--       (_, []) -> (c1, c2, [])
--       (Pos p1 s1 : thn, Pos _ s2 : els) ->
--         let (thn', els', shd) = hoist thn els
--          in if s1 == s2
--               then (thn', els', Pos p1 s1 : shd)
--               else (c1, c2, [])
--  in let (thn, els, pref) = hoist ss1' ss2'
--      in pref ++ Pos p (If e thn els) : cont
