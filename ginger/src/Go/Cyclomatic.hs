module Go.Cyclomatic where

import Go.Ast
import Utilities.Position

-- | simpleProcess checks whether a sequence of statements constitutes
-- a *simple* process.
--
-- A process is deemed simple if:
-- 1. It is linear
-- 2. It has no decision points (topologically equivalent branches are assumed to have been merged).
-- 3. Does not spawn additional processes
-- 4. Does not create additional concurrency primitives
simpleProcess :: [Pos Stmt] -> Maybe ()
simpleProcess = \case
  [] -> return ()
  Pos _ s : ss -> do
    _ <- case s of
      Skip -> return ()
      Return -> return ()
      As {} -> return ()
      Chan {} -> Nothing
      Wgdef {} -> Nothing
      Break -> Nothing
      Continue -> return ()
      Add {} -> return ()
      Wait {} -> return ()
      Atomic _ -> return ()
      Close {} -> return ()
      Decl {} -> return ()
      Block ss' -> simpleProcess ss'
      If {} -> Nothing
      Select {} -> Nothing
      For {} -> Nothing
      Go {} -> Nothing
      While {} -> Nothing
    simpleProcess ss

-- | Determines whether a sequence of Go statements is 'terminal'
-- i.e., every one of its execution paths terminates with a 'return'
-- statement.
--
-- It is useful in stream-lining if statements.
-- Let ⟨_⟩ be a short-hand for the 'terminal' predicate. The rules are:
--
--  > [RETURN]:   ⟨return : ss⟩
--  > [SKIP]:     ⟨skip : ss⟩
--  >             |- ⟨ss⟩
--  > [DECL]:     ⟨x := e : ss⟩
--  >             |- ⟨ss⟩
--  > [BREAK]:    ⟨break : ss, _⟩
--  >             |- ⟨ss⟩
--  > [ASSIGN]:   ⟨x = e : ss⟩
--  >             |- ⟨ss⟩
--  > [CLOSE]:    ⟨close(c) : ss⟩
--  >             |- ⟨ss⟩
--  > [BLOCK-1]:  ⟨{ ss' } : ss⟩
--  >             |- ⟨ss'⟩
--  > [BLOCK-2]:  ⟨{ ss' } : ss⟩
--  >             |- ⟨ss⟩
--  > [IF-1]:     ⟨if e then ss1 else ss2 : ss⟩
--  >             |- ⟨ss1⟩
--  >             |- ⟨ss2⟩
--  > [IF-2]:     ⟨if e then ss1 else ss2 : ss⟩
--  >             |- ⟨ss⟩
--  > [SELECT-1]: ⟨select { case o: ss₁ | ... | case o: ssₙ | default: ss₀ } : ss⟩
--  >             |- ∀ 0 ≤ i ≤ n. ⟨ssᵢ⟩
--  > [SELECT-2]: ⟨select { case o: ss₁ | ... | case o: ssₙ } : ss⟩
--  >             |- ∀ 1 ≤ i ≤ n. ⟨ssᵢ⟩
--  > [SELECT-3]: ⟨select { _ } : ss⟩
--  >             |- ⟨ss⟩
--  > [FOR-1]:    ⟨for _ { ss' } : ss⟩
--  >             |- ⟨ss'⟩
--  > [FOR-2]:    ⟨for _ { ss' } : ss⟩
--  >             |- ⟨ss⟩
--  > [GO]:       ⟨go { ss' } : ss⟩
--  >             |- ⟨ss⟩
terminal :: [Pos Stmt] -> Bool
terminal = \case
  [] -> False
  Pos _ s : ss -> case s of
    Return -> True
    Skip -> terminal ss
    Break {} -> terminal ss
    Continue {} -> terminal ss
    Chan {} -> terminal ss
    Wgdef {} -> terminal ss
    Add {} -> terminal ss
    Wait {} -> terminal ss
    Decl {} -> terminal ss
    As {} -> terminal ss
    Close {} -> terminal ss
    Atomic {} -> terminal ss
    Block ss' -> terminal ss' || terminal ss
    If _ ss1 ss2 -> (terminal ss1 && terminal ss2) || terminal ss
    Select cs mds ->
      let tmds = maybe True terminal mds
          tcs = all (terminal . snd) cs
       in (tcs && tmds) || terminal ss
    For _ _ _ _ ss' -> terminal ss' || terminal ss
    While _ ss' -> terminal ss' || terminal ss
    Go _ -> terminal ss

-- | Remove return statements from the statement sequence.
stripReturns :: [Pos Stmt] -> [Pos Stmt]
stripReturns = \case
  [] -> []
  Pos p s : ss -> case s of
    Return {} -> []
    _ -> Pos p s : stripReturns ss
