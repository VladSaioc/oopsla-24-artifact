module Go.GoForCommute where

import Data.Bifunctor

import Go.Ast
import Go.Cyclomatic
import Utilities.Position

-- | Describes a syntactical goroutine.
data Goroutine = Goroutine
  { decls :: [Pos Stmt],
    body :: [Pos Stmt],
    pos :: Int
  }
  deriving (Eq, Ord, Read, Show)

-- | Attempts to commute appropriate 'go' and 'for' statements.
-- A 'go' and 'for' statements may be commuted iff. the communication pattern
-- of the goroutines spawned in the body of the 'for' exhibit simple behaviour,
-- and all the operations in the body of the loop are non-blocking concurrent operations.
-- Every child goroutine inside the loop is turned to a goroutine outside the loop, and its body
-- is encapsulated in a loop with the same topology as the outer loop.
-- All non-blocking concurrent operations are hoisted in a sequential loop before
-- the goroutines.
--
-- Example:
--
-- > for (x : e1 .. e2) {
-- >    o1; o2; go { S1 }; go { S2 }; ...; go { Sn }; ... ok
-- > }
--
-- becomes
--
-- > for {
-- >    o1; o2; ...; ok
-- > }
-- > go {
-- >    for (x : e1 .. e2) { S1 }
-- > }
-- > go {
-- >    for (x : e1 .. e2) { S2 }
-- > }
-- > ...
-- > go {
-- >    for (x : e1 .. e2) { Sn }
-- > }
goForCommute :: Prog -> Prog
goForCommute (Prog ss) = Prog (goForCommuteSpine ss)

-- | Perform go-for-commute alongside the spine of the AST.
goForCommuteSpine :: [Pos Stmt] -> [Pos Stmt]
goForCommuteSpine = \case
  [] -> []
  Pos p s : ss ->
    let un c = c . goForCommuteSpine
        bin c s1 s2 = c (goForCommuteSpine s1) (goForCommuteSpine s2)
        s'' = case s of
          Block s' -> un Block s'
          If e s1 s2 -> bin (If e) s1 s2
          While e s' -> un (While e) s'
          Go s' -> un Go s'
          For x e1 e2 d s' ->
            case (forBodyGoroutines s', forBodyNonblockingConcurrency s') of
              (Just gs, Just os) ->
                let makeGo (Goroutine {decls, body, pos}) =
                      let g = Go $ decls ++ [Pos p $ For x e1 e2 d body]
                       in Pos pos g
                    os' = Pos p $ For x e1 e2 d os
                 in Block $ os' : map makeGo gs
              _ -> un (For x e1 e2 d) s'
          Select cs def ->
            let cs' = map (second goForCommuteSpine) cs
             in Select cs' $ fmap goForCommuteSpine def
          s' -> s'
     in Pos p s'' : goForCommuteSpine ss

-- | Inspects the body of a for loop and extracts all non-blocking concurrent operations.
-- If the loop body does not contain only non-blocking concurrent operations and
-- simple goroutine spawns, it returns Nothing.
forBodyNonblockingConcurrency :: [Pos Stmt] -> Maybe [Pos Stmt]
forBodyNonblockingConcurrency = \case
  [] -> Just []
  Pos p s : ss -> case s of
    Skip -> forBodyNonblockingConcurrency ss
    Continue -> forBodyNonblockingConcurrency ss
    Break -> Nothing
    Return -> Nothing
    Decl {} -> Nothing
    As {} -> Nothing
    Wgdef {} -> Nothing
    Add {} -> do
      ss' <- forBodyNonblockingConcurrency ss
      return $ Pos p s : ss'
    Wait {} -> Nothing
    Chan {} -> Nothing
    Atomic {} -> Nothing
    Close {} -> Nothing
    Block ss' -> forBodyNonblockingConcurrency $ ss' ++ ss
    If {} -> Nothing
    Select {} -> Nothing
    For {} -> Nothing
    While {} -> Nothing
    Go {} -> forBodyNonblockingConcurrency ss

-- | Inspects the body of a for loop and extracts all simple goroutine spawns.
-- If the loop body does not contain only non-blocking concurrent operations and
-- simple goroutine spawns, it returns Nothing.
forBodyGoroutines :: [Pos Stmt] -> Maybe [Goroutine]
forBodyGoroutines = \case
  [] -> Just []
  Pos p s : ss -> case s of
    Skip -> forBodyGoroutines ss
    Continue -> forBodyGoroutines ss
    Break -> Nothing
    Return -> Nothing
    Decl {} -> Nothing
    As {} -> Nothing
    Wgdef {} -> Nothing
    Add {} -> forBodyGoroutines ss
    Wait {} -> Nothing
    Chan {} -> Nothing
    Atomic {} -> Nothing
    Close {} -> Nothing
    Block ss' -> forBodyGoroutines $ ss' ++ ss
    If {} -> Nothing
    Select {} -> Nothing
    For {} -> Nothing
    While {} -> Nothing
    Go s' -> do
      _ <- simpleProcess s'
      ds <- goroutineDecls s'
      let stmts = goroutineStmts s'
      let g = Goroutine {decls = ds, body = stmts, pos = p}
      ss' <- forBodyGoroutines ss
      return $ g : ss'

-- | Collects all the upfront declarations in a goroutine's body.
goroutineDecls :: [Pos Stmt] -> Maybe [Pos Stmt]
goroutineDecls = \case
  [] -> return []
  Pos p s : ss -> case s of
    Decl {} -> do
      ss' <- goroutineDecls ss
      return $ Pos p s : ss'
    Block ss' -> goroutineDecls $ ss' ++ ss
    _ -> return []

-- | Collects all the non-declaration statements in a goroutine's body.
goroutineStmts :: [Pos Stmt] -> [Pos Stmt]
goroutineStmts ss =
  let addStmt = \case
        Pos _ Decl {} -> id
        s -> (s :)
   in Prelude.foldr addStmt [] ss
