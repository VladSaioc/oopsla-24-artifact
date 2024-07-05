module Pipeline.Sanity.CallgraphOk (noRecursion) where

import Control.Monad
import Data.Map qualified as M
import Data.Set qualified as S

import Pipeline.Callgraph (getCG)
import Promela.Ast
import Promela.Utilities
import Utilities.Err
import Utilities.Position

data Ctxt a = Ctxt
  { ancestors :: S.Set String,
    callgraph :: M.Map String Module,
    visited :: S.Set String,
    syntax :: a
  }
  deriving (Eq, Ord, Read, Show)

makeCtx :: a -> Ctxt a
makeCtx a =
  Ctxt
    { ancestors = S.empty,
      callgraph = M.empty,
      visited = S.empty,
      syntax = a
    }

transferCtx :: a -> Ctxt b -> Ctxt a
transferCtx a = \case
  Ctxt {ancestors, callgraph, visited} ->
    Ctxt
      { ancestors = ancestors,
        callgraph = callgraph,
        visited = visited,
        syntax = a
      }

wrap :: Ctxt b -> Ctxt ()
wrap = transferCtx ()

updateCtxVisited :: Ctxt a -> Ctxt b -> Ctxt a
updateCtxVisited ctx ctx' =
  ctx {visited = S.union (visited ctx) (visited ctx')}

-- | Check that there is no recursion within the call graph.
noRecursion :: Spec -> Err ()
noRecursion (Spec ms) =
  let ss = getInit (Spec ms)
      ctx = (makeCtx ss) {callgraph = getCG (Spec ms)}
   in do
        _ <- traverseStmts ctx
        return ()

traverseStmts :: Ctxt [Pos Stmt] -> Err (Ctxt ())
traverseStmts ctx = case syntax ctx of
  [] -> return (wrap ctx)
  Pos _ s : ss -> do
    -- Traverse successor statements
    ctx' <- traverseStmts (ctx {syntax = ss})
    -- Traverse lists of syntax sequences.
    let visit traverseF =
          Control.Monad.foldM
            (\c syn -> fmap (updateCtxVisited c) (traverseF (c {syntax = syn})))
    -- Used in branching control flow constructs.
    let visitBranches = visit traverseStmts
    let visitExpressions = visit traverseExp ctx'
    -- Helper function for branching control flow with option "else" branch
    let branchingFlow os mels = do
          let os' = map (uncurry (:)) os
          ctx'' <- visitBranches ctx' os'
          case mels of
            Just els -> traverseStmts (ctx'' {syntax = els})
            Nothing -> return ctx'
    case s of
      Decl _ _ me -> case me of
        Just e -> traverseExp (ctx' {syntax = e})
        _ -> return (wrap ctx)
      If os mels -> branchingFlow os mels
      Do os mels -> branchingFlow os mels
      For r ss' -> do
        ctx'' <- traverseRange (ctx' {syntax = r})
        visitBranches ctx'' [ss']
      As _ e -> traverseExp (ctx' {syntax = e})
      Assert e -> traverseExp (ctx' {syntax = e})
      Recv _ es -> visitExpressions es
      Send _ es -> visitExpressions es
      ExpS e -> traverseExp (ctx' {syntax = e})
      _ -> return ctx'

traverseRange :: Ctxt Range -> Err (Ctxt ())
traverseRange ctx =
  let visit =
        Control.Monad.foldM
          (\c e -> fmap (updateCtxVisited c) (traverseExp (c {syntax = e})))
          (wrap ctx)
   in case syntax ctx of
        Between _ e1 e2 -> visit [e1, e2]
        _ -> return (wrap ctx)

traverseExp :: Ctxt Exp -> Err (Ctxt ())
traverseExp ctx =
  let visit =
        Control.Monad.foldM
          (\c e -> fmap (updateCtxVisited c) (traverseExp (c {syntax = e})))
          (wrap ctx)
   in case syntax ctx of
        And e1 e2 -> visit [e1, e2]
        Or e1 e2 -> visit [e1, e2]
        Eq e1 e2 -> visit [e1, e2]
        Ne e1 e2 -> visit [e1, e2]
        Le e1 e2 -> visit [e1, e2]
        Lt e1 e2 -> visit [e1, e2]
        Ge e1 e2 -> visit [e1, e2]
        Gt e1 e2 -> visit [e1, e2]
        Plus e1 e2 -> visit [e1, e2]
        Minus e1 e2 -> visit [e1, e2]
        Mult e1 e2 -> visit [e1, e2]
        Div e1 e2 -> visit [e1, e2]
        Neg e -> visit [e]
        Not e -> visit [e]
        Run "close" [_] -> return (wrap ctx)
        Run f es -> do
          _ <-
            multiGuard
              [ (S.member f (ancestors ctx), "Function " ++ f ++ " is called recursively.")
              ]
          if S.member f (visited ctx)
            then return (wrap ctx)
            else case M.lookup f (callgraph ctx) of
              Just (Proc _ _ ss) -> do
                ctx' <- visit es
                let ctx'' = ctx' {ancestors = S.insert f (ancestors ctx')}
                ctx''' <- traverseStmts (ctx'' {syntax = ss})
                return (ctx' {visited = S.insert f (visited ctx''')})
              Just _ -> Bad ("Name '" ++ f ++ "' is not bound to a function")
              _ -> Bad ("Call to unknown function: " ++ f)
        _ -> return (wrap ctx)
