module VIRGo.SanityCheck (sanityCheck) where

import Control.Monad
import Data.Set qualified as S

import VIRGo.Ast
import Utilities.Err

-- | The sanity check context bookkeeps variable names
data Ctx = Ctx
  { -- | Iteration variables
    ienv :: S.Set String,
    -- | Free variables
    fvs :: S.Set String,
    -- | Channel names
    chenv :: S.Set String,
    -- | Waitgroup names
    wgenv :: S.Set String
  }
  deriving (Eq, Ord, Read)

-- | Perform a sanity check on the given VIRGo program
sanityCheck :: 𝑆 -> Err Ctx
sanityCheck prc = do
  -- Create a fresh context
  let ctx =
        Ctx
          { ienv = S.empty,
            fvs = S.empty,
            chenv = S.empty,
            wgenv = S.empty
          }
  sanityCheckStm ctx prc

-- | Perform sanity checks on primitive declarations.
-- Remember any encountered free variables and the names of declared channels.
sanityCheckChan :: Ctx -> 𝐷 -> Err Ctx
sanityCheckChan ctx = \case
  (Chan c e) -> do
    -- Make sure there are no duplicate channel declarations.
    _ <- multiGuard [(S.member c (chenv ctx) || S.member c (wgenv ctx), "Duplicate concurrency primitive declaration: " ++ c)]
    -- Remember the name of the newly declared channel
    let ctx' = ctx {chenv = S.insert c (chenv ctx)}
    -- Sanity check the capacity expression
    sanityCheckExp ctx' e
  Wg w -> do
    -- Make sure there are no duplicate waitgroup declarations.
    _ <- multiGuard [(S.member w (chenv ctx) || S.member w (wgenv ctx), "Duplicate concurrency primitive declaration: " ++ w)]
    -- Remember the name of the newly declared channel
    return $ ctx {wgenv = S.insert w (wgenv ctx)}


-- | Perform sanity checks on VIRGo statements.
sanityCheckStm :: Ctx -> 𝑆 -> Err Ctx
sanityCheckStm ctx = \case
  Def d -> sanityCheckChan ctx d
  Skip -> return ctx
  Return -> return ctx
  Close _ -> return ctx
  Go s1 -> sanityCheckStm ctx s1
  If e s1 s2 -> do
    ctx' <- sanityCheckExp ctx e
    ctx'' <- sanityCheckStm ctx' s1
    sanityCheckStm ctx'' s2
  Seq s1 s2 -> do
    ctx' <- sanityCheckStm ctx s1
    sanityCheckStm ctx' s2
  For x e1 e2 os -> do
    ctx' <- sanityCheckExp ctx e1
    ctx'' <- sanityCheckExp ctx' e2
    _ <-
      multiGuard
        -- Ensure there are no duplicate loop variables
        [ (S.member x (ienv ctx), "Duplicate loop variable: " ++ x),
          -- Ensure that a channel name is not reused for a loop index
          (S.member x (chenv ctx), "Channel used as loop index: " ++ x),
          -- Ensure that a free variable is not used as a loop index
          (S.member x (fvs ctx), "Free variable used as loop index: " ++ x),
          -- Ensure that a WaitGroup name is not used as a loop index
          (S.member x (wgenv ctx), "WaitGroup used as a free variable: " ++ x)
        ]
    -- Bookkeep loop index variable name
    let ctx''' = ctx'' {ienv = S.insert x (ienv ctx)}
    foldM sanityCheckOp ctx''' os
  Atomic op -> sanityCheckOp ctx op

-- Sanity check a channel operation. Ensures that
-- the channel named by the operation was previously declared.
sanityCheckOp :: Ctx -> Op -> Err Ctx
sanityCheckOp ctx =
  let checkPrim c =
        if S.member c (chenv ctx) || S.member c (wgenv ctx)
          then return ctx
          else Bad ("Usage of undeclared concurrency primitive: " ++ c)
   in \case
        Send c -> checkPrim c
        Recv c -> checkPrim c
        Wait w -> checkPrim w
        Add w e -> do
          ctx' <- sanityCheckExp ctx e
          _ <- checkPrim w
          return ctx'

-- Sanity check VIRGo expressions. Ensures that loop variables or channel
-- names are not used in expressions.
sanityCheckExp :: Ctx -> 𝐸 -> Err Ctx
sanityCheckExp ctx =
  let bin e1 e2 = do
        ctx' <- sanityCheckExp ctx e1
        sanityCheckExp ctx' e2
   in \case
        e1 :& e2 -> bin e1 e2
        e1 :| e2 -> bin e1 e2
        Not e1 -> bin BTrue e1
        e1 :== e2 -> bin e1 e2
        e1 :!= e2 -> bin e1 e2
        e1 :< e2 -> bin e1 e2
        e1 :<= e2 -> bin e1 e2
        e1 :> e2 -> bin e1 e2
        e1 :>= e2 -> bin e1 e2
        e1 :+ e2 -> bin e1 e2
        e1 :- e2 -> bin e1 e2
        e1 :* e2 -> bin e1 e2
        e1 :/ e2 -> bin e1 e2
        Const _ -> return ctx
        BTrue -> return ctx
        BFalse -> return ctx
        Var x -> do
          _ <-
            multiGuard
              [ (S.member x (ienv ctx), "Loop variable used as a free variable: " ++ x),
                (S.member x (chenv ctx), "Channel used as a free variable: " ++ x),
                (S.member x (wgenv ctx), "WaitGroup used as a free variable: " ++ x)
              ]
          return (ctx {fvs = S.insert x (fvs ctx)})
