module VIRGo.GetAst (getAst) where

import VIRGo.AbsVIRGo qualified as R'
import VIRGo.Ast
import VIRGo.ErrM qualified as R''
import VIRGo.ParVIRGo
import Utilities.Err as U
import Utilities.General

-- | Extract integer value from VIRGo constant number token.
(#) :: R'.NUMBER -> Int
(#) (R'.NUMBER (_, i)) = read i :: Int

(&) :: R'.ID -> String
(&) (R'.ID (_, x)) = x

composeSyntax :: Monad m => (a -> m b) -> [a] -> m [b]
composeSyntax f rs = do
  os' <- foldMonad f [] (flip (:)) rs
  return (reverse os')

-- Parses the given string as a Promela program and
-- performs additional refinement on the existing parse tree.
getAst :: String -> Err 𝑆
getAst = pProgram . pProg . myLexer

pProgram :: R''.Err R'.Prog -> Err 𝑆
pProgram = \case
  R''.Ok (R'.Prog s) -> pStms s
  R''.Bad err -> Bad err

pChan :: R'.Stm -> Err [𝐷]
pChan = \case
  (R'.Chan c e) -> do
    e' <- pExp e
    return [Chan (c &) e']
  _ -> return []

pStms :: [R'.Stm] -> Err 𝑆
pStms ss =
  let (|>) rs1 rs2 = do
        s1 <- rs1
        s2 <- pStm rs2
        return (Seq s1 s2)
   in foldl (|>) (return Skip) ss

pStm :: R'.Stm -> Err 𝑆
pStm = \case
  R'.Chan c e ->  do
    e' <- pExp e
    return $ Def (Chan (c &) e')
  R'.Wgdef w -> return $ Def (Wg (w &))
  R'.Return {} -> return Return
  R'.Skip {} -> return Skip
  R'.SOp o -> do
    o' <- pOp o
    return (Atomic o')
  R'.For _ x e1 e2 os -> do
    e1' <- pExp e1
    e2' <- pExp e2
    os' <- composeSyntax pOp os
    return (For (x &) e1' e2' os')
  R'.SIf _ e s1 _ s2 -> do
    e' <- pExp e
    s1' <- pStms s1
    s2' <- pStms s2
    return (If e' s1' s2')
  R'.Go _ s -> do
    s' <- pStms s
    return (Go s')

pOp :: R'.Op -> Err Op
pOp = \case
  R'.Snd c _ -> return (Send (c &))
  R'.Rcv c _ -> return (Recv (c &))
  R'.Add w e -> do
    e' <- pExp e
    return (Add (w &) e')
  R'.Wait w -> return (Wait (w &))

pExp :: R'.Exp -> Err 𝐸
pExp =
  let un = unaryCons pExp
      bin = binaryCons pExp
   in \case
        R'.Or e1 _ e2 -> bin (:|) e1 e2
        R'.And e1 _ e2 -> bin (:&) e1 e2
        R'.Not _ e -> un Not e
        R'.Eq e1 _ e2 -> bin (:==) e1 e2
        R'.Ne e1 _ e2 -> bin (:!=) e1 e2
        R'.Le e1 _ e2 -> bin (:<=) e1 e2
        R'.Lt e1 _ e2 -> bin (:<) e1 e2
        R'.Ge e1 _ e2 -> bin (:>=) e1 e2
        R'.Gt e1 _ e2 -> bin (:>) e1 e2
        R'.Plus e1 _ e2 -> bin (:+) e1 e2
        R'.Minus e1 _ e2 -> bin (:-) e1 e2
        R'.Mult e1 _ e2 -> bin (:*) e1 e2
        R'.Div e1 _ e2 -> bin (:/) e1 e2
        R'.Const n -> return (Const (n #))
        R'.Var x -> return (Var (x &))
        R'.Tru {} -> return BTrue
        R'.Fal {} -> return BFalse
