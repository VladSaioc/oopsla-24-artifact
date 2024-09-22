module Promela.GetAst (getAst) where

import Promela.AbsPromela qualified as Raw
import Promela.Ast
import Promela.ErrM qualified as RawErr
import Promela.ParPromela
import Utilities.Err as U
import Utilities.General
import Utilities.Position

-- | Extract the integer value from a BNFC-produced number.
(#) :: Raw.NUMBER -> Int
(#) (Raw.NUMBER (_, i)) = read i :: Int

-- | Turn a BNFC-produced identifier into a more ergonomic representation.
(&) :: Raw.ID -> Ident
(&) (Raw.ID (_, x)) = x

-- | Extract the line number from a BNFC-produced identifier.
(~) :: Raw.ID -> Int
(~) (Raw.ID ((l, _), _)) = l

-- | Take a Steps binary AST and convert it to a list of steps.
listSteps :: Raw.Steps -> [Raw.Step]
listSteps = \case
  Raw.StepsSemi s ss -> s : listSteps ss
  Raw.StepsArrow s ss -> s : listSteps ss
  Raw.StepsMissing ss -> listSteps ss
  Raw.StepsLast s -> [s]
  Raw.StepsDone -> []

-- | Safeguard against unimplemented features.
notImplemented :: Show a1 => a1 -> Err a2
notImplemented err = Bad ("Not implemented: " ++ show err)

-- | Decorate a value with the positional information of a token.
(@@) :: ((Int, b), c) -> a -> Pos a
((l, _), _) @@ a = Pos l a

-- | Construct a positional value from a token.
pToken :: ((Int, a), b) -> Pos ()
pToken ((l, _), _) = Pos l ()

-- | Turn a BNFC-produced identifier into a more ergonomic representation and wrap it in a monad.
pIdent :: Monad m => Raw.ID -> m Ident
pIdent = return . (&)

-- | Parses the given string to a raw Promela AST and
-- performs additional refinement on the AST.
getAst :: String -> Err Spec
getAst = pProgram . pSpec . myLexer

-- | Refines a BNFC-produced Promela AST into a more ergonomic representation.
-- It performs minimal semantic checks during refinement.
pProgram :: RawErr.Err Raw.Spec -> Err Spec
pProgram = \case
  RawErr.Ok (Raw.Program modules) -> do
    ms <- results (map pModule modules)
    return (Spec ms)
  RawErr.Bad err -> Bad err

-- | Refine the ASTs of top-level declarations into a more ergonomic representation.
pModule :: Raw.Module -> Err Module
pModule = \case
  -- #define x e, where e ∈ ℤ ∪ { ?? }
  Raw.Define _ x' e' -> do
    x <- pIdent x'
    v <- pVal e'
    return (TopDecl x TInt v)
  -- init { ss }
  Raw.Init _ ss' -> do
    ss <- results (map pStep (listSteps ss'))
    return (Init ss)
  -- proctype f(ps) { ss }
  Raw.Proctype _ f' ps' ss' -> do
    f <- pIdent f'
    ps <- results (map pParam ps')
    ss <- results (map pStep (listSteps ss'))
    return (Proc f ps ss)
  -- typedef t { fs }
  Raw.Typedef _ t' fs' -> do
    t <- pIdent t'
    fs <- results (map pField fs')
    return (Typedef t fs)
  m -> notImplemented m

-- | Refine expressions to constant values. Will fail
-- if the given expression is not a constant.
pVal :: Raw.Exp -> Err Val
pVal = \case
  -- Extracts a value from e, where e = -n and n is a natural number
  Raw.ExpNeg _ (Raw.ExpConst (Raw.CInt n)) ->
    return (VInt (negate (n #)))
  -- Extracts a value from e, where e = n and n is a natural number
  Raw.ExpConst (Raw.CInt i) -> return (VInt (i #))
  Raw.ExpConst (Raw.CFree _) -> return Free
  Raw.ExpConst (Raw.CTrue _) -> return (VBool True)
  Raw.ExpConst (Raw.CFalse _) -> return (VBool False)
  _ -> Bad "Bad #define macro"

-- | Refine procedure parameter members of the AST. A parameter
-- is a pair between a name and a type, with positional information.
pParam :: Raw.Param -> Err Param
pParam (Raw.Param (Raw.Decl t x d)) = case d of
  Raw.DBodyEmpty -> return ((x &), pType t)
  _ -> Bad "Bad parameter definition"

-- Refine a typedef field
pField :: Raw.Field -> Err Field
pField (Raw.Field d@(Raw.Decl _ _ b)) = case b of
  Raw.DBodyEmpty -> Bad "Bad field declaration"
  _ -> do
    Pos _ d' <- pDecl d
    case d' of
      Decl x t e -> return (x, t, e)
      _ -> Bad "Bad field declaration"

-- | Refine a Promela declaration or statement into a more ergonomic representation.
pStep :: Raw.Step -> Err (Pos Stmt)
pStep = \case
  Raw.StepDecl d -> pDecl d
  Raw.StepStmt s -> pStmt s

-- | Refine a Promela declaration into a more ergonomic representation.
pDecl :: Raw.Decl -> Err (Pos Stmt)
pDecl (Raw.Decl t x e) =
  do
    let l = (x ~)
    (t', e') <- pDeclBody (pType t) e
    return (l @ Decl (x &) t' e')

-- | Refine a Promela declaration body into a more ergonomic representation.
pDeclBody :: Type -> Raw.DeclBody -> Err (Type, Maybe Exp)
pDeclBody t b = case (t, b) of
  -- Int variable declarations
  (TInt, Raw.DBodyEmpty) ->
    let v = return (Const (VInt 0))
     in return (t, v)
  (TInt, Raw.DBodyExp _ re) ->
    let Pos _ e = pExp re
     in return (t, return e)
  -- Boolean variable declarations
  (TBool, Raw.DBodyEmpty) ->
    let v = return (Const (VBool False))
     in return (t, v)
  (TBool, Raw.DBodyExp _ re) ->
    let Pos _ e = pExp re
     in return (t, return e)
  -- User-defined typed variable declarations
  (TNamed _, Raw.DBodyEmpty) -> return (t, Nothing)
  -- Channel declarations
  (TChan, Raw.DBodyChan _ k _ _) -> do
    k' <- pCap k
    return (TChan, return (Chan k'))
  _ -> Bad "Bad variable declaration"

-- | Refine a Promela statement into a more ergonomic representation.
pStmt :: Raw.Stmt -> Err (Pos Stmt)
pStmt =
  let desugarOff c v = do
        v' <- pLVal v
        let offset = c (EVar v') (Const (VInt 1))
        return (As v' offset)
   in \case
        -- if :: branches... fi
        Raw.StmtIf (Raw.IF l) os' -> do
          els <- getElse os'
          os <- pOptions os'
          return (l @@ If os els)
        -- do :: branches... od
        Raw.StmtDo (Raw.DO l) os' -> do
          els <- getElse os'
          os <- pOptions os'
          return (l @@ Do os els)
        -- for (range) { ... }
        Raw.StmtFor (Raw.FOR l) r ss -> do
          Pos _ r' <- pRange r
          ss' <- results (map pStep (listSteps ss))
          return (l @@ For r' ss')
        -- goto label
        Raw.StmtGoto (Raw.GOTO l) label -> return (l @@ Goto (label &))
        -- label: skip
        Raw.StmtLabel lbl _ _ -> return ((lbl ~) @ Label (lbl &))
        -- v = e
        Raw.StmtAssign v' _ e' ->
          return
            ( do
                v <- pLVal v'
                e <- pExp e'
                return (As v e)
            )
        -- x++ : Desugared into x = x + 1
        Raw.StmtIncr v _ -> return (desugarOff Plus v)
        -- x-- : Desugared into x = x - 1
        Raw.StmtDecr v _ -> return (desugarOff Minus v)
        -- skip
        Raw.StmtSkip (Raw.SKIP l) -> return (l @@ Skip)
        -- break
        Raw.StmtBreak (Raw.BREAK l) -> return (l @@ Break)
        -- assert e
        Raw.StmtAssert _ e -> return (unaryCons pCond Assert e)
        -- c?e
        Raw.StmtRecv v' _ es' ->
          return
            ( do
                v <- pLVal v'
                es <- results (map pExp es')
                return (Recv v es)
            )
        -- c!e
        Raw.StmtSend v' _ es ->
          return
            ( do
                v <- pLVal v'
                unaryCons results (Send v) (map pExp es)
            )
        -- e
        Raw.StmtExpr c -> return (unaryCons pCond ExpS c)

-- | Refine the statements of a Promela else branch, if present, into a more ergonomic representation.
getElse :: [Raw.Option] -> Err (Maybe [Pos Stmt])
getElse =
  let processElse ss e' = do
          e <- e'
          els <- results (map pStep (listSteps ss))
          case e of
            Just _ -> Bad "Branch list has more than one else."
            Nothing -> return (Just els)
      f = \case
        Raw.OptionSt {} -> id
        Raw.OptionSt2 {} -> id
        Raw.OptionEls _ ss -> processElse ss
        Raw.OptionEls2 _ ss -> processElse ss
   in fmap (fmap reverse) . foldr f (return Nothing)

-- | Refine the branches of a Promela non-determinstic choice into a more ergonomic representation.
pOptions :: [Raw.Option] -> Err [(Pos Stmt, [Pos Stmt])]
pOptions os =
  let f =
        let processBranch s ss = do
              s' <- pStep s
              ss' <- results (map pStep (listSteps ss))
              return (s', ss')
         in \case
              Raw.OptionSt s ss -> processBranch s ss
              Raw.OptionSt2 s ss -> processBranch s ss
              Raw.OptionEls {} -> return (return Skip, [])
              Raw.OptionEls2 {} -> return (return Skip, [])
   in do
        let os'' =
              filter
                ( \case
                    Raw.OptionEls {} -> False
                    Raw.OptionEls2 {} -> False
                    _ -> True
                )
                os
        os' <- Utilities.General.foldMonad f [] (flip (:)) os''
        return $ reverse os'

-- | Refine a Promela range into a more ergonomic representation.
pRange :: Raw.Range -> Err (Pos Range)
pRange = \case
  Raw.RangeExps x' (Raw.COL l) e1 _ e2 ->
    return
      ( do
          _ <- pToken l
          let x = (x' &)
          binaryCons pExp (Between x) e1 e2
      )
  Raw.RangeArrs x1 (Raw.IN l) x2 ->
    return
      ( do
          _ <- pToken l
          binaryCons pIdent In x1 x2
      )

-- | Refine a Promela type into a more ergonomic representation.
pType :: Raw.Type -> Type
pType = \case
  Raw.TypeChan _ -> TChan
  Raw.TypeInt _ -> TInt
  Raw.TypeBool _ -> TBool
  Raw.TypeNamed (Raw.ID (_, x)) -> TNamed x

-- | Refine a Promela conditional expression into a more ergonomic representation.
pCond :: Raw.Cond -> Pos Exp
pCond =
  let bin = binaryCons pCond
      un = unaryCons pCond
   in \case
        -- e1 || e2
        Raw.CondOr e1 _ e2 -> bin Or e1 e2
        -- e1 && e2
        Raw.CondAnd e1 _ e2 -> bin And e1 e2
        -- !e1
        Raw.CondNot _ e -> un Not e
        Raw.CondExp e -> pExp e

-- | Refine a Promela expression into a more ergonomic representation.
pExp :: Raw.Exp -> Pos Exp
pExp =
  let bin = binaryCons pExp
      un = unaryCons pExp
      unVar = unaryCons pLVal
   in \case
        -- e1 == e2
        Raw.ExpEq e1 _ e2 -> bin Eq e1 e2
        -- e1 != e2
        Raw.ExpNe e1 _ e2 -> bin Ne e1 e2
        -- e1 <= e2
        Raw.ExpLe e1 _ e2 -> bin Le e1 e2
        -- e1 >= e2
        Raw.ExpGe e1 _ e2 -> bin Ge e1 e2
        -- e1 < e2
        Raw.ExpLt e1 _ e2 -> bin Lt e1 e2
        -- e1 > e2
        Raw.ExpGt e1 _ e2 -> bin Gt e1 e2
        -- e1 + e2
        Raw.ExpPlus e1 _ e2 -> bin Plus e1 e2
        -- e1 - e2
        Raw.ExpMinus e1 _ e2 -> bin Minus e1 e2
        -- e1 * e2
        Raw.ExpProd e1 _ e2 -> bin Mult e1 e2
        -- e1 / e2
        Raw.ExpDiv e1 _ e2 -> bin Div e1 e2
        -- -e1
        Raw.ExpNeg _ e -> un Neg e
        -- len(v)
        Raw.ExpLen _ v -> unVar Len v
        -- v
        Raw.ExpLVal v -> unVar EVar v
        -- c
        Raw.ExpConst c -> unaryCons pConst Const c
        -- run f (es...)
        Raw.ExpRun (Raw.RUN tok) f' es' -> do
          _ <- pToken tok
          let f = (f' &)
          es <- results (map pExp es')
          return (Run f es)

-- | Refine a Promela l-value into a more ergonomic representation.
pLVal :: Raw.LVal -> Pos LVal
pLVal = \case
  Raw.ValId x -> (x ~) @ Var (x &)
  Raw.ValArr x' e -> do
    x <- pLVal x'
    unaryCons pExp (Arr x) e
  Raw.ValField x' _ f -> do
    x <- pLVal x'
    return (Field x (f &))

-- | Refine a Promela constant into a more ergonomic representation.
pConst :: Raw.Const -> Pos Val
pConst = \case
  Raw.CInt n@(Raw.NUMBER ((l, _), _)) ->
    l @ VInt (n #)
  Raw.CTrue (Raw.TRUE ((l, _), _)) -> l @ VBool True
  Raw.CFalse (Raw.FALSE ((l, _), _)) -> l @ VBool False
  Raw.CFree (Raw.FREEVAR ((l, _), _)) -> l @ Free

-- | Refine a Promela capacity expression into a more ergonomic representation.
pCap :: Raw.Exp -> Err Exp
pCap = \case
  Raw.ExpPlus e1 _ e2 -> binaryCons pCap Plus e1 e2
  Raw.ExpMinus e1 _ e2 -> binaryCons pCap Minus e1 e2
  Raw.ExpProd e1 _ e2 -> binaryCons pCap Mult e1 e2
  Raw.ExpNeg _ e -> unaryCons pCap Neg e
  Raw.ExpConst (Raw.CInt n) -> return $ Const $ VInt $ (n #)
  Raw.ExpLVal (Raw.ValId x) -> return $ EVar $ Var $ (x &)
  _ -> Bad "Invalid capacity expression."
