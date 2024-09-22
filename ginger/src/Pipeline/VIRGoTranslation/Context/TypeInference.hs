module Pipeline.VIRGoTranslation.Context.TypeInference (typesAndFvs) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.UnionFind.IntMap

import Backend.Ast qualified as T
import Backend.Utilities
import VIRGo.Ast
import Utilities.Err
import Utilities.TransformationCtx

-- | Context for unification-based type inferrence of the VIRGo program.
data Ctx a b = Ctx
  { -- | Information the context (e.g. VIRGo syntax).
    datum :: a,
    -- | Next fresh index
    next :: Int,
    -- | Variable environment, from variable name to inferred construct
    venv :: M.Map String b,
    -- | Point supply for unification
    supply :: PointSupply b,
    -- | Environment from inferred construct to equivalence classes
    tenv :: M.Map b (Point b)
  }

-- | Partial implementation is safe because no access
-- of object is necessary for type inference.
instance TransformCtx Ctx where
  source = datum
  updateSource ctx a = ctx {datum = a}

type TCtx a = Ctx a T.Type

nextTypeVar :: Int -> T.Type
nextTypeVar n = T.TVar ("T'" ++ show n)

-- | Create a fresh context containing points for primitive types.
mkTCtx :: TCtx ()
mkTCtx =
  let addPrimitiveTypes =
        let add (te0, ps0) t =
              let (ps1, p) = fresh ps0 t
                  te1 = M.insert t p te0
               in (te1, ps1)
         in foldl add (M.empty, newPointSupply)
      (te, ps) = addPrimitiveTypes [T.TInt, T.TBool]
   in Ctx
        { datum = (),
          next = 0,
          venv = M.empty,
          supply = ps,
          tenv = te
        }

{- | Order type points, such that the "dominant" type ends in the second position.
Will return an error if the two types cannot be unified.

Currently only supports boolean, integer, and named type unification.
-}
orderPoints :: PointSupply T.Type -> Point T.Type -> Point T.Type -> Err (Point T.Type, Point T.Type)
orderPoints ps p1 p2 =
  let (t1, t2) = (descriptor ps p1, descriptor ps p2)
   in case (t1, t2) of
        -- Inference error in any member resolves to bad
        (T.TInt, T.TVar {}) -> return (p2, p1)
        (T.TVar {}, T.TInt) -> return (p1, p2)
        (T.TBool, T.TVar {}) -> return (p2, p1)
        (T.TVar {}, T.TBool) -> return (p1, p2)
        (T.TVar x, T.TVar y) -> return $ if x < y then (p2, p1) else (p1, p2)
        _ ->
          if t1 == t2
            then return (p1, p2)
            else Bad $ unwords ["Type unification error:", show t1, "⊔", show t2]

{- | Injects the proper descriptor when unifying two types.
It favors builtin types over their named counterparts.

Currently only supports boolean, integer, and named type unification.
-}
unifyTypes :: TCtx a -> T.Type -> T.Type -> Err (TCtx T.Type)
unifyTypes ctx@(Ctx {supply = ps, tenv = te}) t1 t2 = do
  -- Pointers should have been added previously
  let p1 = fromJust (M.lookup t1 te)
      p2 = fromJust (M.lookup t2 te)
  (p1', p2') <- orderPoints ps p1 p2
  let t = descriptor ps p2'
  let ps' = union ps p1' p2'
  let ctx' = ctx {supply = ps'}
  return $ t >: ctx'

makeTypeEnvironment :: TCtx a -> M.Map String T.Type
makeTypeEnvironment Ctx {supply, tenv, venv} =
  let pairVarAndType t =
        let p = fromJust $ M.lookup t tenv
         in descriptor supply p
   in M.map pairVarAndType venv

unboundTypes :: TCtx a -> [T.Type]
unboundTypes Ctx {supply, tenv, venv} =
  let getUnboundType t =
        let p = fromJust $ M.lookup t tenv
         in case descriptor supply p of
              t'@(T.TVar x) ->
                if "T'" `L.isPrefixOf` x
                  then Just t'
                  else Nothing
              _ -> Nothing
   in L.nub $ mapMaybe getUnboundType (M.elems venv)

typesAndFvs :: 𝑆 -> Err (𝛤, [T.Type])
typesAndFvs s = do
  ctx' <- stmtFVs $ s >: mkTCtx
  let vs = makeTypeEnvironment ctx'
  let ts = unboundTypes ctx'
  return (vs, ts)

-- | Collect and unify types in channel capacity expressions.
chanFVs :: TCtx 𝐷 -> Err (TCtx ())
chanFVs ctx = case datum ctx of
  Chan _ e -> do
    ctx'@(Ctx {datum = t}) <- expFVs $ e >: ctx
    ctx'' <- unifyTypes ctx' T.TInt t
    done ctx''
  _ -> done ctx

-- | Collect and unify types across VIRGo statements.
stmtFVs :: TCtx 𝑆 -> Err (TCtx ())
stmtFVs ctx@(Ctx {datum = s}) =
  let updateWithExp t ctx' e = do
        ctx1@(Ctx {datum = t1}) <- expFVs $ e >: ctx'
        ctx2 <- unifyTypes ctx1 t t1
        done ctx2
   in case s of
        Def d -> chanFVs (d >: ctx)
        Skip -> done ctx
        Return -> done ctx
        Close _ -> done ctx
        Atomic (Add _ e) -> updateWithExp T.TInt ctx e
        Atomic _ -> done ctx
        Seq s1 s2 -> do
          ctx' <- stmtFVs $ s1 >: ctx
          stmtFVs $ s2 >: ctx'
        For _ e1 e2 _ -> do
          ctx1 <- updateWithExp T.TInt ctx e1
          updateWithExp T.TInt ctx1 e2
        If e s1 s2 -> do
          ctx1 <- updateWithExp T.TBool ctx e
          ctx2 <- stmtFVs $ s1 >: ctx1
          stmtFVs $ s2 >: ctx2
        Go s1 -> stmtFVs $ s1 >: ctx

-- | Extract and infer the types of free variables in expressions.
expFVs :: TCtx 𝐸 -> Err (TCtx T.Type)
expFVs ctx@(Ctx {datum = e, supply}) =
  let -- Short-hand for unifying the operands of binary operators.
      -- It accepts a possible baseline type, a result type, and
      -- two sub-expressions.
      bin mt t e1 e2 = do
        -- Statefully retrieve the type of left-hand side expression
        ctx1@(Ctx {datum = t1}) <- expFVs $ e1 >: ctx
        -- Statefully retrieve the type of right-hand side expression
        ctx2@(Ctx {datum = t2}) <- expFVs $ e2 >: ctx1
        -- If a baseline type exists, unify the left-hand side with it
        ctx3 <- case mt of
          Just t' -> unifyTypes ctx2 t' t1
          Nothing -> return ctx2
        -- Unify sub-expression types
        ctx4 <- unifyTypes ctx3 t1 t2
        -- Decorate context with result type
        return $ t >: ctx4
      -- Arithmetic unification uses the result type as a
      -- baseline type.
      arith t = bin (Just t) t
      -- Comparison does not have a baseline type, because it
      -- is polymorphic e.g., for expression e1 == e2, operands
      -- e1 and e2 must be of the same type, but that may be
      -- any comparable type.
      comp = bin Nothing
   in case e of
        -- Numeric constants produce the integer type
        Const {} -> return $ T.TInt >: ctx
        -- Boolean constants produce the boolean type
        BTrue {} -> return $ T.TBool >: ctx
        BFalse {} -> return $ T.TBool >: ctx
        -- Variables either look for an existing type,
        -- or create a fresh type variable, if the
        -- variable is new. Also update the variable and
        -- type environments.
        Var x -> do
          let env = venv ctx
              n = next ctx
              ctx' = case M.lookup x env of
                Nothing ->
                  let t = nextTypeVar n
                      (supply', p) = fresh supply t
                   in t
                        >: ctx
                          { next = n + 1,
                            venv = M.insert x t env,
                            tenv = M.insert t p (tenv ctx),
                            supply = supply'
                          }
                Just t -> t >: ctx
          return ctx'
        -- Boolean arithmetic produces a boolean type.
        -- Unify operand types with bool.
        Not e1 -> arith T.TBool BTrue e1
        e1 :& e2 -> arith T.TBool e1 e2
        e1 :| e2 -> arith T.TBool e1 e2
        -- Comparisons produce a boolean type.
        -- Unify operand types with each other.
        -- Comparison may be polymorphic.
        e1 :== e2 -> comp T.TBool e1 e2
        e1 :!= e2 -> comp T.TBool e1 e2
        e1 :< e2 -> comp T.TBool e1 e2
        e1 :<= e2 -> comp T.TBool e1 e2
        e1 :> e2 -> comp T.TBool e1 e2
        e1 :>= e2 -> comp T.TBool e1 e2
        -- Numeric arithmetic produces the int type.
        -- Unify operand types with int.
        e1 :+ e2 -> arith T.TInt e1 e2
        e1 :- e2 -> arith T.TInt e1 e2
        e1 :* e2 -> arith T.TInt e1 e2
        e1 :/ e2 -> arith T.TInt e1 e2
