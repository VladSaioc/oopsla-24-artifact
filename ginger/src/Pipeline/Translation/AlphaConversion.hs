module Pipeline.Translation.AlphaConversion (alphaConvert) where

import Data.List qualified as L
import Data.Map as M
import Data.Maybe

import Promela.Ast
import Utilities.Position

-- | Environment from original source name to alpha-converted name
type Env = M.Map String String

-- | Alpha conversion context:
-- 1. The bindings from unconverted to converted names
--    in the current scope
-- 2. The next available name index
-- 3. The output of alpha conversion
data Ctxt a = Ctxt Env Int a deriving (Eq, Ord, Read, Show)

-- | Alpha conversion for whole Promela program.
alphaConvert :: Spec -> Spec
alphaConvert (Spec ms) =
  let -- In a first pass perform alpha conversion for top-level declarations.
      ctx' = Prelude.foldl alphaDefs (Ctxt M.empty 0 []) ms
      -- In a second pass, alpha convert the rest of the program.
      Ctxt _ _ ms' = Prelude.foldl alphaModules ctx' ms
   in -- Return alpha converted program
      Spec (reverse ms')

-- | Alpha conversion of a single variable declaration, based on an
-- alpha conversion index.
-- Produces an updated environment and the next available index.
alphaDeclareVar :: (Env, Int) -> Ident -> (Env, Int)
alphaDeclareVar (env, idx) x =
  let -- | Append the index to the variable name and insert into
      -- the variable declaration.
      x' = ['\'' | "_"  `L.isPrefixOf` x] ++ x ++ "'" ++ show idx
   in (M.insert x x' env, idx + 1)

-- | Alpha conversion of top-level declarations, performed in a first pass.
alphaDefs :: Ctxt [Module] -> Module -> Ctxt [Module]
alphaDefs ctx@(Ctxt env idx ms) = \case
  TopDecl x t v ->
    -- Introduce the top-level variable into the bindings set.
    let (env', idx') = alphaDeclareVar (env, idx) x
        -- The name was just introduced, so there is no risk of failure
        x' = Data.Maybe.fromJust (M.lookup x env')
     in -- Add the converted top-level declaration to the module
        Ctxt env' idx' (TopDecl x' t v : ms)
  _ -> ctx

-- | Alpha conversion of top-level modules, performed in a second pass
alphaModules :: Ctxt [Module] -> Module -> Ctxt [Module]
alphaModules ctx@(Ctxt env aenv ms) = \case
  -- Top-level declarations already converted on a first pass
  TopDecl {} -> ctx
  -- Initial process translation
  Init ss ->
    let -- Alpha conversion of process body
        Ctxt _ aenv' ss' = Prelude.foldl alphaStmt (Ctxt env aenv []) ss
     in -- Reconstruct initial process with converted components
        -- and append to list of converted modules.
        -- ! Body reversed here to preserve linearity
        -- ! in the complexity of construction.
        Ctxt env aenv' (Init (reverse ss') : ms)
  -- Procedure definition translation
  Proc f ps ss ->
    let -- Alpha conversion of parameters
        Ctxt env' aenv' ps' = Prelude.foldl alphaParam (Ctxt env aenv []) ps
        -- Alpha conversion of procedure body
        Ctxt _ aenv2 ss' = Prelude.foldl alphaStmt (Ctxt env' aenv' []) ss
     in -- Reconstruct procedure with converted components
        -- and append to list of converted modules.
        -- ! Parameters and body reversed here to preserve linearity
        -- ! in the complexity of construction.
        Ctxt env aenv2 (Proc f (reverse ps') (reverse ss') : ms)
  -- Type definition translation
  Typedef t fs ->
    let -- Convert any expressions in field declarations for custom type.
        fs' = Prelude.map (\(f, ft, me) -> (f, ft, fmap (alphaExp env) me)) fs
     in Ctxt env aenv (Typedef t fs' : ms)

-- Alpha conversion of function parameter
alphaParam :: Ctxt [Param] -> Param -> Ctxt [Param]
alphaParam (Ctxt env aenv ps) (x, t) =
  let -- Introduce parameter in the name environment
      -- and produce next fresh alpha conversion index.
      (env', aenv') = alphaDeclareVar (env, aenv) x
      x' = Data.Maybe.fromJust (M.lookup x env')
   in -- Add parameter to converted parameters
      Ctxt env' aenv' ((x', t) : ps)

-- Alpha conversion of individual Promela statements.
alphaStmt :: Ctxt [Pos Stmt] -> Pos Stmt -> Ctxt [Pos Stmt]
alphaStmt (Ctxt env aenv ss) (Pos p s) =
  let -- Fold over statement sequences.
      alph = Prelude.foldl alphaStmt
      -- Short-hand for conversion of conditional statements
      -- with multiple branches ('if' and 'do').
      branchingPoint c os mels =
        let -- Conversion of individual case branch.
            alphaBranch (Ctxt e a bs) (c0, ss0) =
              let -- Convert case guard
                  Ctxt e' a' c2 = alphaStmt (Ctxt e a []) c0
                  -- Guard should be a single operation
                  c' = case c2 of
                    [c3] -> c3
                    _ -> Pos NoPos Skip
                  -- Convert case body
                  Ctxt e2 a2 ss' = alph (Ctxt e' a' []) ss0
               in -- Reconstruct case branch with converted components
                  -- and append to list of cases.v
                  Ctxt e2 a2 ((c', reverse ss') : bs)
            -- Convert all case branches
            Ctxt env' aenv' os' = Prelude.foldl alphaBranch (Ctxt env aenv []) os
            -- If a default case is present, convert it.
            Ctxt env2 aenv2 mels' = case mels of
              Just els ->
                let Ctxt aenv3 env3 els' = alph (Ctxt env' aenv' []) els
                 in Ctxt aenv3 env3 (Just els')
              Nothing -> Ctxt env' aenv' Nothing
         in -- Reconstruct conditional statement with converted components
            -- and append to list of statements.
            -- ! Cases reversed here to preserve linearity
            -- ! in the complexity of construction.
            Ctxt env2 aenv2 (p @ c (reverse os') mels' : ss)
      chOp c x es =
        let -- Get alpha converted channel name
            x' = alphaLVal env x
            -- Convert channel payload/receiver
            es' = Prelude.map (alphaExp env) es
         in -- Add channel operation (constructed with
            -- constructor 'c') to the list of converted
            -- statements
            Ctxt env aenv (p @ c x' es' : ss)
   in case s of
        -- Channel operations
        Recv c es -> chOp Recv c es
        Send c es -> chOp Send c es
        -- Expression statements
        ExpS e ->
          let e' = alphaExp env e
           in Ctxt env aenv (p @ ExpS e' : ss)
        Assert e ->
          let e' = alphaExp env e
           in Ctxt env aenv (p @ Assert e' : ss)
        -- Conditional statements
        If os mels -> branchingPoint If os mels
        Do os mels -> branchingPoint Do os mels
        -- Variable declarations
        Decl x t e ->
          let -- Convert RHS expression (if one exists)
              e' = fmap (alphaExp env) e
              -- Obtain binding environment and next fresh
              -- index after introducing declared variable
              (env', aenv') = alphaDeclareVar (env, aenv) x
              -- Retrieve freshly introduced converted name
              x' = Data.Maybe.fromJust (M.lookup x env')
              -- Reconstruct converted declaration statement
              s' = Pos p (Decl x' t e')
           in -- Add statement to converted statements
              Ctxt env' aenv' (s' : ss)
        For r body ->
          let -- Convert range statement
              r' = alphaRange env r
              -- Convert for-loop body
              Ctxt _ aenv' body' = alph (Ctxt env aenv []) body
           in -- Reconstruct converted for-loop statement.
              -- ! Statements reversed here to preserve linearity
              -- ! in the complexity of construction.
              Ctxt env aenv' (p @ For r' (reverse body') : ss)
        As x e ->
          let -- Convert assignment LHS
              x' = alphaLVal env x
              -- Convert assignment RHS
              e' = alphaExp env e
           in Ctxt env aenv (p @ As x' e' : ss)
        -- Goto statement do not require alpha conversion
        Goto lbl -> Ctxt env aenv (p @ Goto lbl : ss)
        -- Irregular control flow statements do not require
        -- alpha conversion.
        Break -> Ctxt env aenv (p @ Break : ss)
        Skip -> Ctxt env aenv (p @ Skip : ss)
        Label lbl -> Ctxt env aenv (p @ Label lbl : ss)

-- Alpha conversion of expressions. Performed inductively,
-- given the name environment. Only swaps names with their
-- converted coounterparts in the given scope.
alphaExp :: Env -> Exp -> Exp
alphaExp env =
  let alpha = alphaExp env
      bin c e1 e2 = c (alpha e1) (alpha e2)
   in \case
        Chan e -> Chan (alpha e)
        Const v -> Const v
        And e1 e2 -> bin And e1 e2
        Or e1 e2 -> bin Or e1 e2
        Eq e1 e2 -> bin Eq e1 e2
        Ne e1 e2 -> bin Ne e1 e2
        Le e1 e2 -> bin Le e1 e2
        Lt e1 e2 -> bin Lt e1 e2
        Ge e1 e2 -> bin Ge e1 e2
        Gt e1 e2 -> bin Gt e1 e2
        Plus e1 e2 -> bin Plus e1 e2
        Minus e1 e2 -> bin Minus e1 e2
        Mult e1 e2 -> bin Mult e1 e2
        Div e1 e2 -> bin Div e1 e2
        Neg e -> Neg (alpha e)
        Not e -> Not (alpha e)
        Len x -> Len (alphaLVal env x)
        EVar x -> EVar (alphaLVal env x)
        Run f es -> Run f (Prelude.map alpha es)

-- Alpha conversion of L-value selectors
alphaLVal :: Env -> LVal -> LVal
alphaLVal env = \case
  -- Simple variables
  Var x ->
    -- The variable is guaranteed to have been
    -- declared in an earlier statement, or passed
    -- as a parameter.
    -- Its absence from the name environment
    -- warrants a fatal exception.
    Var (case M.lookup x env of
      Just x' -> x'
      Nothing -> error $ unwords ["Variable", x, "missing during alpha conversion."])
  -- Field names remain unchanged, but object names
  -- are recursively alpha converted.
  Field x f -> Field (alphaLVal env x) f
  -- Both array L-value and expression must be alpha converted
  Arr x e -> Arr (alphaLVal env x) (alphaExp env e)

-- Alpha conversion of range clause in for loop
alphaRange :: Env -> Range -> Range
alphaRange env = \case
  Between x e1 e2 ->
    let -- The loop index variable is guaranteed to have
        -- been declared in an earlier statement.
        -- Its absence warrants a fatal exception.
        x' = Data.Maybe.fromJust (M.lookup x env)
     in Between x' (alphaExp env e1) (alphaExp env e2)
  In x1 x2 ->
    let -- The loop index and collection variables are guaranteed
        -- to have been declared in an earlier statement.
        -- The absence of either warrants a fatal exception.
        x1' = Data.Maybe.fromJust (M.lookup x1 env)
        x2' = Data.Maybe.fromJust (M.lookup x2 env)
     in In x1' x2'
