module Go.Profiler (profileProgram) where

import Data.List qualified as L
import Data.Monoid

import Go.Ast
import Utilities.Position

-- | Parametricity of a Go program. A program can be parametric wrt.
-- channel operations in loops, WaitGroup operations in loops, WaitGroup add operations,
-- and channel capacities.
data Parametricity = ChLooping | WgLooping | WgAdd | Capacity
  deriving (Read)

instance Show Parametricity where
  show = \case
    ChLooping -> "channel loop parametric"
    WgLooping -> "WaitGroup loop parametric"
    WgAdd -> "add parametric"
    Capacity -> "capacity parametric"

-- | Profile a Go program for parametricity. Traverse AST and check for
-- channel or WaitGroup operations in loops with bounds that contain free variables, or
-- channel capacity declarations and WaitGroup add operations containing free variables.
profileProgram :: Prog -> String
profileProgram p =
  let parametricity = getParametricity p
   in if null parametricity then "Program is not parametric."
      else "Program is " ++ L.intercalate " and " (map show parametricity)

getParametricity :: Prog -> [Parametricity]
getParametricity p =
  let chlooping =  ([ChLooping | Any True == chLoopParametric p])
      wglooping =  ([WgLooping | Any True == wgLoopParametric p])
      wgadd =  ([WgAdd | Any True == addParametric p])
      capping = ([Capacity | Any True == capParametric p])
    in chlooping ++ capping ++ wglooping ++ wgadd

traverseStmt :: Monoid a => Monoid b => (a -> Stmt -> a) -> (a -> Stmt -> b) -> a -> Pos Stmt -> b
traverseStmt makecontext makeresult ctx (Pos p s) =
  let tr = traverseStmt makecontext makeresult
      aggregate ss = mconcat (map (tr (makecontext ctx s)) ss)
      res = makeresult ctx
  in case s of
    Block ss -> aggregate ss
    Go ss -> res s <> aggregate ss
    While _ ss -> res s <> aggregate ss
    For _ _ _ _ ss -> res s <> aggregate ss
    If _ ss1 ss2 -> res s <> aggregate ss1 <> aggregate ss2
    Select cs _ ->
      let caseops = aggregate (map (Pos p . Atomic . (@^) . fst) cs)
          casestmts = mconcat (map (aggregate . snd) cs)
       in res s <> caseops <> casestmts
    _ -> res s

chLoopParametric :: Prog -> Any
chLoopParametric (Prog ss) =
  let makecontext looping = \case
        For _ e1 e2 _ _ -> Any (parametricExp e1) <> Any (parametricExp e2)
        _ -> looping
      makeresult looping = \case
        Atomic (Send _) -> looping
        Atomic (Recv _) -> looping
        _ -> Any False
    in mconcat $ map (traverseStmt makecontext makeresult (Any False)) ss

wgLoopParametric :: Prog -> Any
wgLoopParametric (Prog ss) =
  let makecontext looping = \case
        For _ e1 e2 _ _ -> Any (parametricExp e1) <> Any (parametricExp e2)
        _ -> looping
      makeresult looping = \case
        Add _ _ -> looping
        Wait _ -> looping
        _ -> Any False
    in mconcat $ map (traverseStmt makecontext makeresult (Any False)) ss

capParametric :: Prog -> Any
capParametric (Prog ss) =
  let makeresult _ = \case
        Chan _ e -> Any (parametricExp e)
        _ -> Any False
    in mconcat $ map (traverseStmt mempty makeresult (Any False)) ss

addParametric :: Prog -> Any
addParametric (Prog ss) =
  let makeresult _ = \case
        Add e _ -> Any (parametricExp e)
        _ -> Any False
    in mconcat $ map (traverseStmt mempty makeresult (Any False)) ss

parametricExp :: Exp -> Bool
parametricExp =
  let bin e1 e2 = parametricExp e1 || parametricExp e2
  in \case
    CTrue -> False
    CFalse -> False
    CNum _ -> False
    And e1 e2 -> bin e1 e2
    Or e1 e2 -> bin e1 e2
    Le e1 e2 -> bin e1 e2
    Lt e1 e2 -> bin e1 e2
    Ge e1 e2 -> bin e1 e2
    Gt e1 e2 -> bin e1 e2
    Eq e1 e2 -> bin e1 e2
    Ne e1 e2 -> bin e1 e2
    Not e -> parametricExp e
    Neg e -> parametricExp e
    Plus e1 e2 -> bin e1 e2
    Minus e1 e2 -> bin e1 e2
    Mult e1 e2 -> bin e1 e2
    Div e1 e2 -> bin e1 e2
    Var _ -> True
