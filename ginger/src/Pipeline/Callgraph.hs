module Pipeline.Callgraph (getCG) where

import Data.Map

import Promela.Ast

-- | Construct call graph from a Promela model.
getCG :: Spec -> Map String Module
getCG (Spec ms) =
  Prelude.foldl
    ( \cg -> \case
        go@(Proc f _ _) -> insert f go cg
        _ -> cg
    )
    empty
    ms
