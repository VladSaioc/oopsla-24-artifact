module Promela.Utilities (getInit, commStmt) where

import Data.List qualified as L
import Promela.Ast
import Utilities.Position

-- | Retrieve statements in the `init` process of a Promela model.
getInit :: Spec -> [Pos Stmt]
getInit (Spec ms) =
  let i =
        L.find
          ( \case
              Init {} -> True
              _ -> False
          )
          ms
   in case i of
        Just (Init ss) -> ss
        _ -> []

-- | Check whether a statement is a communication operation.
commStmt :: Stmt -> Bool
commStmt = \case
  Send {} -> True
  Recv {} -> True
  _ -> False
