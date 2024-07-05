module Pipeline.VIRGoTranslation.Exps (parseExp) where

import Backend.Ast qualified as T
import Backend.Utilities
import VIRGo.Ast

-- | Parse VIRGo expression into back-end expression.
-- No explanation is needed. Parity is 1-to-1.
parseExp :: 𝐸 -> T.Exp
parseExp =
  let bin c e1 e2 = c (parseExp e1) (parseExp e2)
   in \case
        BTrue -> (True ?)
        BFalse -> (False ?)
        e1 :& e2 -> bin (T.:&&) e1 e2
        e1 :| e2 -> bin (T.:||) e1 e2
        Not e -> T.Not (parseExp e)
        e1 :== e2 -> bin (T.:==) e1 e2
        e1 :!= e2 -> bin (T.:!=) e1 e2
        e1 :< e2 -> bin (T.:<) e1 e2
        e1 :<= e2 -> bin (T.:<=) e1 e2
        e1 :> e2 -> bin (T.:>) e1 e2
        e1 :>= e2 -> bin (T.:>=) e1 e2
        e1 :+ e2 -> bin (T.:+) e1 e2
        e1 :- e2 -> bin (T.:-) e1 e2
        e1 :/ e2 -> bin (T.:/) e1 e2
        e1 :* e2 -> bin (T.:*) e1 e2
        Const n -> T.ECon (T.CNum n)
        Var x -> T.EVar x
