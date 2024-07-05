module Pipeline.VIRGoTranslation.Context.Capacity (caps) where

import Data.Map qualified as M

import VIRGo.Ast
import Pipeline.VIRGoTranslation.Exps (parseExp)
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Utilities

{- | Extract capacity expressions from channel definitions in a VIRGo program.
-}
caps :: 𝑆 -> 𝛫
caps = foldStatement M.union processChans

processChans :: 𝛬 -> 𝑆 -> 𝛫
processChans _ = \case
  Def (Chan c e) -> M.singleton c (parseExp e)
  _ -> M.empty
